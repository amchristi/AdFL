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

import java.awt.*;
import java.awt.event.*;
import java.lang.ref.WeakReference;
import javax.activation.DataContentHandler;
import javax.activation.DataContentHandlerFactory;
import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;
import javax.swing.text.AbstractDocument;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.DocumentFilter;
import org.netbeans.api.annotations.common.StaticResource;
import org.openide.util.ImageUtilities;
import org.openide.util.RequestProcessor;
import java.io.*;

/**
 * Quick search infrastructure for an arbitrary component.
 * When quick search is attached to a component, it listens on key events going
 * to the component and displays a quick search field.
 *
 * @author Martin Entlicher
 * @since 7.43
 */
public class QuickSearch {

    @StaticResource
    private static final String // NOI18N
            ICON_FIND = "org/openide/awt/resources/quicksearch/find.png";

    @StaticResource
    private static final String // NOI18N
            ICON_FIND_WITH_MENU = "org/openide/awt/resources/quicksearch/findMenu.png";

    private static final Object CLIENT_PROPERTY_KEY = new Object();

    private final JComponent component;

    private final Object constraints;

    private final Callback callback;

    private final JMenu popupMenu;

    private final boolean asynchronous;

    private boolean enabled = true;

    private SearchTextField searchTextField;

    private KeyAdapter quickSearchKeyAdapter;

    private SearchFieldListener searchFieldListener;

    private JPanel searchPanel;

    private final RequestProcessor rp;

    private static enum QS_FIRE {

        UPDATE, NEXT, MAX
    }

    private AnimationTimer animationTimer;

    private boolean alwaysShown = false;

    private volatile boolean hasSearchText = false;

    private QuickSearch(JComponent component, Object constraints, Callback callback, boolean asynchronous, JMenu popupMenu) {
        this.component = component;
        this.constraints = constraints;
        this.callback = callback;
        this.asynchronous = asynchronous;
        this.popupMenu = popupMenu;
        if (asynchronous) {
            rp = new RequestProcessor(QuickSearch.class);
        } else {
            rp = null;
        }
        setUpSearch();
    }

    /**
     * Attach quick search to a component with given constraints.
     * It listens on key events going to the component and displays a quick search
     * field.
     *
     * @param component The component to attach to
     * @param constraints The constraints that are used to add the search field
     * to the component. It's passed to {@link JComponent#add(java.awt.Component, java.lang.Object)}
     * when adding the quick search UI to the component.
     * @param callback The call back implementation, which is notified from the
     * quick search field submissions.
     * @return An instance of QuickSearch class.
     */
    public static QuickSearch attach(JComponent component, Object constraints, Callback callback) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "71f64975-3cba-432b-9f50-89c26c45210b");
        return attach(component, constraints, callback, false, null);
    }

    /**
     * Attach quick search to a component with given constraints.
     * It listens on key events going to the component and displays a quick search
     * field.
     *
     * @param component The component to attach to
     * @param constraints The constraints that are used to add the search field
     * to the component. It's passed to {@link JComponent#add(java.awt.Component, java.lang.Object)}
     * when adding the quick search UI to the component.
     * @param callback The call back implementation, which is notified from the
     * quick search field submissions.
     * @return An instance of QuickSearch class.
     */
    public static QuickSearch attach(JComponent component, Object constraints, Callback callback, boolean asynchronous) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e31b03e4-5574-426d-a9e1-9081ab197e25");
        return attach(component, constraints, callback, asynchronous, null);
    }

    /**
     * Attach quick search to a component with given constraints.
     * It listens on key events going to the component and displays a quick search
     * field.
     *
     * @param component The component to attach to
     * @param constraints The constraints that are used to add the search field
     * to the component. It's passed to {@link JComponent#add(java.awt.Component, java.lang.Object)}
     * when adding the quick search UI to the component.
     * @param callback The call back implementation, which is notified from the
     * quick search field submissions.
     * @param popupMenu A pop-up menu, that is displayed on the find icon, next to the search
     * field. This allows customization of the search criteria. The pop-up menu
     * is taken from {@link JMenu#getPopupMenu()}.
     * @return An instance of QuickSearch class.
     */
    public static QuickSearch attach(JComponent component, Object constraints, Callback callback, JMenu popupMenu) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "62410a4c-5dfc-448f-9720-5c9c4838ef8f");
        return attach(component, constraints, callback, false, popupMenu);
    }

    /**
     * Attach quick search to a component with given constraints.
     * It listens on key events going to the component and displays a quick search
     * field.
     *
     * @param component The component to attach to
     * @param constraints The constraints that are used to add the search field
     * to the component. It's passed to {@link JComponent#add(java.awt.Component, java.lang.Object)}
     * when adding the quick search UI to the component.
     * @param callback The call back implementation, which is notified from the
     * quick search field submissions.
     * @param asynchronous Set whether the quick search notifies the call back
     * asynchronously, or not.
     * By default, Callback is notified synchronously on EQ thread.
     * If <code>true</code>, three notification methods are called asynchronously
     * on a background thread. These are
     * {@link Callback#quickSearchUpdate(java.lang.String)},
     * {@link Callback#showNextSelection(javax.swing.text.Position.Bias)},
     * {@link Callback#findMaxPrefix(java.lang.String)}.
     * If <code>false</code> all methods are called synchronously on EQ thread.
     * @param popupMenu A pop-up menu, that is displayed on the find icon, next to the search
     * field. This allows customization of the search criteria. The pop-up menu
     * is taken from {@link JMenu#getPopupMenu()}.
     * @return An instance of QuickSearch class.
     */
    public static QuickSearch attach(JComponent component, Object constraints, Callback callback, boolean asynchronous, JMenu popupMenu) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a6211d44-499d-424e-b9ab-d75a4b040e5b");
        Object qso = component.getClientProperty(CLIENT_PROPERTY_KEY);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b6fcaaa3-47d9-4007-b96a-b2f478a21cc3");
        if (qso instanceof QuickSearch) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cbb16172-f093-46ed-899a-380d9e93c25f");
            // NOI18N
            throw new IllegalStateException("A quick search is attached to this component already, detach it first.");
        } else {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d5ffecb1-0369-4f68-885d-354cd8f72b8a");
            QuickSearch qs = new QuickSearch(component, constraints, callback, asynchronous, popupMenu);
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "de4b9dc7-f204-407c-9e94-ea276c7bf776");
            component.putClientProperty(CLIENT_PROPERTY_KEY, qs);
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b77f87e0-9f47-4ee6-94e2-f46d46bbb722");
            return qs;
        }
    }

    /**
     * Detach the quick search from the component it was attached to.
     */
    public void detach() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "054478ee-7394-4a87-84a6-f88d9dee0368");
        setEnabled(false);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e081b998-79ed-41e2-9190-65ffe28f90da");
        component.putClientProperty(CLIENT_PROPERTY_KEY, null);
    }

    /**
     * Test whether the quick search field is always shown.
     * This is <code>false</code> by default.
     * @return <code>true</code> when the search field is always shown,
     * <code>false</code> otherwise.
     * @since 7.49
     */
    public boolean isAlwaysShown() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fb20c762-8b28-40e3-81b9-b3cf5d33c517");
        return alwaysShown;
    }

    /**
     * Set whether the quick search field should always be shown.
     * @param alwaysShown <code>true</code> to always show the search field,
     * <code>false</code> otherwise.
     * @since 7.49
     */
    public void setAlwaysShown(boolean alwaysShown) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "234412ae-ff0d-4913-bf5f-ee4fdcb816db");
        this.alwaysShown = alwaysShown;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ebcfa82a-8bae-4e15-8453-32313fbfe119");
        if (alwaysShown) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "14b47215-75c5-4b35-9c47-82464bf074fa");
            displaySearchField();
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "865d48a7-9db1-4146-9793-f5c56df98a0a");
            removeSearchField();
        }
    }

    /**
     * Test whether the quick search is enabled. This is <code>true</code>
     * by default.
     * @return <code>true</code> when the quick search is enabled,
     * <code>false</code> otherwise.
     */
    public boolean isEnabled() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "86310e14-2e2e-447c-8e4e-941951cb4203");
        return enabled;
    }

    /**
     * Set the enabled state of the quick search.
     * This allows to activate/deactivate the quick search functionality.
     * @param enabled <code>true</code> to enable the quick search,
     * <code>false</code> otherwise.
     */
    public void setEnabled(boolean enabled) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8ccef2c3-79e3-405f-8f89-22befdbe7c7a");
        if (this.enabled == enabled) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0a84049c-f179-4928-ae44-9bc1dde4a310");
            return;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2aaca8d0-5ac5-41af-b253-09cdfe76b3b0");
        this.enabled = enabled;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "975ae428-d00d-4483-bd02-b32280bd434d");
        if (enabled) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e478ad77-0be7-4820-af48-161821145242");
            component.addKeyListener(quickSearchKeyAdapter);
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3ee2aa39-86cf-483b-a20b-482ae23d10e9");
            removeSearchField();
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "afaaf91e-30f4-46ec-a6da-b0ddef982db8");
            component.removeKeyListener(quickSearchKeyAdapter);
        }
    }

    /**
     * Process this key event in addition to the key events obtained from the
     * component we're attached to.
     * @param ke a key event to process.
     */
    public void processKeyEvent(KeyEvent ke) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "da7b4125-cb4f-44c8-8c1a-89ab63f14722");
        if (!isEnabled()) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7c332d17-4caa-4e1b-b84d-5a90d2685d56");
            return;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "21d69d84-62bd-4bc1-aa86-0640e7889563");
        if (searchPanel != null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fb7d9ba0-b222-4f9a-9431-d85e9ca3f504");
            if (!isKeyEventInSearchFieldIgnored(ke)) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "11a58dd0-6d5a-4fa2-952a-b478d33a963f");
                searchTextField.setCaretPosition(searchTextField.getText().length());
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0485e008-8a12-40df-a0a2-1f08bb959637");
                searchTextField.processKeyEvent(ke);
            }
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c0946287-aaa0-4eef-b5f5-347b68543c14");
            switch(ke.getID()) {
                case KeyEvent.KEY_PRESSED:
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c515985f-ed8c-4880-a6f3-8ea8f43914b8");
                    quickSearchKeyAdapter.keyPressed(ke);
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "628a9793-0501-4fad-a1f1-1746fc80812a");
                    break;
                case KeyEvent.KEY_RELEASED:
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "658e3d7f-055c-46aa-a5f2-7b894a0a1641");
                    quickSearchKeyAdapter.keyReleased(ke);
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "99d8efce-342c-4473-a357-15ff9434fcee");
                    break;
                case KeyEvent.KEY_TYPED:
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "06d7b3cf-6b58-4473-8ece-22816e4fc9f7");
                    quickSearchKeyAdapter.keyTyped(ke);
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c1736fb6-caf3-4dc5-9099-c96cfb163f67");
                    break;
            }
        }
    }

    private boolean isKeyEventInSearchFieldIgnored(KeyEvent ke) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9fae3b2c-0846-4698-a234-7b7e5ce6e81f");
        // Ignore DELETE key events unless the search field has focus
        if (ke.getKeyCode() == KeyEvent.VK_DELETE) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "45b25dce-bd8a-430d-8d96-1425fba713f2");
            return !searchTextField.isFocusOwner();
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8303183b-1b4c-4152-a471-23482e325fa5");
            return false;
        }
    }

    private void fireQuickSearchUpdate(String searchText) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "14506305-93be-4f85-aae4-6ab542d4dc15");
        if (asynchronous) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "14b183df-1283-4f50-87d9-35af6eec27a4");
            rp.post(new LazyFire(QS_FIRE.UPDATE, searchText));
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fe467767-4ffc-41be-86da-282a88363aba");
            callback.quickSearchUpdate(searchText);
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "35d43e50-09fb-48c0-afc4-3e7c4b445d5f");
        hasSearchText = true;
    }

    private void fireShowNextSelection(boolean forward) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1d068ecf-f239-473c-a74f-86251ad7acc4");
        if (asynchronous) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "017a5b77-c11c-4c3c-8ae4-200bdb7d1437");
            rp.post(new LazyFire(QS_FIRE.NEXT, forward));
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2956755e-74c5-4212-9c69-6817394c578e");
            callback.showNextSelection(forward);
        }
    }

    private void findMaxPrefix(String prefix, DataContentHandlerFactory newPrefixSetter) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "90e5f053-5151-4b12-9110-9bf8127585c0");
        if (asynchronous) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "094d368b-107c-40d6-ba12-aecbfea5d4ff");
            rp.post(new LazyFire(QS_FIRE.MAX, prefix, newPrefixSetter));
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "27a6d282-d604-4b8a-92a5-e5f119f58c76");
            prefix = callback.findMaxPrefix(prefix);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1bdcd27e-9781-46e2-870c-76e9e759d666");
            newPrefixSetter.createDataContentHandler(prefix);
        }
    }

    private void setUpSearch() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "42ab4a09-0a03-48e2-90b9-53f5453b4794");
        searchTextField = new SearchTextField();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9e33be6f-7b26-47e7-8341-605f2e11617a");
        // create new key listeners
        quickSearchKeyAdapter = (new KeyAdapter() {

            @Override
            public void keyTyped(KeyEvent e) {
                int modifiers = e.getModifiers();
                int keyCode = e.getKeyCode();
                char c = e.getKeyChar();
                // NOI18N
                if ((c == '+') || (c == '-') || (c == ' '))
                    return;
                if (((modifiers > 0) && (modifiers != KeyEvent.SHIFT_MASK)) || e.isActionKey()) {
                    return;
                }
                if (Character.isISOControl(c) || (keyCode == KeyEvent.VK_SHIFT) || (keyCode == KeyEvent.VK_ESCAPE))
                    return;
                displaySearchField();
                final KeyStroke stroke = KeyStroke.getKeyStrokeForEvent(e);
                searchTextField.setText(String.valueOf(stroke.getKeyChar()));
                e.consume();
            }
        });
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3e5cca03-7f75-4c0f-be16-7cc67aba3f44");
        if (isEnabled()) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "28bf52b6-30f9-4549-bb00-c2038c2dc849");
            component.addKeyListener(quickSearchKeyAdapter);
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "884c58fa-a026-441e-8d0a-551bdfb7228a");
        // Create a the "multi-event" listener for the text field. Instead of
        // adding separate instances of each needed listener, we're using a
        // class which implements them all. This approach is used in order
        // to avoid the creation of 4 instances which takes some time
        searchFieldListener = new SearchFieldListener();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ea352660-fbd0-4fb7-9416-8a7e8c6a4ae1");
        searchTextField.addKeyListener(searchFieldListener);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cc2bd1e0-7ca3-40d1-a54f-0159c4ea0385");
        searchTextField.addFocusListener(searchFieldListener);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "87b42d23-14bb-4632-a578-1cc21f27734b");
        Document searchDoc = searchTextField.getDocument();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f54425c0-9c5f-4acd-a70e-6dab2b2cd385");
        searchDoc.addDocumentListener(searchFieldListener);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "01080f11-2f3e-4628-955c-1e61159c76fd");
        if (searchDoc instanceof AbstractDocument) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a4e42481-2d9a-4b30-856c-0aee526644eb");
            ((AbstractDocument) searchDoc).setDocumentFilter(searchFieldListener.new ReplaceFilter());
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d0e8b7f7-839a-4b5f-ad75-15663e5a59e9");
        if (isAlwaysShown()) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5fcfad26-a9f6-430e-90fc-9c6d5bee03d9");
            displaySearchField();
        }
    }

    private void displaySearchField() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "02526007-c558-4e42-a79b-3eba5971bd07");
        if (searchPanel != null || !isEnabled()) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e6b40d9b-fa93-4bee-93c1-9fe04960061d");
            return;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "51158fdf-7435-4b5f-af46-7fd087354170");
        searchTextField.setOriginalFocusOwner();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8c2c4155-d6e4-48d2-975f-49fe32b84876");
        searchTextField.setFont(component.getFont());
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "59f023dc-ad12-46ea-a764-bc247da493e5");
        searchPanel = new SearchPanel(component, isAlwaysShown());
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ba386c99-27ae-4ba7-a219-3875de485cfb");
        final JLabel lbl;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "913cccb8-3412-419b-b7b1-f548afe3c029");
        if (popupMenu != null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3d3a9978-a844-458b-978a-9ff285da439e");
            lbl = new JLabel(org.openide.util.ImageUtilities.loadImageIcon(ICON_FIND_WITH_MENU, false));
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "107f83a6-995c-4b00-b97b-0db89175420f");
            lbl.addMouseListener(new MouseAdapter() {

                @Override
                public void mousePressed(MouseEvent e) {
                    maybeShowPopup(e, lbl);
                }
            });
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a75f305e-bb02-462c-a55d-51fabece1431");
            lbl = new JLabel(org.openide.util.ImageUtilities.loadImageIcon(ICON_FIND, false));
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d73f3489-d06d-49b2-a99b-e228cc2c6772");
        if (asynchronous) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "91083f23-53ad-4286-9284-28401c7e00eb");
            animationTimer = new AnimationTimer(lbl, lbl.getIcon());
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bcb1df8e-2783-4ff3-aea0-e3c906aa3973");
            animationTimer = null;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "673151e0-e5e2-48b0-a738-014b1f2a5398");
        searchPanel.setLayout(new BoxLayout(searchPanel, BoxLayout.X_AXIS));
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "729baf3c-aee6-4ff4-b71d-8d6885a3b397");
        if ("Aqua".equals(UIManager.getLookAndFeel().getID())) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6b048adc-bddf-4d7e-875c-50a2975a7dad");
            // NOI18N
            if (popupMenu != null) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "58951a62-1848-4488-ae5e-dade83de3c03");
                final JPopupMenu dummy = new JPopupMenu();
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fb749d10-89f7-4075-8ab4-d93c39cd497c");
                dummy.addPopupMenuListener(new PopupMenuListener() {

                    @Override
                    public void popupMenuWillBecomeVisible(PopupMenuEvent e) {
                        SwingUtilities.invokeLater(new Runnable() {

                            @Override
                            public void run() {
                                dummy.setVisible(false);
                            }
                        });
                    }

                    @Override
                    public void popupMenuWillBecomeInvisible(PopupMenuEvent e) {
                    }

                    @Override
                    public void popupMenuCanceled(PopupMenuEvent e) {
                    }
                });
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "077a9a48-7857-44ae-8e13-e36929ab5bec");
                // NOI18N
                searchTextField.putClientProperty("JTextField.Search.FindPopup", dummy);
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3eb14631-61f9-44f1-8f39-cdff7b2dd44e");
                searchTextField.putClientProperty("JTextField.Search.FindAction", new // NOI18N
                        ActionListener() {

                            @Override
                            public void actionPerformed(ActionEvent e) {
                                maybeShowPopup(null, searchTextField);
                            }
                        });
            }
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1ea7d583-0c58-4963-9707-3cf465f4f56c");
            searchPanel.add(lbl);
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "aa2523a7-edeb-4baa-a46b-6cb836b75797");
        searchPanel.add(searchTextField);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fd62bf9b-d584-4383-bddc-092be574ece6");
        searchPanel.setBackground(component.getBackground());
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "51a6225c-c961-4253-8985-2e5aec929767");
        lbl.setLabelFor(searchTextField);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "761ccb29-4655-4905-8e05-8e9207032060");
        searchTextField.setColumns(10);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "601d8e4d-7cae-4094-b746-07badf92394e");
        searchTextField.setMaximumSize(searchTextField.getPreferredSize());
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1624f82c-8223-4758-ad75-1ea26adfc9b1");
        // NOI18N
        searchTextField.putClientProperty("JTextField.variant", "search");
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d52bfe77-9469-48aa-b968-b2b7a94fd48d");
        lbl.setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 5));
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1c47ce82-0e2f-4c83-b242-89dd1467aca2");
        if (constraints == null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e30caf1c-25d5-4e31-9252-2a1f380d8716");
            component.add(searchPanel);
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2a13b519-9294-4719-aa55-e2d318c0a11a");
            component.add(searchPanel, constraints);
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fab6ab95-7207-4e64-98ca-26dcabe21f15");
        component.invalidate();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "68e9a817-022b-45d7-94df-17d1dbcb3af1");
        component.revalidate();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8eac8475-3e0b-4170-9969-4ffc6e2dee28");
        component.repaint();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "de7c4ab9-852f-41b7-9871-66748d861ae8");
        searchTextField.requestFocus();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "88d82634-42f4-4897-88e3-807dd4840494");
        // Select an existing text for an easy rewrite
        searchTextField.selectAll();
    }

    protected void maybeShowPopup(MouseEvent evt, Component comp) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "22c70bd5-45d0-4fb3-9946-5e44ae470482");
        if (evt != null && !SwingUtilities.isLeftMouseButton(evt)) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fb0b317c-0273-40ba-adce-9baa2b4f2887");
            return;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0b7e962f-e34d-413d-85f8-eefc61e8f3ba");
        JPopupMenu pm = popupMenu.getPopupMenu();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "99b63247-d1d8-408a-9be2-1aaf6b4db51b");
        pm.show(comp, 0, comp.getHeight() - 1);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e8568cb5-02f8-44af-9bd8-a932b657b85b");
        searchTextField.setText("");
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e20900d0-0ed9-486a-a594-2a3716c8832d");
        searchTextField.requestOriginalFocusOwner();
    }

    private void removeSearchField() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "58a320b2-642b-41c3-8974-c899f2a644da");
        if (isAlwaysShown()) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b299d13d-e9aa-450c-9d3f-f7272066e94b");
            return;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "446fc06e-527b-4a4b-94af-f75d6e19721a");
        if (searchPanel == null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3c92ccac-3c9f-4abd-979d-821c317541fb");
            return;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "199d017b-c445-420a-b3de-bdbaace6f52c");
        if (animationTimer != null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c0cc8b6d-8087-4c8b-b4e9-bfc4d2a06efe");
            animationTimer.stopProgressAnimation();
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "098908ef-92d5-476b-92f3-8e3e02e9fd11");
        Component sp = searchPanel;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5dc14a0a-734a-450b-831d-e6c3fcb357b6");
        searchPanel = null;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cf7213b8-343b-4f78-9ea0-10b3ea08737e");
        component.remove(sp);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "78a9bb0a-5f0f-4e90-ac5e-06467e4641b4");
        component.invalidate();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "998303df-e7c8-4565-be09-88470aef0bf9");
        component.revalidate();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8200d4a1-600d-49bb-a049-470341d04907");
        component.repaint();
    }

    /**
     * Accessed from test.
     */
    JTextField getSearchField() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c4b6b9b8-7b3f-4cef-9d50-da0b82fd12ea");
        return searchTextField;
    }

    /**
     * Utility method, that finds a greatest common prefix of two supplied
     * strings.
     *
     * @param str1 The first string
     * @param str2 The second string
     * @param ignoreCase Whether to ignore case in the comparisons
     * @return The greatest common prefix of the two strings.
     */
    public static String findMaxPrefix(String str1, String str2, boolean ignoreCase) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4653534c-17ae-40b8-be46-f14c7b45968c");
        int n1 = str1.length();
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f1be0975-6876-40e3-a588-b23d161ef89c");
        int n2 = str2.length();
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b3b7e97a-08db-478d-af8d-dbb4bae7426b");
        int i = 0;
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d73c48d6-f228-49ff-b7ed-b5d8d323d6be");
        if (ignoreCase) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "11221b90-2098-4296-9443-1d6442c272e6");
            for (; i < n1 && i < n2; i++) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "83315c63-af51-4fc2-8cee-46ec9606651a");
                char c1 = Character.toUpperCase(str1.charAt(i));
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cbf1bb3a-3379-45b2-8282-0d7f405c6549");
                char c2 = Character.toUpperCase(str2.charAt(i));
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6089e627-270c-473a-bc26-96cf748aef5a");
                if (c1 != c2) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9dae47c3-b232-49e8-889c-e58996672222");
                    break;
                }
            }
        } else {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a1fb5035-331c-4c80-9cf6-037f264b4dca");
            for (; i < n1 && i < n2; i++) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "564115c7-dd40-4097-af2f-0aec65bc966c");
                char c1 = str1.charAt(i);
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2ee72421-ed44-493c-968e-d8acabd5f365");
                char c2 = str2.charAt(i);
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "eba4d3bd-6eaa-4683-b5e4-9d6339c95762");
                if (c1 != c2) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f8615798-498d-47c6-ad5c-67485a1783df");
                    break;
                }
            }
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4d5db9cd-bdf3-4357-b659-33a8e797a8e5");
        return str1.substring(0, i);
    }

    private static final class AnimationTimer {

        private final JLabel jLabel;

        private final Icon findIcon;

        private final Timer animationTimer;

        public AnimationTimer(final JLabel jLabel, Icon findIcon) {
            this.jLabel = jLabel;
            this.findIcon = findIcon;
            animationTimer = new Timer(100, new ActionListener() {

                ImageIcon[] icons;

                int index = 0;

                @Override
                public void actionPerformed(ActionEvent e) {
                    if (icons == null) {
                        icons = new ImageIcon[8];
                        for (int i = 0; i < 8; i++) {
                            // NOI18N
                            icons[i] = ImageUtilities.loadImageIcon("org/openide/awt/resources/quicksearch/progress_" + i + ".png", false);
                        }
                    }
                    jLabel.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 6));
                    jLabel.setIcon(icons[index]);
                    // mac os x
                    jLabel.repaint();
                    index = (index + 1) % 8;
                }
            });
        }

        public void startProgressAnimation() {
            if (animationTimer != null && !animationTimer.isRunning()) {
                animationTimer.start();
            }
        }

        public void stopProgressAnimation() {
            if (animationTimer != null && animationTimer.isRunning()) {
                animationTimer.stop();
                jLabel.setIcon(findIcon);
                jLabel.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));
            }
        }
    }

    private class LazyFire implements Runnable {

        private final QS_FIRE fire;

        // private final QuickSearchListener[] qsls;
        private final String searchText;

        private final boolean forward;

        private final DataContentHandlerFactory newPrefixSetter;

        LazyFire(QS_FIRE fire, String searchText) {
            this(fire, searchText, true, null);
        }

        LazyFire(QS_FIRE fire, boolean forward) {
            this(fire, null, forward);
        }

        LazyFire(QS_FIRE fire, String searchText, boolean forward) {
            this(fire, searchText, forward, null);
        }

        LazyFire(QS_FIRE fire, String searchText, DataContentHandlerFactory newPrefixSetter) {
            this(fire, searchText, true, newPrefixSetter);
        }

        LazyFire(QS_FIRE fire, String searchText, boolean forward, DataContentHandlerFactory newPrefixSetter) {
            this.fire = fire;
            // this.qsls = qsls;
            this.searchText = searchText;
            this.forward = forward;
            this.newPrefixSetter = newPrefixSetter;
            animationTimer.startProgressAnimation();
        }

        @Override
        public void run() {
            try {
                switch(fire) {
                    case // fireQuickSearchUpdate(qsls, searchText);
                            UPDATE:
                        // fireQuickSearchUpdate(qsls, searchText);
                        callback.quickSearchUpdate(searchText);
                        break;
                    case // fireShowNextSelection(qsls, forward);
                            NEXT:
                        // fireShowNextSelection(qsls, forward);
                        callback.showNextSelection(forward);
                        break;
                    case // String mp = findMaxPrefix(qsls, searchText);
                            MAX:
                        // String mp = findMaxPrefix(qsls, searchText);
                        String mp = callback.findMaxPrefix(searchText);
                        newPrefixSetter.createDataContentHandler(mp);
                        break;
                }
            } finally {
                animationTimer.stopProgressAnimation();
            }
        }
    }

    private static class SearchPanel extends JPanel {

        public static final boolean isAquaLaF = // NOI18N
                "Aqua".equals(UIManager.getLookAndFeel().getID());

        private JComponent component;

        private boolean alwaysShown = false;

        public SearchPanel(JComponent component, boolean alwaysShown) {
            this.component = component;
            this.alwaysShown = alwaysShown;
            if (isAquaLaF) {
                setBorder(BorderFactory.createEmptyBorder(9, 6, 8, 2));
            } else {
                setBorder(BorderFactory.createEmptyBorder(2, 6, 2, 2));
            }
            setOpaque(true);
        }

        @Override
        protected void paintComponent(Graphics g) {
            if (isAquaLaF && g instanceof Graphics2D) {
                Graphics2D g2d = (Graphics2D) g;
                if (alwaysShown) {
                    g2d.setColor(component.getBackground());
                    g2d.fillRect(0, 0, getWidth(), getHeight());
                } else {
                    g2d.setPaint(new // NOI18N
                            GradientPaint(// NOI18N
                            0, // NOI18N
                            0, // NOI18N
                            UIManager.getColor("NbExplorerView.quicksearch.background.top"), 0, getHeight(), // NOI18N
                            UIManager.getColor("NbExplorerView.quicksearch.background.bottom")));
                    g2d.fillRect(0, 0, getWidth(), getHeight());
                    // NOI18N
                    g2d.setColor(UIManager.getColor("NbExplorerView.quicksearch.border"));
                    g2d.drawLine(0, 0, getWidth(), 0);
                }
            } else {
                super.paintComponent(g);
            }
        }
    }

    /**
     * searchTextField manages focus because it handles VK_ESCAPE key
     */
    private class SearchTextField extends JTextField {

        private WeakReference<Component> originalFocusOwner = new WeakReference<Component>(null);

        public SearchTextField() {
        }

        void setOriginalFocusOwner() {
            Component focusOwner = KeyboardFocusManager.getCurrentKeyboardFocusManager().getFocusOwner();
            if (focusOwner != null && component.isAncestorOf(focusOwner)) {
                originalFocusOwner = new WeakReference<Component>(focusOwner);
            } else {
                originalFocusOwner = new WeakReference<Component>(component);
            }
        }

        void requestOriginalFocusOwner() {
            SwingUtilities.invokeLater(new Runnable() {

                // additional bugfix - do focus change later or removing
                // the component while it's focused will cause focus to
                // get transferred to the next component in the
                // parent focusTraversalPolicy *after* our request
                // focus completes, so focus goes into a black hole - Tim
                @Override
                public void run() {
                    Component fo = originalFocusOwner.get();
                    if (fo != null) {
                        fo.requestFocusInWindow();
                    }
                }
            });
        }

        @Override
        public boolean isManagingFocus() {
            return true;
        }

        @Override
        public void processKeyEvent(KeyEvent ke) {
            // close a modal dialog
            if (ke.getKeyCode() == KeyEvent.VK_ESCAPE) {
                removeSearchField();
                ke.consume();
                searchFieldListener.ignoreEvents = true;
                try {
                    // Clear the text after ESC
                    // NOI18N
                    setText("");
                } finally {
                    searchFieldListener.ignoreEvents = false;
                }
                // bugfix #32909, reqest focus when search field is removed
                requestOriginalFocusOwner();
                // fireQuickSearchCanceled();
                callback.quickSearchCanceled();
                hasSearchText = false;
            } else {
                if (!hasSearchText) {
                    int keyCode = ke.getKeyCode();
                    if (keyCode == KeyEvent.VK_DOWN || keyCode == KeyEvent.VK_UP || keyCode == KeyEvent.VK_LEFT || keyCode == KeyEvent.VK_RIGHT || keyCode == KeyEvent.VK_TAB || keyCode == KeyEvent.VK_F3) {
                        // Ignore movement events when search text was not set
                        return;
                    }
                }
                super.processKeyEvent(ke);
            }
        }
    }

    private class SearchFieldListener extends KeyAdapter implements DocumentListener, FocusListener {

        private boolean ignoreEvents;

        private boolean ignoreRemove;

        SearchFieldListener() {
        }

        @Override
        public void changedUpdate(DocumentEvent e) {
            if (ignoreEvents)
                return;
            searchForNode();
        }

        @Override
        public void insertUpdate(DocumentEvent e) {
            if (ignoreEvents)
                return;
            searchForNode();
        }

        @Override
        public void removeUpdate(DocumentEvent e) {
            if (ignoreEvents || ignoreRemove)
                return;
            searchForNode();
        }

        @Override
        public void keyPressed(KeyEvent e) {
            int keyCode = e.getKeyCode();
            if (keyCode == KeyEvent.VK_ESCAPE) {
                removeSearchField();
                searchTextField.requestOriginalFocusOwner();
                ignoreEvents = true;
                try {
                    // Clear the text after ESC
                    // NOI18N
                    searchTextField.setText("");
                } finally {
                    ignoreEvents = false;
                }
                // fireQuickSearchCanceled();
                callback.quickSearchCanceled();
                hasSearchText = false;
                e.consume();
            } else if (keyCode == KeyEvent.VK_UP || (keyCode == KeyEvent.VK_F3 && e.isShiftDown())) {
                fireShowNextSelection(false);
                // Stop processing the event here. Otherwise it's dispatched
                // to the tree too (which scrolls)
                e.consume();
            } else if (keyCode == KeyEvent.VK_DOWN || keyCode == KeyEvent.VK_F3) {
                fireShowNextSelection(true);
                // Stop processing the event here. Otherwise it's dispatched
                // to the tree too (which scrolls)
                e.consume();
            } else if (keyCode == KeyEvent.VK_TAB) {
                findMaxPrefix(searchTextField.getText(), new DataContentHandlerFactory() {

                    @Override
                    public DataContentHandler createDataContentHandler(final String maxPrefix) {
                        if (!SwingUtilities.isEventDispatchThread()) {
                            SwingUtilities.invokeLater(new Runnable() {

                                @Override
                                public void run() {
                                    createDataContentHandler(maxPrefix);
                                    searchTextField.transferFocus();
                                }
                            });
                            return null;
                        }
                        ignoreEvents = true;
                        try {
                            searchTextField.setText(maxPrefix);
                        } finally {
                            ignoreEvents = false;
                        }
                        return null;
                    }
                });
                e.consume();
            } else if (keyCode == KeyEvent.VK_ENTER) {
                removeSearchField();
                // fireQuickSearchConfirmed();
                callback.quickSearchConfirmed();
                component.requestFocusInWindow();
                e.consume();
            }
        }

        /**
         * Searches for a node in the tree.
         */
        private void searchForNode() {
            String text = searchTextField.getText();
            if (text.isEmpty() && isAlwaysShown()) {
                callback.quickSearchCanceled();
                hasSearchText = false;
            } else {
                fireQuickSearchUpdate(text);
            }
        }

        @Override
        public void focusGained(FocusEvent e) {
            if (e.getSource() == searchTextField) {
                // make sure nothing is selected
                int n = searchTextField.getText().length();
                searchTextField.select(n, n);
            }
        }

        @Override
        public void focusLost(FocusEvent e) {
            if (e.isTemporary() || isAlwaysShown()) {
                return;
            }
            Component oppositeComponent = e.getOppositeComponent();
            if (e.getSource() != searchTextField) {
                ((Component) e.getSource()).removeFocusListener(this);
            }
            if (oppositeComponent instanceof JMenuItem || oppositeComponent instanceof JPopupMenu) {
                oppositeComponent.addFocusListener(this);
                return;
            }
            if (oppositeComponent == searchTextField) {
                return;
            }
            if (searchPanel != null) {
                removeSearchField();
                // fireQuickSearchConfirmed();
                callback.quickSearchCanceled();
                hasSearchText = false;
            }
        }

        private class ReplaceFilter extends DocumentFilter {

            @Override
            public void replace(FilterBypass fb, int offset, int length, String text, AttributeSet attrs) throws BadLocationException {
                // Replace might do remove before insert. Suppress remove so that we get only one event with the final text
                if (text != null && !text.isEmpty()) {
                    // will insert
                    ignoreRemove = true;
                }
                try {
                    super.replace(fb, offset, length, text, attrs);
                } finally {
                    ignoreRemove = false;
                }
            }
        }
    }

    /**
     * Call back interface, that is notified with the submissions to the quick search field.
     *
     * @author Martin Entlicher
     * @since 7.43
     */
    public static interface Callback {

        /**
         * Called with an updated search text.
         * When {@link #isAsynchronous()} is <code>false</code>
         * it's called in EQ thread, otherwise, it's called in a background thread.
         * The client should update the visual representation of the search results
         * and then return.<p>
         * This method is called to initiate and update the search process.
         * @param searchText The new text to search for.
         */
        void quickSearchUpdate(String searchText);

        /**
         * Called to select a next occurrence of the search result.
         * When {@link #isAsynchronous()} is <code>false</code>
         * it's called in EQ thread, otherwise, it's called in a background thread.
         * The client should update the visual representation of the search results
         * and then return.<p>
         * @param forward The direction of the next search result.
         * <code>true</code> for forward direction,
         * <code>false</code> for backward direction.
         */
        void showNextSelection(boolean forward);

        /**
         * Find the maximum prefix among the search results, that starts with the provided string.
         * This method is called when user press TAB in the search field, to auto-complete
         * the maximum prefix.
         * When {@link #isAsynchronous()} is <code>false</code>
         * it's called in EQ thread, otherwise, it's called in a background thread.
         * Utility method {@link QuickSearch#findMaxPrefix(java.lang.String, java.lang.String, boolean)}
         * can be used by the implementation.
         * @param prefix The prefix to start with
         * @return The maximum prefix.
         */
        String findMaxPrefix(String prefix);

        /**
         * Called when the quick search is confirmed by the user.
         * This method is called in EQ thread always.
         */
        void quickSearchConfirmed();

        /**
         * Called when the quick search is canceled by the user.
         * This method is called in EQ thread always.
         */
        void quickSearchCanceled();
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