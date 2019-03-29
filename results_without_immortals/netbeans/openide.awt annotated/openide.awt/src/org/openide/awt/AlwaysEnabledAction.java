/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2011 Oracle and/or its affiliates. All rights reserved.
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
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2011 Sun
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

import java.awt.EventQueue;
import java.awt.Image;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.net.URL;
import java.util.Collection;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.prefs.BackingStoreException;
import java.util.prefs.PreferenceChangeEvent;
import java.util.prefs.PreferenceChangeListener;
import java.util.prefs.Preferences;
import javax.swing.AbstractAction;
import javax.swing.AbstractButton;
import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JMenuItem;
import javax.swing.JToggleButton;
import org.openide.util.ContextAwareAction;
import org.openide.util.ImageUtilities;
import org.openide.util.Lookup;
import org.openide.util.LookupEvent;
import org.openide.util.LookupListener;
import org.openide.util.NbPreferences;
import org.openide.util.Utilities;
import org.openide.util.WeakSet;
import org.openide.util.WeakListeners;
import org.openide.util.actions.Presenter;
import org.openide.util.actions.ActionInvoker;
import java.io.*;

/**
 * Lazily initialized always enabled action
 *
 * @author Jaroslav Tulach <jtulach@netbeans.org>
 */
class AlwaysEnabledAction extends AbstractAction implements PropertyChangeListener, ContextAwareAction {

    // -J-Dorg.openide.awt.AlwaysEnabledAction.level=FINE
    private static final Logger LOG = Logger.getLogger(AlwaysEnabledAction.class.getName());

    // NOI18N
    private static final String PREFERENCES_NODE = "preferencesNode";

    // NOI18N
    private static final String PREFERENCES_KEY = "preferencesKey";

    // NOI18N
    private static final String PREFERENCES_DEFAULT = "preferencesDefault";

    static AlwaysEnabledAction create(Map m) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "61d9af01-b847-46df-8d15-66a330586b60");
        return (m.containsKey(PREFERENCES_KEY)) ? new CheckBox(m) : new AlwaysEnabledAction(m);
    }

    final Map map;

    private final AlwaysEnabledAction parent;

    private PropertyChangeListener weakL;

    ActionListener delegate;

    final Lookup context;

    final Object equals;

    public AlwaysEnabledAction(Map m) {
        super();
        this.map = m;
        this.context = null;
        this.equals = this;
        parent = null;
    }

    AlwaysEnabledAction(Map m, AlwaysEnabledAction parent, Lookup context, Object equals) {
        super();
        this.map = m;
        this.parent = parent;
        this.context = context;
        this.equals = equals;
    }

    private static ActionListener bindToContext(ActionListener a, Lookup context) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a3fd29cb-4ccc-411b-a278-d1b842754f24");
        if (context != null) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b4b2b4e8-988c-475a-89ce-01b8428efe18");
            if (a instanceof ContextAwareAction) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b26ed349-f1fa-4e31-aae5-cefe457fc27a");
                return ((ContextAwareAction) a).createContextAwareInstance(context);
            }
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c7f80604-3afd-426e-b8f9-b8d2a5716047");
        return a;
    }

    protected ActionListener getDelegate() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "26a9a8c0-46f4-433f-89c9-3addaba0a5d5");
        if (delegate == null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f1ac1124-0775-4f75-a283-612ed7f2fefd");
            ActionListener al;
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d279967d-e264-4374-acfb-db9bc436afe3");
            if (parent == null) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d050c658-e8fe-49d3-83e4-04623f480dcb");
                // NOI18N
                Object listener = map.get("delegate");
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1f6bc877-a6e6-48b5-a171-8052685c479d");
                if (!(listener instanceof ActionListener)) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0eea8317-ed02-4069-9df5-f8686c94ec38");
                    throw new NullPointerException("No 'delegate' in " + map);
                }
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d0ba5955-ea35-4002-8f45-a138e5121c7a");
                al = (ActionListener) listener;
            } else {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "250b0319-3c77-4a58-bda0-9a2a86c7159f");
                al = parent.getDelegate();
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0e916a26-0c64-47de-86ff-65df6299b9bb");
            delegate = bindToContext(al, context);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b579d025-ee3c-4d2b-8147-b4052027243e");
            if (delegate instanceof Action) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "88ec24a3-8ebe-4df8-946c-d5e3a353088a");
                Action actionDelegate = (Action) delegate;
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "956f9276-4a57-46a9-b9bd-90bdd36615f8");
                if (weakL == null) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0a31f40c-96d1-4c7d-a7b0-24bdd4706842");
                    weakL = WeakListeners.propertyChange(this, actionDelegate);
                }
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "aaf8449d-7cda-4246-bf0d-0df11047f2d7");
                actionDelegate.addPropertyChangeListener(weakL);
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3bf0edd1-c723-42a3-b7be-ac5e19ac4e7e");
                // Ensure display names and other properties are in sync or propagate them
                syncActionDelegateProperty(Action.NAME, actionDelegate);
            }
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fc6ba2a7-1fdc-4ae6-bfad-3b864c950cbe");
        return delegate;
    }

    private void syncActionDelegateProperty(String propertyName, Action actionDelegate) {
        // else either both values are null or
        // this has null and delegate has non-null which is probably fine (declarer does not care)
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "15d6c891-83e3-4715-b319-3dc2cc7ab0b0");
        Object value = extractCommonAttribute(map, propertyName);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0fdaaa61-9481-41f6-97be-e7d4ae564ddc");
        Object delegateValue = actionDelegate.getValue(propertyName);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7e515bd2-1c21-424f-aeb7-9f1a350e047c");
        if (value != null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2e979945-01b9-4380-ac07-08e782e50848");
            if (delegateValue == null) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "104f3542-f41a-4253-b374-25305dadeb44");
                actionDelegate.putValue(propertyName, value);
            } else {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "edfcebf5-8a69-430c-9b82-caf5dbddbb77");
                if (!delegateValue.equals(value)) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6c8fe8b1-524a-4520-8157-f61241d9ce69");
                    // Values differ
                    LOG.log(Level.FINE, "Value of property \"{0}\" of AlwaysEnabledAction " + "is \"{1}\" but delegate {2} has \"{3}\"", new Object[] { propertyName, value, delegate, delegateValue });
                }
            }
        }
    }

    @Override
    public boolean isEnabled() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9f9612c0-9867-4540-81a4-db6189f90b9c");
        // assert EventQueue.isDispatchThread();
        if (delegate instanceof Action) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9dfa157f-0947-4523-a808-6f6595eec490");
            return ((Action) delegate).isEnabled();
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e08b6c80-72ed-4106-8823-676378f81772");
        return true;
    }

    public void actionPerformed(final ActionEvent e) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2ecb810b-4327-4c54-9c05-a8c11859a2cd");
        if (getDelegate() instanceof Action) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "964a0736-1a87-4206-940d-cccd6367c0e7");
            if (!((Action) getDelegate()).isEnabled()) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a3324e2f-5e8f-4d0d-a671-a31b9ac64674");
                Utilities.disabledActionBeep();
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f872b62f-b498-471a-bfe6-81cea961e615");
                // Do not fire newValue == null (see #165838)
                // NOI18N
                firePropertyChange("enabled", null, isEnabled());
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "abfcd12d-0fb8-4e2b-8c25-8faae72065c3");
                return;
            }
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e43fb383-b45a-4d62-ae44-d90a44f94fc8");
        // NOI18N
        boolean async = Boolean.TRUE.equals(map.get("asynchronous"));
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ca690eb2-df5d-4ff2-8a6e-3dfb7e2ae733");
        Runnable ar = new Runnable() {

            public void run() {
                getDelegate().actionPerformed(e);
            }
        };
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4c547e28-159a-4499-93cf-c978af9f7cfe");
        ActionInvoker.invokeAction(this, e, async, ar);
    }

    @Override
    public Object getValue(String name) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fb310e92-b32d-4ca1-92d3-a62da5044a58");
        if (delegate instanceof Action) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4b116094-0fd0-4cc6-8763-cdaaf062ee84");
            Object ret = ((Action) delegate).getValue(name);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d4ca7d4e-1419-4d28-873f-faf1754cb4bb");
            if (ret != null) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e4a4aef4-a5d5-4c2d-843e-fa35d8d4af99");
                return ret;
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a3f078f2-1238-46cf-a0fe-fbd13fad36ff");
            if (// NOI18N
                    "iconBase".equals(name) && ((Action) delegate).getValue(Action.SMALL_ICON) != null) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1f1611d5-2e7f-4ccc-b0f6-b4439420a8e4");
                return null;
            }
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "eda53625-296e-4b95-9dab-c709b0da3149");
        Object o = extractCommonAttribute(map, name);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "94c795d7-83ad-4f8f-9fbb-e24165ad73b1");
        // cf. #137709 JG18:
        return o != null ? o : super.getValue(name);
    }

    static final Object extractCommonAttribute(Map fo, String name) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9cc4a607-f57e-4f11-a9e0-d174b07ec4b3");
        try {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "31252feb-9aaf-4dd2-8d2e-d4091a3089df");
            if (Action.NAME.equals(name)) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a179f350-1d55-4f35-aea0-9c7390a92ceb");
                // NOI18N
                String actionName = (String) fo.get("displayName");
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "36019fc9-c346-4005-820c-b07c2e9c58d0");
                // return Actions.cutAmpersand(actionName);
                return actionName;
            }
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "18d20a8d-cb26-4df6-84be-ea8ea94f43e2");
            if (Action.MNEMONIC_KEY.equals(name)) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2b618ff0-e061-483c-a67f-f4a73ee1ef26");
                // NOI18N
                String actionName = (String) fo.get("displayName");
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6f5d2710-c2a2-4b7b-8235-81124d2c2dd3");
                if (null == actionName)
                    return null;
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3c9baec5-cf81-4096-b43a-9609cf90f14b");
                // NOI18N
                int position = Mnemonics.findMnemonicAmpersand(actionName);
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2f2cec90-da39-4763-9f5c-a08dea9a4fbd");
                if (position == -1) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c699785d-51ad-44ed-8764-0a949dd98c24");
                    return null;
                } else {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7e6bdf6b-fb61-44c6-a7fa-33d5abb7dcc1");
                    // #167996: copied from AbstractButton.setMnemonic
                    int vk = (int) actionName.charAt(position + 1);
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ed91eecf-432d-4146-a615-59586e6ba9fe");
                    if (vk >= 'a' && vk <= 'z') {
                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9e5ce0d3-d035-45aa-a9c4-3594c22a8787");
                        // NOI18N
                        // NOI18N
                        vk -= ('a' - 'A');
                    }
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8904535a-8890-413e-a8b0-d8df8556a570");
                    return vk;
                }
            }
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b1cbe34d-71e6-4b0c-9c70-0c4334c10b99");
            if (Action.SMALL_ICON.equals(name)) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "aad9b5c4-3675-4a6c-85a2-5a13bab12ff5");
                // NOI18N
                Object icon = fo == null ? null : fo.get("iconBase");
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3bd67f48-a53c-46e4-9f2c-854052ab3b42");
                if (icon instanceof Icon) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "178a4089-1c53-4920-b954-75472664da83");
                    return (Icon) icon;
                }
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3c3d7256-ca52-42bc-9c5b-2ad0e64b0b60");
                if (icon instanceof URL) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "53932f2c-c4d2-4bfd-b1e9-5e851d45e151");
                    icon = Toolkit.getDefaultToolkit().getImage((URL) icon);
                }
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ddee5d92-af7f-43d1-b0ca-57798c5ab7e6");
                if (icon instanceof Image) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "790bb15d-a280-488c-bb1f-dde771a3b94d");
                    return ImageUtilities.image2Icon((Image) icon);
                }
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d2fb3bd0-3130-458b-a599-af266df0b413");
                if (icon instanceof String) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0f43b9b2-22ed-4803-a160-ebd46d1e83ae");
                    return ImageUtilities.loadImageIcon((String) icon, true);
                }
            }
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "413fd2a8-4a01-415c-a267-f84651a62c39");
            if ("iconBase".equals(name)) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ea00f72f-deb7-4d67-817a-c4c7da2dfa40");
                // NOI18N
                return fo == null ? null : fo.get("iconBase");
            }
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "52ecb075-76be-437f-88a9-48b10f2a0ccb");
            if ("noIconInMenu".equals(name)) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "767cbc44-5e6c-445b-9990-a44a89001c66");
                // NOI18N
                return fo == null ? null : fo.get("noIconInMenu");
            }
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "04cdb270-21ea-405b-9094-4c8c70db7c79");
            // Delegate query to other properties to "fo" ignoring special properties
            if (!"delegate".equals(name) && !"instanceCreate".equals(name)) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "74bdd01c-6ace-4a5b-b417-8760e29a276b");
                return fo == null ? null : fo.get(name);
            }
        } catch (RuntimeException x) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ccf2e864-fccc-4dc8-9158-42e9ec896df9");
            // noted in #172103
            LOG.log(Level.WARNING, "Could not get action attribute " + name, x);
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2bb758e8-92c5-446f-839a-06df8f74f8b8");
        return null;
    }

    @Override
    public int hashCode() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f332bb71-4c74-4e2e-98c8-d393c50f29e9");
        if (equals == this) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2691ba3b-fb3d-4923-8487-c1bea7c5e5a7");
            return super.hashCode();
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7a892eef-05a1-4649-9282-8f8299d0bcdc");
        return equals.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "93679a7f-538f-450c-ae98-dadc217fab9b");
        if (obj == this) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "eccc7a08-701c-4ea8-b8f6-19b4018496ad");
            return true;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "64e5ce9e-da7e-4161-b4a1-a182bda9e585");
        if (obj instanceof AlwaysEnabledAction) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8933b1e9-e36d-4627-a09e-b43b01a73bbb");
            final AlwaysEnabledAction other = (AlwaysEnabledAction) obj;
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6169417d-f7ed-493e-89ec-6c9187858d6d");
            if (this == this.equals && other == other.equals) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a4c5d241-1d07-4494-9514-3ff6180084c5");
                return (this == other);
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "957e4ea5-c632-4725-a4e0-b4c22daa80e2");
            if (this.equals.equals(other.equals)) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "785a07ff-e762-4817-b71a-f614ee73dc10");
                return true;
            }
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "14cb0834-9f5e-40de-af31-eebaf85fa81d");
        return false;
    }

    @Override
    public String toString() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "873e7e00-add5-4b72-9560-3f7f80a5b0b5");
        // NOI18N
        return "AlwaysEnabledAction[" + getValue(Action.NAME) + "]";
    }

    public void propertyChange(PropertyChangeEvent evt) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "62c90cfb-5652-4bef-ab8d-bd80b6cd27c4");
        if (evt.getSource() == delegate) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4d8322bd-117b-451a-9679-20dfeaab5c5c");
            firePropertyChange(evt.getPropertyName(), evt.getOldValue(), evt.getNewValue());
        }
    }

    public Action createContextAwareInstance(Lookup actionContext) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d4912853-5366-4509-b164-fc858eb07ce6");
        return new AlwaysEnabledAction(map, this, actionContext, equals);
    }

    static final class CheckBox extends AlwaysEnabledAction implements Presenter.Menu, Presenter.Popup, Presenter.Toolbar, PreferenceChangeListener, LookupListener {

        private static final long serialVersionUID = 1L;

        private static final ActionListener EMPTY = new ActionListener() {

            public void actionPerformed(ActionEvent ae) {
                // Do nothing
            }
        };

        private JCheckBoxMenuItem menuItem;

        private JCheckBoxMenuItem popupItem;

        private WeakSet<AbstractButton> toolbarItems;

        private Preferences preferencesNode;

        private Lookup.Result<Preferences> preferencesNodeResult;

        private boolean prefsListening;

        CheckBox(Map m) {
            super(m);
        }

        CheckBox(Map m, AlwaysEnabledAction parent, Lookup context, Object equals) {
            super(m, parent, context, equals);
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            // Toggle state in preferences
            togglePreferencesSelected();
            super.actionPerformed(e);
        }

        public JMenuItem getMenuPresenter() {
            if (menuItem == null) {
                menuItem = new JCheckBoxMenuItem();
                menuItem.setSelected(isPreferencesSelected());
                Actions.connect(menuItem, this, false);
            }
            return menuItem;
        }

        public JMenuItem getPopupPresenter() {
            if (popupItem == null) {
                popupItem = new JCheckBoxMenuItem();
                popupItem.setSelected(isPreferencesSelected());
                Actions.connect(popupItem, this, true);
            }
            return popupItem;
        }

        public AbstractButton getToolbarPresenter() {
            if (toolbarItems == null) {
                toolbarItems = new WeakSet<AbstractButton>(4);
            }
            AbstractButton b = new DefaultIconToggleButton();
            toolbarItems.add(b);
            b.setSelected(isPreferencesSelected());
            Actions.connect(b, this);
            return b;
        }

        public void preferenceChange(PreferenceChangeEvent pce) {
            updateItemsSelected();
        }

        @Override
        protected ActionListener getDelegate() {
            return EMPTY;
        }

        @Override
        public Action createContextAwareInstance(Lookup actionContext) {
            return new CheckBox(map, this, actionContext, equals);
        }

        private boolean isPreferencesSelected() {
            String key = (String) getValue(PREFERENCES_KEY);
            Preferences prefs = prefs();
            boolean value;
            if (key != null && prefs != null) {
                Object defaultValue = getValue(PREFERENCES_DEFAULT);
                value = prefs.getBoolean(key, defaultValue instanceof Boolean ? (Boolean) defaultValue : false);
                synchronized (this) {
                    if (!prefsListening) {
                        prefsListening = true;
                        prefs.addPreferenceChangeListener(this);
                    }
                }
            } else {
                value = false;
            }
            return value;
        }

        private void updateItemsSelected() {
            boolean selected = isPreferencesSelected();
            if (menuItem != null) {
                menuItem.setSelected(selected);
            }
            if (popupItem != null) {
                popupItem.setSelected(selected);
            }
            if (toolbarItems != null) {
                for (AbstractButton b : toolbarItems) {
                    b.setSelected(selected);
                }
            }
        }

        private synchronized Preferences prefs() {
            if (preferencesNode == null) {
                Object prefsNodeOrLookup = getValue(PREFERENCES_NODE);
                if (prefsNodeOrLookup instanceof String) {
                    String nodeName = (String) prefsNodeOrLookup;
                    if (nodeName.startsWith("system:")) {
                        preferencesNode = Preferences.systemRoot();
                        if (preferencesNode != null) {
                            nodeName = nodeName.substring("system:".length());
                            try {
                                preferencesNode = preferencesNode.nodeExists(nodeName) ? preferencesNode.node(nodeName) : null;
                            } catch (BackingStoreException ex) {
                                preferencesNode = null;
                            }
                        }
                    } else if (nodeName.startsWith("user:")) {
                        preferencesNode = Preferences.userRoot();
                        if (preferencesNode != null) {
                            nodeName = nodeName.substring("user:".length());
                            try {
                                preferencesNode = preferencesNode.nodeExists(nodeName) ? preferencesNode.node(nodeName) : null;
                            } catch (BackingStoreException ex) {
                                preferencesNode = null;
                            }
                        }
                    } else {
                        preferencesNode = NbPreferences.root();
                        if (preferencesNode != null) {
                            try {
                                preferencesNode = preferencesNode.nodeExists(nodeName) ? preferencesNode.node(nodeName) : null;
                            } catch (BackingStoreException ex) {
                                preferencesNode = null;
                            }
                        }
                    }
                } else if (prefsNodeOrLookup instanceof Preferences) {
                    preferencesNode = (Preferences) prefsNodeOrLookup;
                } else if (prefsNodeOrLookup instanceof Lookup) {
                    Lookup prefsLookup = (Lookup) prefsNodeOrLookup;
                    preferencesNodeResult = prefsLookup.lookupResult(Preferences.class);
                    Collection<? extends Preferences> instances = preferencesNodeResult.allInstances();
                    if (instances.size() > 0) {
                        preferencesNode = instances.iterator().next();
                        preferencesNodeResult.addLookupListener(this);
                    }
                    return prefsLookup.lookup(Preferences.class);
                } else {
                    preferencesNode = null;
                }
            }
            return preferencesNode;
        }

        public void resultChanged(LookupEvent ev) {
            preferencesNode = null;
            preferencesNodeResult = null;
            updateItemsSelected();
        }

        private void togglePreferencesSelected() {
            String key = (String) getValue(PREFERENCES_KEY);
            Preferences prefs = prefs();
            if (key != null && prefs != null) {
                Object defaultValue = getValue(PREFERENCES_DEFAULT);
                prefs.putBoolean(key, !prefs.getBoolean(key, defaultValue instanceof Boolean ? (Boolean) defaultValue : false));
            }
        }
    }

    /**
     * A button that provides a default icon when no text and no custom icon have been set.
     * Copied from Toolbar.java and made a toggle button.
     */
    static class DefaultIconToggleButton extends JToggleButton {

        private Icon unknownIcon;

        @Override
        public Icon getIcon() {
            Icon retValue = super.getIcon();
            if (null == retValue && (null == getText() || getText().isEmpty())) {
                if (unknownIcon == null) {
                    // NOI18N
                    unknownIcon = ImageUtilities.loadImageIcon("org/openide/awt/resources/unknown.gif", false);
                    // unknownIcon = ImageUtilities.loadImageIcon("org/openide/loaders/unknown.gif", false); //NOI18N
                }
                retValue = unknownIcon;
            }
            return retValue;
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