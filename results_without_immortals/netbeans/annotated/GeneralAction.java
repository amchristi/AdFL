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
 * Software is Sun Microsystems, Inc. Portions Copyright 2006 Sun
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
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;
import javax.swing.Action;
import javax.swing.ActionMap;
import org.openide.awt.ContextAction.Performer;
import org.openide.util.ContextAwareAction;
import org.openide.util.Lookup;
import org.openide.util.Parameters;
import org.openide.util.Utilities;
import org.openide.util.WeakListeners;
import org.openide.util.actions.ActionInvoker;
import java.io.*;

/**
 * @author Jaroslav Tulach
 */
final class GeneralAction {

    /**
     * Creates a new instance of DelegatingAction
     */
    private GeneralAction() {
    }

    static final Logger LOG = Logger.getLogger(GeneralAction.class.getName());

    public static ContextAwareAction callback(String key, Action defaultDelegate, Lookup context, boolean surviveFocusChange, boolean async) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ac3d993e-b002-4a40-8d8a-34ddc8035358");
        if (key == null) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "895d07a2-6424-4114-88fe-bf06866697ac");
            throw new NullPointerException();
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b15c67d7-05d8-4dfa-8852-c8383713b20d");
        return new DelegateAction(null, key, context, defaultDelegate, surviveFocusChange, async);
    }

    public static Action alwaysEnabled(Map map) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "61a10dd7-15d6-43a3-b63b-71119c248a1b");
        return new AlwaysEnabledAction(map);
    }

    public static ContextAwareAction callback(Map map) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f133f38e-cb8d-4f5e-8a61-d80e940c1375");
        Action fallback = (Action) map.get("fallback");
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2ca7e302-99c7-4e6e-9e15-76b071a497e4");
        DelegateAction d = new DelegateAction(map, fallback);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b2312cd4-15be-4c51-9ce6-eb30d1b5c2e8");
        Parameters.notNull("key", d.key);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "731d2d19-d9f8-4dc7-a53f-0e26f7f4b415");
        return d;
    }

    public static <T> ContextAwareAction context(ContextAction.Performer<? super T> perf, ContextSelection selectionType, Lookup context, Class<T> dataType) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9cb3915d-bf77-4f2a-9312-bd6ec039daf6");
        return new ContextAction<T>(perf, selectionType, context, dataType, false);
    }

    public static ContextAwareAction context(Map map) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7ed270b6-fc1b-43ea-a9bb-5d61149e5a81");
        // NOI18N
        Class<?> dataType = readClass(map.get("type"));
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "86a7b547-77ba-484f-8080-0e1e6d6fa49d");
        return new DelegateAction(map, _context(map, dataType, Utilities.actionsGlobalContext()));
    }

    public static Action bindContext(Map map, Lookup context) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3c336daf-cd14-4c25-9cad-17b119846443");
        // NOI18N
        Class<?> dataType = readClass(map.get("type"));
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f27ac7c1-a64a-4881-abc9-52d8d3a85336");
        return new BaseDelAction(map, _context(map, dataType, context));
    }

    private static <T> ContextAwareAction _context(Map map, Class<T> dataType, Lookup context) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1d002cc5-c1d6-42f8-9d1f-a3fd2095e84c");
        // NOI18N
        ContextSelection sel = readSelection(map.get("selectionType"));
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8324a306-c96b-4d31-b74f-43b4eaf5249f");
        Performer<T> perf = new Performer<T>(map);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "268c1e5f-3aa2-4a4c-bc3f-d5cfb642dea5");
        // NOI18N
        boolean survive = Boolean.TRUE.equals(map.get("surviveFocusChange"));
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a6c40a9a-79ed-4bda-b831-a8d1494e92c1");
        return new ContextAction<T>(perf, sel, context, dataType, survive);
    }

    private static ContextSelection readSelection(Object obj) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6528a067-4490-41cb-b6a5-cb61cea2ab22");
        if (obj instanceof ContextSelection) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "824b16a2-dff5-412b-88b1-0c22ad2eeff0");
            return (ContextSelection) obj;
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "96e77f20-cb4a-4cd2-8d4f-75e759b7d58e");
        if (obj instanceof String) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bd93ed4b-3250-4198-8054-a9ac8c506f4d");
            return ContextSelection.valueOf((String) obj);
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a0f71ea5-02b2-4362-abf1-676e094c71da");
        // NOI18N
        throw new IllegalStateException("Cannot parse 'selectionType' value: " + obj);
    }

    private static Class<?> readClass(Object obj) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f8eb2270-d97d-4d24-81cb-4308ceb5408b");
        if (obj instanceof Class) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "51b43336-c19e-463e-8bd4-0886a3ee5602");
            return (Class) obj;
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5d4b462f-7181-4ae0-87d2-c0738ff7ef00");
        if (obj instanceof String) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4566d3f2-b64f-46dd-8194-a8b58ac420a9");
            ClassLoader l = Lookup.getDefault().lookup(ClassLoader.class);
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "229174f0-a4b6-4477-998f-8ebbdc9dadad");
            if (l == null) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "21a28716-bc14-4232-9a4f-9fc2e52d2839");
                l = Thread.currentThread().getContextClassLoader();
            }
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8623dbed-8327-44ec-944b-5f957e965b14");
            if (l == null) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e8e6abf1-6990-4143-98cd-d9ca32a4b700");
                l = GeneralAction.class.getClassLoader();
            }
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1425e980-1dca-465c-a089-99113246c4f2");
            try {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b4d61283-fae8-46ac-8afc-0b680cdc8b06");
                return Class.forName((String) obj, false, l);
            } catch (Exception ex) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8abd354d-c57f-4391-a7a6-acf0d30f2e0b");
                throw new IllegalStateException(ex);
            }
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0cd300ac-5274-4698-8fc9-c53c6f02d5c5");
        // NOI18N
        throw new IllegalStateException("Cannot read 'type' value: " + obj);
    }

    static final Object extractCommonAttribute(Map fo, Action action, String name) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8c0a3352-bf7d-4567-acfa-18e7e233ea64");
        return AlwaysEnabledAction.extractCommonAttribute(fo, name);
    }

    public Logger getLOG() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ea7d39a2-7ddd-4938-ad8d-3055b2458f8c");
        return LOG;
    }

    /**
     * A delegate action that is usually associated with a specific lookup and
     * extract the nodes it operates on from it. Otherwise it delegates to the
     * regular NodeAction.
     */
    static final class DelegateAction extends BaseDelAction implements ContextAwareAction {

        public DelegateAction(Map map, Object key, Lookup actionContext, Action fallback, boolean surviveFocusChange, boolean async) {
            super(map, key, actionContext, fallback, surviveFocusChange, async);
        }

        public DelegateAction(Map map, Action fallback) {
            super(map, fallback);
        }
    }

    static class BaseDelAction extends Object implements Action, PropertyChangeListener {

        /**
         * file object, if we are associated to any
         */
        final Map map;

        /**
         * action to delegate too
         */
        final Action fallback;

        /**
         * key to delegate to
         */
        final Object key;

        /**
         * are we asynchronous?
         */
        final boolean async;

        /**
         * global lookup to work with
         */
        final GlobalManager global;

        /**
         * support for listeners
         */
        private PropertyChangeSupport support;

        /**
         * listener to check listen on state of action(s) we delegate to
         */
        final PropertyChangeListener weakL;

        Map<String, Object> attrs;

        /**
         * Constructs new action that is bound to given context and
         * listens for changes of <code>ActionMap</code> in order to delegate
         * to right action.
         */
        protected BaseDelAction(Map map, Object key, Lookup actionContext, Action fallback, boolean surviveFocusChange, boolean async) {
            this.map = map;
            this.key = key;
            this.fallback = fallback;
            this.global = GlobalManager.findManager(actionContext, surviveFocusChange);
            this.weakL = WeakListeners.propertyChange(this, fallback);
            this.async = async;
            if (fallback != null) {
                fallback.addPropertyChangeListener(weakL);
            }
        }

        protected BaseDelAction(Map map, Action fallback) {
            this(map, // NOI18N
            map.get("key"), // NOI18N
            Utilities.actionsGlobalContext(), // NOI18N
            fallback, // NOI18N
            Boolean.TRUE.equals(map.get("surviveFocusChange")), // NOI18N
            Boolean.TRUE.equals(map.get("asynchronous")));
        }

        /**
         * Overrides superclass method, adds delegate description.
         */
        @Override
        public String toString() {
            // NOI18N
            return super.toString() + "[key=" + key + "]";
        }

        /**
         * Invoked when an action occurs.
         */
        public void actionPerformed(final java.awt.event.ActionEvent e) {
            assert EventQueue.isDispatchThread();
            final javax.swing.Action a = findAction();
            if (a != null) {
                ActionInvoker.invokeAction(a, e, async, null);
            }
        }

        public boolean isEnabled() {
            assert EventQueue.isDispatchThread();
            javax.swing.Action a = findAction();
            return a == null ? false : a.isEnabled();
        }

        public synchronized void addPropertyChangeListener(PropertyChangeListener listener) {
            boolean first = false;
            if (support == null) {
                support = new PropertyChangeSupport(this);
                first = true;
            }
            support.addPropertyChangeListener(listener);
            if (first) {
                global.registerListener(key, this);
            }
        }

        public synchronized void removePropertyChangeListener(PropertyChangeListener listener) {
            if (support != null) {
                support.removePropertyChangeListener(listener);
                if (!support.hasListeners(null)) {
                    global.unregisterListener(key, this);
                    support = null;
                }
            }
        }

        public void putValue(String key, Object value) {
            if (attrs == null) {
                attrs = new HashMap<String, Object>();
            }
            attrs.put(key, value);
        }

        public Object getValue(String key) {
            if (attrs != null && attrs.containsKey(key)) {
                return attrs.get(key);
            }
            Object ret = GeneralAction.extractCommonAttribute(map, this, key);
            if (ret != null) {
                return ret;
            }
            Action a = findAction();
            return a == null ? null : a.getValue(key);
        }

        public void setEnabled(boolean b) {
        }

        void updateState(ActionMap prev, ActionMap now, boolean fire) {
            if (key == null) {
                return;
            }
            boolean prevEnabled = false;
            if (prev != null) {
                Action prevAction = prev.get(key);
                if (prevAction != null) {
                    prevEnabled = fire && prevAction.isEnabled();
                    prevAction.removePropertyChangeListener(weakL);
                }
            }
            if (now != null) {
                Action nowAction = now.get(key);
                boolean nowEnabled;
                if (nowAction != null) {
                    nowAction.addPropertyChangeListener(weakL);
                    nowEnabled = nowAction.isEnabled();
                } else {
                    nowEnabled = fallback != null && fallback.isEnabled();
                }
                PropertyChangeSupport sup = fire ? support : null;
                if (sup != null && nowEnabled != prevEnabled) {
                    // NOI18N
                    sup.firePropertyChange("enabled", prevEnabled, !prevEnabled);
                }
            }
        }

        /**
         * Finds an action that we should delegate to
         * @return the action or null
         */
        private Action findAction() {
            Action a = global.findGlobalAction(key);
            return a == null ? fallback : a;
        }

        /**
         * Clones itself with given context.
         */
        public Action createContextAwareInstance(Lookup actionContext) {
            Action f = fallback;
            if (f instanceof ContextAwareAction) {
                f = ((ContextAwareAction) f).createContextAwareInstance(actionContext);
            }
            DelegateAction other = new DelegateAction(map, key, actionContext, f, global.isSurvive(), async);
            if (attrs != null) {
                other.attrs = new HashMap<String, Object>(attrs);
            }
            return other;
        }

        public void propertyChange(PropertyChangeEvent evt) {
            if ("enabled".equals(evt.getPropertyName())) {
                // NOI18N
                PropertyChangeSupport sup;
                synchronized (this) {
                    sup = support;
                }
                if (sup != null) {
                    // NOI18N
                    sup.firePropertyChange("enabled", evt.getOldValue(), evt.getNewValue());
                }
            }
        }

        @Override
        public int hashCode() {
            int k = key == null ? 37 : key.hashCode();
            int m = map == null ? 17 : map.hashCode();
            int f = fallback == null ? 7 : fallback.hashCode();
            return (k << 2) + (m << 1) + f;
        }

        @Override
        public boolean equals(Object obj) {
            if (obj == this) {
                return true;
            }
            if (obj instanceof DelegateAction) {
                DelegateAction d = (DelegateAction) obj;
                if (key != null && !key.equals(d.key)) {
                    return false;
                }
                if (map != null && !map.equals(d.map)) {
                    return false;
                }
                if (fallback != null && !fallback.equals(d.fallback)) {
                    return false;
                }
                return true;
            }
            return false;
        }
    }

    // end of DelegateAction
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
