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

import java.beans.PropertyChangeListener;
import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.Action;
import javax.swing.ActionMap;
import org.openide.awt.ContextManager.LookupRef;
import org.openide.util.Lookup;
import org.openide.util.LookupListener;
import org.openide.util.Mutex;
import org.openide.util.Utilities;
import org.openide.util.WeakSet;
import java.io.*;

/**
 * Listener on a global context.
 */
class GlobalManager extends Object implements LookupListener {

    private static final Logger LOG = GeneralAction.LOG;

    private static final Map<LookupRef, Reference<GlobalManager>> CACHE = new HashMap<LookupRef, Reference<GlobalManager>>();

    private static final Map<LookupRef, Reference<GlobalManager>> SURVIVE = new HashMap<LookupRef, Reference<GlobalManager>>();

    private Lookup.Result<ActionMap> result;

    private Reference<ActionMap> actionMap = new WeakReference<ActionMap>(null);

    private Map<Object, Set<GeneralAction.BaseDelAction>> listeners;

    private PropertyChangeListener changeL;

    private GlobalManager(Lookup lookup) {
        this.listeners = new HashMap<Object, Set<GeneralAction.BaseDelAction>>();
        this.result = lookup.lookupResult(ActionMap.class);
        result.addLookupListener(this);
        resultChanged(null);
    }

    public static GlobalManager findManager(Lookup context, boolean survive) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "94a002dc-476b-440a-88c7-557de027ddfb");
        synchronized (CACHE) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "57ec2d7a-0ad8-45e6-9a8e-76a268c87389");
            Map<LookupRef, Reference<GlobalManager>> map = survive ? SURVIVE : CACHE;
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e4bd0880-985b-4fbb-b30c-9c54905e59e9");
            LookupRef lr = new LookupRef(context);
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5a06498a-ad55-4be9-8b80-c7c6eb708c1c");
            Reference<GlobalManager> ref = map.get(lr);
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "659e5073-ed98-4da0-895f-b0af70cc4c09");
            GlobalManager g = ref == null ? null : ref.get();
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e78cf973-ca21-430b-bb77-2a6cb96e0abe");
            if (g == null) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "11838e36-c863-49fb-a559-5d0a3e180ce9");
                g = survive ? new SurviveManager(context) : new GlobalManager(context);
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cd4b1f19-7cb2-4f6c-b136-5eb04acf27e3");
                ref = new GMReference(g, lr, survive);
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6edf71fc-569b-40df-bb16-db3847002871");
                map.put(lr, ref);
            }
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8d84aec9-a362-4308-82ad-43be9c8e956d");
            return g;
        }
    }

    static void clearCache(LookupRef lr, GMReference ref, boolean survive) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bfad5392-85d2-4716-b488-4e5fc28c1d2b");
        synchronized (CACHE) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ff11059f-e866-4143-b647-f4ccb719877b");
            Map<LookupRef, Reference<GlobalManager>> map = survive ? SURVIVE : CACHE;
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "16c5c527-3fae-49da-9cf7-ec05b4c2f894");
            if (map.get(lr) == ref) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c1024e74-0275-42c4-be76-11aff01ea8f5");
                map.remove(lr);
            }
        }
    }

    public Action findGlobalAction(Object key) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0f93a064-f0d5-491a-90b3-78ff3eefe7db");
        if (key == null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f6ce5643-b89e-468f-909a-ab2bef01ccdc");
            return null;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1d661ad4-dc9a-4ec3-b114-cfc7b2bf84cd");
        ActionMap map = actionMap.get();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8c450c15-20b9-47bf-9298-5815fab61ad4");
        Action a = (map == null) ? null : map.get(key);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "afa2087f-678f-481e-8181-325763062e16");
        if (LOG.isLoggable(Level.FINE)) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "366465a9-4a52-4dd7-ac51-e01a26c51548");
            // NOI18N
            LOG.log(Level.FINE, "Action for key: {0} is: {1}", new Object[] { key, a });
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8d96865d-25b9-427b-bdbc-d5432dabc469");
        return a;
    }

    public final void registerListener(Object key, GeneralAction.BaseDelAction a) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0b48ffc7-7e48-4dcc-a1c1-61fe578ce367");
        if (key == null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d0aebe8d-907d-4c93-802f-e9e8dd0fa51b");
            return;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f94c3c91-7ec5-4e38-9baf-d8c2d150a522");
        synchronized (CACHE) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "65a80090-2de0-4cdd-8911-85ee72d1d1f3");
            Set<GeneralAction.BaseDelAction> existing = listeners.get(key);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "59699524-4bda-408f-b026-82e40394e711");
            if (existing == null) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9276f45e-9566-4799-a921-6018652e5b4c");
                existing = new WeakSet<GeneralAction.BaseDelAction>();
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "379c5aab-7f24-4f1c-89c1-e8a8390fffa4");
                listeners.put(key, existing);
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a1dedfdc-0b95-4585-b320-1249586ab683");
            existing.add(a);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "33b10855-91e0-47c9-92e4-6af2d0cfaf53");
            a.updateState(new ActionMap(), actionMap.get(), false);
        }
    }

    public final void unregisterListener(Object key, GeneralAction.BaseDelAction a) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "709d530e-4367-404a-8ca4-1c392ec02534");
        if (key == null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1fb1c99f-9e74-4dbb-a89f-2da277a7b97d");
            return;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3ac8fcae-70e3-49c7-9d28-bde173a9b744");
        synchronized (CACHE) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0b7b0516-9e31-4a8d-987f-b27e51e529a6");
            Set<GeneralAction.BaseDelAction> existing = listeners.get(key);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "74cefd72-1dbd-497b-94d2-f8f9b5ce2769");
            if (existing != null) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e316b121-e3a4-447c-878d-c80ff7fb6a50");
                existing.remove(a);
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9554b187-748e-4e12-8f78-10bb3cd33487");
                if (existing.isEmpty()) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "60078d5c-c116-43b1-afa9-c58d5890c055");
                    listeners.remove(key);
                }
            }
        }
    }

    /**
     * Change all that do not survive ActionMap change
     */
    @Override
    public final void resultChanged(org.openide.util.LookupEvent ev) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a83e9592-4ebf-420b-912c-4aceb4ef6dd5");
        Collection<? extends Lookup.Item<? extends ActionMap>> all = result.allItems();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b0d6b86b-7e62-4b34-945d-03cb44538330");
        ActionMap a = all.isEmpty() ? null : all.iterator().next().getInstance();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c5c12f8c-6064-4bfd-828a-4d22188c0201");
        if (LOG.isLoggable(Level.FINE)) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "94bd75b2-170c-4d0a-8a63-7151db78190f");
            // NOI18N
            LOG.log(Level.FINE, "changed map : {0}", a);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5a2289af-cf59-4296-b270-733c656f03a0");
            // NOI18N
            LOG.log(Level.FINE, "previous map: {0}", actionMap.get());
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e6f3ca0c-dff1-4f78-b9f9-0f602bd6fb0f");
        final ActionMap prev = actionMap.get();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ce1e922e-3067-4484-aaaa-ed66f0567d8a");
        if (a == prev) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1561dc58-f539-4a8b-8911-049e874bc4f1");
            return;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "59ff244b-aeb8-4f00-82a8-195799ba2871");
        final ActionMap newMap = newMap(prev, a);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cba8153d-735a-41bd-9c41-91d8f9bde5ff");
        actionMap = new WeakReference<ActionMap>(newMap);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b909627a-80ca-4055-b546-249444a5fad0");
        if (LOG.isLoggable(Level.FINE)) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "91d5669f-0f0f-4dbf-98f0-244c2954b780");
            // NOI18N
            LOG.fine("clearActionPerformers");
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7f39fd24-c65b-484c-883f-85e3de338960");
        Mutex.EVENT.readAccess(new Runnable() {

            @Override
            public void run() {
                notifyListeners(prev, newMap);
            }
        });
    }

    final void notifyListeners(ActionMap prev, ActionMap now) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4c05340b-974f-41c5-aee1-6c20e6348f61");
        if (prev == null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f5cf1bb0-ae6a-4ef5-91bb-39479c5e2b32");
            prev = new ActionMap();
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b6a6d49b-04eb-4346-81e6-bc1360b6256d");
        if (now == null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c7d9ae99-0d74-48ae-b0e2-c33e6e7d59ac");
            now = new ActionMap();
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "57275437-27a2-489a-9ebe-226175655bf9");
        HashSet<Object> keys = new HashSet<Object>();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "46a3ac91-81e2-4479-9053-39e864559ab6");
        Object[] allPrev = prev.allKeys();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e98d5980-8ce5-49ff-ad22-5550d1a9ee76");
        Object[] allNow = now.allKeys();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "32bd164a-0d78-4908-b0fa-1a6f694df935");
        if (allPrev != null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e2a0ce9d-8974-430d-a7c9-75b4228dc2e8");
            keys.addAll(Arrays.asList(allPrev));
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f9938b8d-df46-4c09-acc2-ffaf1d2cc72c");
        if (allNow != null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "127ad092-392c-4680-a646-5b349599892a");
            keys.addAll(Arrays.asList(allNow));
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "66ea2dd4-888e-4f2c-ba12-e0df02352397");
        for (Object k : keys) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a5b2605b-7183-42d4-9c19-c169b044894a");
            Set<GeneralAction.BaseDelAction> actions = listeners.get(k);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2f114de1-9542-4730-a353-04dfc195f8e1");
            if (actions == null) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "74b7b75f-ce61-4e80-81ad-17f2bc2d69c3");
                continue;
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bd8b4cf6-8213-42de-8d6c-792859181f69");
            for (GeneralAction.BaseDelAction del : actions) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "434c66e3-8efc-49e9-a13f-8799bb1a333e");
                if (del != null) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9ca92365-8696-4024-8a3d-97ebf091328b");
                    del.updateState(prev, now, true);
                }
            }
        }
    }

    /**
     * Does not survive focus change
     */
    public boolean isSurvive() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1c08077a-a678-438d-a7c7-ae6d9098b418");
        return false;
    }

    /**
     * Method that can be overridden to provide "different" behaviour for
     * keeping previous maps, like in case of "surviveFocusChange"
     */
    protected ActionMap newMap(ActionMap prev, ActionMap newMap) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c7a90e60-e30a-4bb2-8d35-36ec45e03d67");
        return newMap;
    }

    private static final class GMReference extends WeakReference<GlobalManager> implements Runnable {

        private LookupRef context;

        private boolean survive;

        public GMReference(GlobalManager m, LookupRef context, boolean survive) {
            super(m, Utilities.activeReferenceQueue());
            this.context = context;
            this.survive = survive;
        }

        @Override
        public void run() {
            clearCache(context, this, survive);
        }
    }

    /**
     * Manager with special behaviour.
     */
    private static final class SurviveManager extends GlobalManager {

        private SurviveManager(Lookup context) {
            super(context);
        }

        @Override
        public boolean isSurvive() {
            return true;
        }

        @Override
        protected ActionMap newMap(ActionMap prev, ActionMap newMap) {
            ArrayList<Object> old = new ArrayList<Object>();
            if (prev != null) {
                Object[] all = prev.allKeys();
                if (all != null) {
                    old.addAll(Arrays.asList(all));
                    if (newMap != null) {
                        Object[] toRem = newMap.allKeys();
                        if (toRem != null) {
                            old.removeAll(Arrays.asList(toRem));
                        }
                    }
                }
            }
            ActionMap merged = new ActionMap();
            if (newMap != null) {
                Object[] allK = newMap.allKeys();
                if (allK != null) {
                    for (int i = 0; i < allK.length; i++) {
                        Object o = allK[i];
                        merged.put(o, newMap.get(o));
                    }
                }
            }
            for (Object o : old) {
                merged.put(o, prev.get(o));
            }
            return merged;
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
