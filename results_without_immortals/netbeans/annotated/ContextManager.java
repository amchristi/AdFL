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

import java.awt.event.ActionEvent;
import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.openide.util.Lookup;
import org.openide.util.Lookup.Item;
import org.openide.util.Lookup.Provider;
import org.openide.util.Lookup.Result;
import org.openide.util.LookupEvent;
import org.openide.util.LookupListener;
import org.openide.util.Mutex;
import org.openide.util.Utilities;
import org.openide.util.WeakListeners;
import org.openide.util.WeakSet;
import org.openide.util.lookup.Lookups;
import org.openide.util.lookup.ProxyLookup;
import java.io.*;

/**
 * Listener on a global context.
 */
class ContextManager extends Object {

    private static final Logger LOG = GeneralAction.LOG;

    private static final Map<LookupRef, Reference<ContextManager>> CACHE = new HashMap<LookupRef, Reference<ContextManager>>();

    private static final Map<LookupRef, Reference<ContextManager>> SURVIVE = new HashMap<LookupRef, Reference<ContextManager>>();

    private Map<Class, LSet> listeners;

    private Lookup lookup;

    private LSet<Lookup.Provider> selectionAll;

    private ContextManager(Lookup lookup) {
        this.listeners = new HashMap<Class, LSet>();
        this.lookup = lookup;
    }

    public static ContextManager findManager(Lookup context, boolean survive) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f7cd5644-d942-40ba-b700-1cd71b0202e5");
        synchronized (CACHE) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5c804a6f-8e6a-4888-a3c2-eb7d51868feb");
            Map<LookupRef, Reference<ContextManager>> map = survive ? SURVIVE : CACHE;
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "648f60c8-27fb-4538-9fe7-1136486174d7");
            LookupRef lr = new LookupRef(context);
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "659cdc64-66a4-460a-b528-76c59ebdef14");
            Reference<ContextManager> ref = map.get(lr);
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0cdb8200-8ca3-4e9a-b9f7-88e194acd767");
            ContextManager g = ref == null ? null : ref.get();
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a419647b-5c34-4190-a6d1-fd2f76af5014");
            if (g == null) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "628e578d-81c5-4fd7-8ffc-4ebdbf180ce2");
                g = survive ? new SurviveManager(context) : new ContextManager(context);
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "023faceb-84f9-4827-b880-a2306b09a445");
                ref = new GMReference(g, lr, survive);
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "abe577ea-5b99-463b-a9fd-ce01118ff497");
                map.put(lr, ref);
            }
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "db98b58b-58d5-4aeb-a4b4-cffe62af5799");
            return g;
        }
    }

    static void clearCache(LookupRef lr, GMReference ref, boolean survive) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9af2b8f9-04c8-4a13-b245-a974487d254b");
        synchronized (CACHE) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cb427ee7-d0e8-49d6-8231-fb353eaac156");
            Map<LookupRef, Reference<ContextManager>> map = survive ? SURVIVE : CACHE;
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "dfd57229-d739-4b51-9f94-b5602cb1520a");
            if (map.get(lr) == ref) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8f5c24f8-008b-4ca5-8cc1-8bf784ea9da8");
                map.remove(lr);
            }
        }
    }

    public <T> void registerListener(Class<T> type, ContextAction<T> a) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8ae700e1-b0ad-4cb7-a962-da2854e1e9bf");
        synchronized (CACHE) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1669069e-6cd9-4948-ad5c-07399af63337");
            LSet<T> existing = findLSet(type);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2cfcaeed-459d-44e1-86e2-70a44bdc5c48");
            if (existing == null) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2d651454-689a-4344-a258-5a0aa38f62d0");
                Lookup.Result<T> result = createResult(lookup.lookupResult(type));
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9b260c0a-e43b-4b47-85d6-37f5ecc84c0f");
                existing = new LSet<T>(result, type);
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "584fddee-950b-4377-98fe-c698449a21d1");
                listeners.put(type, existing);
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "878548ab-40f6-44f5-a443-eec5c50275f7");
            existing.add(a);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "252bf49b-ddf7-48e8-b6ce-a08bda52a1ea");
            if (a.selectMode == ContextSelection.ALL) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9036f25f-1490-4984-aee3-65cd4c3a4df4");
                initSelectionAll();
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "16a172a3-0fdc-450d-81e8-bf9bc1bb969a");
                selectionAll.add(a);
            }
        }
    }

    public <T> void unregisterListener(Class<T> type, ContextAction<T> a) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c70ac01c-ceaa-4ec0-bccf-87499b9f90ff");
        synchronized (CACHE) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "369c3d6a-f6cf-46b0-8a88-24b9dd69037e");
            LSet<T> existing = findLSet(type);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a0e2bae2-647b-4709-995d-ae4a581183d1");
            if (existing != null) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6929fef1-16ac-4a10-b851-cc26b9347d1e");
                existing.remove(a);
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4abfa464-1ed1-463d-b51f-d52ef079f7fd");
                if (existing.isEmpty()) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ebb42670-c0f3-429f-b122-cf5e36908a4f");
                    listeners.remove(type);
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f2e03fc0-3bb0-488b-901d-a04f4dc0a925");
                    existing.cleanup();
                }
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "519897e2-abe1-4e89-8382-9cbb5e8c9938");
            if (a.selectMode == ContextSelection.ALL && selectionAll != null) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5053f4c4-c9ce-4ff6-8aad-f08a2b424b8d");
                selectionAll.remove(a);
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0ec90625-1a13-4e48-8d8c-7700060bf7f2");
                if (selectionAll.isEmpty() && !isSurvive()) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8990a2c3-82f7-470a-8e7f-9f7febaff826");
                    selectionAll = null;
                }
            }
        }
    }

    /**
     * Does not survive focus change
     */
    public boolean isSurvive() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9d5b3fba-c15f-4f88-9554-85a5b71e6734");
        return false;
    }

    /**
     * Checks whether a type is enabled.
     */
    public <T> boolean isEnabled(Class<T> type, ContextSelection selectMode, ContextAction.Performer<? super T> enabler) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f6ca6cbe-5fc4-4f4c-8945-504cf9e191ca");
        Lookup.Result<T> result = findResult(type);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d9b4c445-418c-40a5-9914-bb85f5f90060");
        boolean e = isEnabledOnData(result, type, selectMode);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d0fd40f9-3ee5-43b5-9e15-cc50d99e83e4");
        if (e && enabler != null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "80328571-a647-4df1-ae2c-19e9b058b109");
            e = enabler.enabled(listFromResult(result));
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "58f237c5-190a-412d-9ee3-d46a9c3ad731");
        return e;
    }

    private <T> boolean isEnabledOnData(Lookup.Result<T> result, Class<T> type, ContextSelection selectMode) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "484280b1-94c8-4885-8ca5-c8eb1b83abb9");
        boolean res = isEnabledOnDataImpl(result, type, selectMode);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9f517b28-2ee0-42e7-b5ed-7da27939cef5");
        LOG.log(Level.FINE, "isEnabledOnData(result, {0}, {1}) = {2}", new Object[] { type, selectMode, res });
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8f0eff1d-4020-417c-8872-9b127068d298");
        return res;
    }

    private <T> boolean isEnabledOnDataImpl(Lookup.Result<T> result, Class<T> type, ContextSelection selectMode) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "94db55b5-2020-4514-bb72-720eefce63cb");
        switch(selectMode) {
            case EXACTLY_ONE:
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a336d61a-b52b-4922-baad-f3418688ea39");
                Collection<Lookup.Item<T>> instances = new HashSet<Lookup.Item<T>>(result.allItems());
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8b836fac-c221-40e5-b185-a15cb6e91e39");
                return instances.size() == 1;
            case ANY:
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "31fef181-baf0-48d4-a7d2-b69a9b41fc36");
                return !result.allItems().isEmpty();
            case EACH:
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2e32a655-8216-42fd-b291-0c85a9fb77cc");
                {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4bcffdf6-c21c-4fac-a2bd-70a7f6c97078");
                    if (result.allItems().isEmpty()) {
                        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4a7f3717-c200-4e0b-b4d1-1064a096167f");
                        return false;
                    }
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "32feccdc-9d6a-4a2e-8499-f29dfc0391f2");
                    Lookup.Result<Lookup.Provider> items = lookup.lookupResult(Lookup.Provider.class);
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "40fff3d4-2663-46b4-a199-51c48c3ca73e");
                    if (result.allItems().size() != items.allItems().size()) {
                        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "474e9104-eaba-47b9-85ae-699e33441f50");
                        return false;
                    }
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "342fc11f-c59b-4cca-af2c-f4193f693b52");
                    Lookup.Template<T> template = new Lookup.Template<T>(type);
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2413b4e8-3652-49b8-938b-68c4768e3d79");
                    for (Lookup.Provider prov : items.allInstances()) {
                        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3ff21428-eb64-4c28-8aca-daf5f17a6942");
                        if (prov.getLookup().lookupItem(template) == null) {
                            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "505dd255-19dc-458b-9c93-a0d6b9bb1ccf");
                            return false;
                        }
                    }
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5d314ca5-40e1-41f7-8446-e29d6ac965d8");
                    return true;
                }
            case ALL:
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "19b7218b-7be5-4ef8-80ab-d94b5f9bee1a");
                {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b598315c-5ebe-4d48-b079-28f145ddb9e8");
                    if (result.allItems().isEmpty()) {
                        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "58a4c6c3-0633-4178-ba4f-11edbbff3c24");
                        return false;
                    }
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5d85c4d3-a699-4da4-86d3-09d8599b1ed8");
                    Lookup.Result<Lookup.Provider> items = lookup.lookupResult(Lookup.Provider.class);
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "787b15a5-2de7-45f2-b661-c7764e397695");
                    if (result.allItems().size() < items.allItems().size()) {
                        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3e7f3aee-4105-4482-94db-1279fe0f180c");
                        return false;
                    }
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "98e7ba79-21e4-4f4b-ac70-3f86538f4d7e");
                    Lookup.Template<T> template = new Lookup.Template<T>(type);
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "12ce184c-a5ce-4268-aea1-e79386aaf6bf");
                    for (Lookup.Provider prov : items.allInstances()) {
                        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b52fc675-560c-46b6-a8b5-dad6f3986083");
                        if (prov.getLookup().lookupItem(template) == null) {
                            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b510b0b9-0bbf-472b-b0f4-8009cdcc2c47");
                            return false;
                        }
                    }
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a618ddb5-40db-4149-8647-7f7f010b2191");
                    return true;
                }
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3e2f4f5b-28b8-463c-8d1e-aae54cbf9fdf");
        return false;
    }

    // package private for tests only
    @SuppressWarnings("unchecked")
    <T> LSet<T> findLSet(Class<T> type) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "411ae735-0419-4dfe-af32-504284e8b8db");
        synchronized (CACHE) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4c62fb4c-a49f-4291-b734-7b2c81861102");
            return listeners.get(type);
        }
    }

    private <T> Lookup.Result<T> findResult(final Class<T> type) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0447368f-2091-49c0-b9ff-53a455aa829e");
        LSet<T> lset = findLSet(type);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c321549e-a605-4130-919a-f934cff0e47c");
        Lookup.Result<T> result;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "99f4393f-0f1e-4068-bf0b-300571774cf4");
        if (lset != null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "44e942b6-e3d5-410b-ba5d-7153d8fe5fe7");
            result = lset.result;
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "78000017-1c00-4493-b7c1-b383a4294ef3");
            result = lookup.lookupResult(type);
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c09af0cf-b5de-44c1-acba-554c869127c9");
        return result;
    }

    protected <T> Lookup.Result<T> createResult(Lookup.Result<T> res) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d603474f-e5ad-4536-a0d9-56c562bec9a8");
        return res;
    }

    public <T> void actionPerformed(final ActionEvent e, ContextAction.Performer<? super T> perf, final Class<T> type, ContextSelection selectMode) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "506f6ac7-2f99-4b45-ae13-f560b37b9dcf");
        Lookup.Result<T> result = findResult(type);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9d209785-3e76-48fc-ab58-abd4b10a6651");
        final List<? extends T> all = listFromResult(result);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4d825547-c84b-4432-8508-a2c30c2f4890");
        perf.actionPerformed(e, Collections.unmodifiableList(all), new LkpAE());
    }

    private <T> List<? extends T> listFromResult(Lookup.Result<T> result) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cbcdc361-a1b7-4978-a5d3-db797b31089c");
        Collection<? extends T> col = result.allInstances();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "26455561-d80d-4bd4-9e96-5fa6b95a71de");
        Collection<T> tmp = new LinkedHashSet<T>(col);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "86f9ee0a-8f61-42f5-9bf0-4289ee473848");
        if (tmp.size() != col.size()) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4590c068-b208-491e-b0a6-ef57834b3f05");
            Collection<T> nt = new ArrayList<T>(tmp.size());
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "39025878-1d09-40cf-a4dd-502f8e8c951b");
            nt.addAll(tmp);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "120e84f7-3b75-4a7d-addf-42b8751960a5");
            col = nt;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6606b70d-c7e0-47e6-8b04-a283d9521e83");
        List<? extends T> all;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6af27a9a-196a-4501-9dc9-b3d40fbb7fff");
        if (col instanceof List) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "51d070b9-e809-408f-91fe-90457aa55ebb");
            all = (List<? extends T>) col;
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5390911f-1dc1-4bd5-99b4-c1c4033643dd");
            ArrayList<T> arr = new ArrayList<T>();
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2900550a-ab0e-4921-b74a-8cd25479f215");
            arr.addAll(col);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1bcb6954-2021-41fa-9f71-3587a6b66247");
            all = arr;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "06a8ed4d-1f97-488d-b04b-b70cca0ed0a7");
        return all;
    }

    private Lookup.Result<Lookup.Provider> initSelectionAll() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9596af00-ea98-48fc-bdbb-0f5226cd1e3a");
        if (selectionAll == null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b1ef1e6c-cc31-4dde-ad7a-64df395caeb8");
            Lookup.Result<Lookup.Provider> result = lookup.lookupResult(Lookup.Provider.class);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d01ba9c0-5882-44a7-b2e9-bb32708399c4");
            selectionAll = new LSet<Lookup.Provider>(result, Lookup.Provider.class);
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4a4548ed-75e2-4f5a-b25a-df06505ef9a8");
        return selectionAll.result;
    }

    private static final class GMReference extends WeakReference<ContextManager> implements Runnable {

        private LookupRef context;

        private boolean survive;

        public GMReference(ContextManager m, LookupRef context, boolean survive) {
            super(m, Utilities.activeReferenceQueue());
            this.context = context;
            this.survive = survive;
        }

        public void run() {
            clearCache(context, this, survive);
        }
    }

    /**
     * Manager with special behaviour.
     */
    private static final class SurviveManager extends ContextManager {

        private SurviveManager(Lookup context) {
            super(context);
        }

        @Override
        public boolean isSurvive() {
            return true;
        }

        @Override
        protected <T> Result<T> createResult(Result<T> res) {
            return new NeverEmptyResult<T>(res, super.initSelectionAll());
        }
    }

    private static final class NeverEmptyResult<T> extends Lookup.Result<T> implements LookupListener {

        private final Lookup.Result<T> delegate;

        private final Lookup.Result<Provider> nodes;

        private final Collection<LookupListener> listeners;

        private Collection<? extends Item<T>> allItems;

        private Collection<? extends T> allInstances;

        private Set<Class<? extends T>> allClasses;

        public NeverEmptyResult(Result<T> delegate, Result<Provider> nodes) {
            this.delegate = delegate;
            this.nodes = nodes;
            this.listeners = new CopyOnWriteArrayList<LookupListener>();
            // add weak listeners so this can be GCed when listeners are empty
            this.delegate.addLookupListener(WeakListeners.create(LookupListener.class, this, this.delegate));
            this.nodes.addLookupListener(WeakListeners.create(LookupListener.class, this, this.nodes));
            initValues();
        }

        @Override
        public void addLookupListener(LookupListener l) {
            listeners.add(l);
        }

        @Override
        public void removeLookupListener(LookupListener l) {
            listeners.remove(l);
        }

        @Override
        public Collection<? extends Item<T>> allItems() {
            Collection<? extends Item<T>> res = delegate.allItems();
            synchronized (this) {
                if (!res.isEmpty()) {
                    allItems = res;
                }
                return allItems;
            }
        }

        @Override
        public Collection<? extends T> allInstances() {
            Collection<? extends T> res = delegate.allInstances();
            synchronized (this) {
                if (!res.isEmpty()) {
                    allInstances = res;
                }
                return allInstances;
            }
        }

        @Override
        public Set<Class<? extends T>> allClasses() {
            Set<Class<? extends T>> res = delegate.allClasses();
            synchronized (this) {
                if (!res.isEmpty()) {
                    allClasses = res;
                }
                return allClasses;
            }
        }

        @Override
        public void resultChanged(LookupEvent ev) {
            if (ev.getSource() == nodes) {
                Collection<? extends Item<Provider>> arr = nodes.allItems();
                if (arr.size() == 1 && arr.iterator().next().getInstance() == null) {
                    return;
                }
                initValues();
                return;
            }
            final LookupEvent mev = new LookupEvent(this);
            for (LookupListener ll : listeners) {
                ll.resultChanged(mev);
            }
        }

        private synchronized void initValues() {
            allItems = Collections.emptyList();
            allInstances = Collections.emptyList();
            allClasses = Collections.emptySet();
        }
    }

    /**
     * Special set, that is weakly holding its actions, but also
     * listens on changes in lookup.
     */
    static final class LSet<T> extends WeakSet<ContextAction> implements LookupListener, Runnable {

        final Lookup.Result<T> result;

        public LSet(Lookup.Result<T> context, Class<T> type) {
            this.result = context;
            this.result.addLookupListener(this);
            // activate listener
            this.result.allItems();
        }

        @Override
        public boolean add(ContextAction e) {
            assert e != null;
            return super.add(e);
        }

        @Override
        public void resultChanged(LookupEvent ev) {
            Mutex.EVENT.readAccess(this);
        }

        @Override
        public void run() {
            ContextAction[] arr;
            synchronized (CACHE) {
                arr = toArray(new ContextAction[0]);
            }
            long now = 0;
            assert (now = System.currentTimeMillis()) >= 0;
            for (ContextAction a : arr) {
                if (a != null) {
                    a.updateState();
                }
            }
            long took = 0;
            assert (took = System.currentTimeMillis() - now) >= 0;
            if (took > 2000) {
                LOG.log(Level.WARNING, "Updating state of {1} actions took {0} ms. here is the action list:", new Object[] { took, arr.length });
                for (ContextAction a : arr) {
                    LOG.log(Level.INFO, "  {0}", a);
                }
            }
        }

        private void cleanup() {
            this.result.removeLookupListener(this);
        }
    }

    static class LookupRef extends WeakReference<Lookup> {

        private final int hashCode;

        public LookupRef(Lookup referent) {
            super(referent);
            hashCode = System.identityHashCode(referent);
        }

        @Override
        public boolean equals(Object obj) {
            if (obj instanceof LookupRef) {
                LookupRef lr = (LookupRef) obj;
                return get() == lr.get();
            }
            return false;
        }

        @Override
        public int hashCode() {
            return hashCode;
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
