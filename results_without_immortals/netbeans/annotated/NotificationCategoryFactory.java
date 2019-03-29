/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 2013 Oracle and/or its affiliates. All rights reserved.
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
 * Portions Copyrighted 2013 Sun Microsystems, Inc.
 */
package org.openide.awt;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.ResourceBundle;
import org.openide.awt.NotificationDisplayer.Category;
import org.openide.util.Lookup;
import org.openide.util.LookupEvent;
import org.openide.util.LookupListener;
import org.openide.util.NbBundle;
import org.openide.util.lookup.Lookups;
import java.io.*;

/**
 * @author jpeska
 */
class NotificationCategoryFactory {

    // NOI18N
    static final String ATTR_CATEGORY_NAME = "categoryName";

    // NOI18N
    static final String ATTR_BUNDLE_NAME = "localizingBundle";

    // NOI18N
    static final String ATTR_DISPLAY_NAME_KEY = "diplayNameKey";

    // NOI18N
    static final String ATTR_DESCRIPTION_KEY = "descriptionKey";

    // NOI18N
    private static final String CATEGORY_LIST_PATH = "Notification/Category";

    private static NotificationCategoryFactory theInstance;

    private Lookup.Result<Category> lookupRes;

    private Map<String, Category> name2category;

    private List<Category> categories;

    private NotificationCategoryFactory() {
    }

    static Category create(Map<String, String> attrs) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b9cb2dd0-544c-4218-9270-ac6ecf13aa0c");
        String categoryName = attrs.get(ATTR_CATEGORY_NAME);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "538f4176-8e8f-46e4-b6ff-2279198cefe9");
        String bundleName = attrs.get(ATTR_BUNDLE_NAME);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6f911072-22b0-4373-bb26-de0ce715b742");
        String displayNameKey = attrs.get(ATTR_DISPLAY_NAME_KEY);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e6a88753-653b-49f4-8e2b-c4514dc3023e");
        String descriptionKey = attrs.get(ATTR_DESCRIPTION_KEY);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c6261c92-1c00-455f-b45e-92f9ab464c9e");
        return create(categoryName, bundleName, displayNameKey, descriptionKey);
    }

    static Category create(String categoryName, String bundleName, String displayNameKey, String descriptionKey) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "828a4d13-c223-459f-ac3a-7f9c75b04b3a");
        ResourceBundle bundle = NbBundle.getBundle(bundleName);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a28a0076-2e25-4c30-babc-2f939545182f");
        String displayName = bundle.getString(displayNameKey);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2c4c7530-590b-40e8-9f6b-1b2885618b61");
        String description = bundle.getString(descriptionKey);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "37206c20-af37-48ae-9f9e-afeefcab341c");
        return new Category(categoryName, displayName, description);
    }

    /**
     * @return The one and only instance of this class.
     */
    public static NotificationCategoryFactory getInstance() {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "82d5e556-502d-438d-961a-d4d93ea50f59");
        if (null == theInstance) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f4511cb1-6462-4d4c-b67f-615d773c529b");
            theInstance = new NotificationCategoryFactory();
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6b2d6899-36fb-41b6-8027-40d907f603a1");
        return theInstance;
    }

    Category getCategory(String categoryName) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d8185da7-6f44-425e-a061-fba66d815454");
        synchronized (this) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "541a626c-7939-44ca-bd73-ff4ef47e7072");
            initCategories();
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c7391af2-30b6-48ac-9fb0-9e526bf4aa30");
            return name2category.get(categoryName);
        }
    }

    List<Category> getCategories() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "85293d9f-1293-4676-8c93-ca8f060e896f");
        synchronized (this) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d6be385d-8b4a-4914-8b5d-acd053176e59");
            initCategories();
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "52926b36-78c8-4002-ab3f-f71ad6e5efd5");
            return categories;
        }
    }

    private void initCategories() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f1c635aa-a174-4088-b3ad-5ff2ff1c7b0e");
        synchronized (this) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3c06d675-60f9-4ea7-8d24-8dbf3cb08055");
            if (null == name2category) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5782d8e9-a857-4135-ba9e-874c4a5fc5d1");
                if (null == lookupRes) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "81d63e76-8289-47c9-9af4-470377856a1f");
                    lookupRes = initLookup();
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "57dabef3-c3ea-4be0-9652-2beef4b2e344");
                    lookupRes.addLookupListener(new LookupListener() {

                        @Override
                        public void resultChanged(LookupEvent ev) {
                            synchronized (NotificationCategoryFactory.this) {
                                name2category = null;
                                categories = null;
                            }
                        }
                    });
                }
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5282f249-2293-4079-9605-066766862d5b");
                int index = 0;
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4daff965-3f91-4b2c-8302-f9848a536a6b");
                categories = new ArrayList<Category>(Category.getDefaultCategories());
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4743e8c8-291e-418e-a1ac-c4284077a008");
                categories.addAll(lookupRes.allInstances());
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6b7dabe8-5d97-4ac4-9360-ae299e32b37f");
                name2category = new HashMap<String, Category>(categories.size());
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4393708c-3055-4595-897d-4ca5f9c81c9e");
                for (Category c : categories) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4962e24c-846b-454b-b7bc-c836d13884ac");
                    name2category.put(c.getName(), c);
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ae356616-4b6e-4c26-a876-5b7a4bd4370a");
                    c.setIndex(index++);
                }
            }
        }
    }

    private Lookup.Result<Category> initLookup() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "177e7540-e90f-48b5-ac9f-3ab23880527a");
        Lookup lkp = Lookups.forPath(CATEGORY_LIST_PATH);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2d89dceb-18ae-4959-886a-7bb7f79c1f54");
        Lookup.Template<Category> template = new Lookup.Template<Category>(Category.class);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "66830816-6c4d-4ec1-be19-67440b579bd8");
        Lookup.Result<Category> res = lkp.lookup(template);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4a6b30e3-10ba-4800-82a4-b9516f00d96d");
        return res;
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
