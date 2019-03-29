/* ===========================================================
 * JFreeChart : a free chart library for the Java(tm) platform
 * ===========================================================
 *
 * (C) Copyright 2000-2016, by Object Refinery Limited and Contributors.
 *
 * Project Info:  http://www.jfree.org/jfreechart/index.html
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
 * USA.
 *
 * [Oracle and Java are registered trademarks of Oracle and/or its affiliates. 
 * Other names may be trademarks of their respective owners.]
 *
 * -----------------------------------
 * DefaultIntervalCategoryDataset.java
 * -----------------------------------
 * (C) Copyright 2002-2016, by Jeremy Bowman and Contributors.
 *
 * Original Author:  Jeremy Bowman;
 * Contributor(s):   David Gilbert (for Object Refinery Limited);
 *
 * Changes
 * -------
 * 29-Apr-2002 : Version 1, contributed by Jeremy Bowman (DG);
 * 24-Oct-2002 : Amendments for changes made to the dataset interface (DG);
 * ------------- JFREECHART 1.0.x ---------------------------------------------
 * 08-Mar-2007 : Added equals() and clone() overrides (DG);
 * 25-Feb-2008 : Fix for the special case where the dataset is empty, see bug
 *               1897580 (DG)
 * 18-Dec-2008 : Use ResourceBundleWrapper - see patch 1607918 by
 *               Jess Thrysoee (DG);
 * 03-Jul-2013 : Use ParamChecks (DG);
 *
 */
package org.jfree.data.category;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.ResourceBundle;
import org.jfree.chart.util.Args;
import org.jfree.chart.util.ResourceBundleWrapper;
import org.jfree.data.DataUtils;
import org.jfree.data.UnknownKeyException;
import org.jfree.data.general.AbstractSeriesDataset;
import java.io.*;

/**
 * A convenience class that provides a default implementation of the
 * {@link IntervalCategoryDataset} interface.
 * <p>
 * The standard constructor accepts data in a two dimensional array where the
 * first dimension is the series, and the second dimension is the category.
 */
public class DefaultIntervalCategoryDataset extends AbstractSeriesDataset implements IntervalCategoryDataset {

    /**
     * The series keys.
     */
    private Comparable[] seriesKeys;

    /**
     * The category keys.
     */
    private Comparable[] categoryKeys;

    /**
     * Storage for the start value data.
     */
    private Number[][] startData;

    /**
     * Storage for the end value data.
     */
    private Number[][] endData;

    /**
     * Creates a new dataset using the specified data values and automatically
     * generated series and category keys.
     *
     * @param starts  the starting values for the intervals ({@code null}
     * not permitted).
     * @param ends  the ending values for the intervals ({@code null} not
     * permitted).
     */
    public DefaultIntervalCategoryDataset(double[][] starts, double[][] ends) {
        this(DataUtils.createNumberArray2D(starts), DataUtils.createNumberArray2D(ends));
    }

    /**
     * Constructs a dataset and populates it with data from the array.
     * <p>
     * The arrays are indexed as data[series][category].  Series and category
     * names are automatically generated - you can change them using the
     * {@link #setSeriesKeys(Comparable[])} and
     * {@link #setCategoryKeys(Comparable[])} methods.
     *
     * @param starts  the start values data.
     * @param ends  the end values data.
     */
    public DefaultIntervalCategoryDataset(Number[][] starts, Number[][] ends) {
        this(null, null, starts, ends);
    }

    /**
     * Constructs a DefaultIntervalCategoryDataset, populates it with data
     * from the arrays, and uses the supplied names for the series.
     * <p>
     * Category names are generated automatically ("Category 1", "Category 2",
     * etc).
     *
     * @param seriesNames  the series names (if {@code null}, series names
     * will be generated automatically).
     * @param starts  the start values data, indexed as data[series][category].
     * @param ends  the end values data, indexed as data[series][category].
     */
    public DefaultIntervalCategoryDataset(String[] seriesNames, Number[][] starts, Number[][] ends) {
        this(seriesNames, null, starts, ends);
    }

    /**
     * Constructs a DefaultIntervalCategoryDataset, populates it with data
     * from the arrays, and uses the supplied names for the series and the
     * supplied objects for the categories.
     *
     * @param seriesKeys  the series keys (if {@code null}, series keys
     * will be generated automatically).
     * @param categoryKeys  the category keys (if {@code null}, category
     * keys will be generated automatically).
     * @param starts  the start values data, indexed as data[series][category].
     * @param ends  the end values data, indexed as data[series][category].
     */
    public DefaultIntervalCategoryDataset(Comparable[] seriesKeys, Comparable[] categoryKeys, Number[][] starts, Number[][] ends) {
        this.startData = starts;
        this.endData = ends;
        if (starts != null && ends != null) {
            String baseName = "org.jfree.data.resources.DataPackageResources";
            ResourceBundle resources = ResourceBundleWrapper.getBundle(baseName);
            int seriesCount = starts.length;
            if (seriesCount != ends.length) {
                String errMsg = "DefaultIntervalCategoryDataset: the number " + "of series in the start value dataset does " + "not match the number of series in the end " + "value dataset.";
                throw new IllegalArgumentException(errMsg);
            }
            if (seriesCount > 0) {
                // set up the series names...
                if (seriesKeys != null) {
                    if (seriesKeys.length != seriesCount) {
                        throw new IllegalArgumentException("The number of series keys does not " + "match the number of series in the data.");
                    }
                    this.seriesKeys = seriesKeys;
                } else {
                    String prefix = resources.getString("series.default-prefix") + " ";
                    this.seriesKeys = generateKeys(seriesCount, prefix);
                }
                // set up the category names...
                int categoryCount = starts[0].length;
                if (categoryCount != ends[0].length) {
                    String errMsg = "DefaultIntervalCategoryDataset: the " + "number of categories in the start value " + "dataset does not match the number of " + "categories in the end value dataset.";
                    throw new IllegalArgumentException(errMsg);
                }
                if (categoryKeys != null) {
                    if (categoryKeys.length != categoryCount) {
                        throw new IllegalArgumentException("The number of category keys does not match " + "the number of categories in the data.");
                    }
                    this.categoryKeys = categoryKeys;
                } else {
                    String prefix = resources.getString("categories.default-prefix") + " ";
                    this.categoryKeys = generateKeys(categoryCount, prefix);
                }
            } else {
                this.seriesKeys = new Comparable[0];
                this.categoryKeys = new Comparable[0];
            }
        }
    }

    /**
     * Returns the number of series in the dataset (possibly zero).
     *
     * @return The number of series in the dataset.
     *
     * @see #getRowCount()
     * @see #getCategoryCount()
     */
    @Override
    public int getSeriesCount() {
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "3c64e4b1-dbfb-45d5-a904-4c03629c6e81");
        int result = 0;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "14c8ba79-07dd-495b-9206-253a4fd2a202");
        if (this.startData != null) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "a23ebe0f-0bde-4987-9d8f-64f96cf0e598");
            result = this.startData.length;
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "f00c76c1-6a57-483e-b18b-b7574bc650c4");
        return result;
    }

    /**
     * Returns a series index.
     *
     * @param seriesKey  the series key.
     *
     * @return The series index.
     *
     * @see #getRowIndex(Comparable)
     * @see #getSeriesKey(int)
     */
    public int getSeriesIndex(Comparable seriesKey) {
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "b8bd8ae5-2fe8-4603-81ba-c93d80fb4b34");
        int result = -1;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "21a878c7-3f86-492b-9185-9d258172cd0a");
        for (int i = 0; i < this.seriesKeys.length; i++) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "50ebcf81-fb5a-4a76-ac7c-9f6d7e7c9173");
            if (seriesKey.equals(this.seriesKeys[i])) {
                writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "fc8e08a2-8858-4d6a-86fa-7919fc41fbb8");
                result = i;
                writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "e55acc0f-4c12-47e9-a42f-0c0332164ac6");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "8ca9b73f-34b1-417d-9c8c-dcd9c3a65afa");
        return result;
    }

    /**
     * Returns the name of the specified series.
     *
     * @param series  the index of the required series (zero-based).
     *
     * @return The name of the specified series.
     *
     * @see #getSeriesIndex(Comparable)
     */
    @Override
    public Comparable getSeriesKey(int series) {
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "03fe8849-6ed6-4bb2-8399-b2e43b5e80bb");
        if ((series >= getSeriesCount()) || (series < 0)) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "0b41a0e4-d682-488e-89d1-b144806f3c7d");
            throw new IllegalArgumentException("No such series : " + series);
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "3d2b9435-f0db-4df0-bb50-719341b16bcf");
        return this.seriesKeys[series];
    }

    /**
     * Sets the names of the series in the dataset.
     *
     * @param seriesKeys  the new keys ({@code null} not permitted, the
     * length of the array must match the number of series in the
     * dataset).
     *
     * @see #setCategoryKeys(Comparable[])
     */
    public void setSeriesKeys(Comparable[] seriesKeys) {
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "daaf0343-67c8-49f3-a0ad-202fb8ab283f");
        Args.nullNotPermitted(seriesKeys, "seriesKeys");
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "c6fb99a7-d77d-428f-871c-f5c432a5da9a");
        if (seriesKeys.length != getSeriesCount()) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "c373dd38-30b9-47a7-80d4-8e8b8d34d8e3");
            throw new IllegalArgumentException("The number of series keys does not match the data.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "2912163f-70f3-4429-bac6-b62de98d7eef");
        this.seriesKeys = seriesKeys;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "ee9b6dd6-416f-4989-bf67-6be1fae4c62e");
        fireDatasetChanged();
    }

    /**
     * Returns the number of categories in the dataset.
     *
     * @return The number of categories in the dataset.
     *
     * @see #getColumnCount()
     */
    public int getCategoryCount() {
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "ad631b30-47f3-4d0a-9116-f63720f569a2");
        int result = 0;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "696ecd47-cbee-4644-bc96-00bbba366560");
        if (this.startData != null) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "a9ef3f51-9585-4529-a59a-5eacd02f900c");
            if (getSeriesCount() > 0) {
                writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "4634cb07-d0c0-4e68-a606-30c2968349ec");
                result = this.startData[0].length;
            }
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "6abfc1f1-1d96-4ddf-813d-6d076560df07");
        return result;
    }

    /**
     * Returns a list of the categories in the dataset.  This method supports
     * the {@link CategoryDataset} interface.
     *
     * @return A list of the categories in the dataset.
     *
     * @see #getRowKeys()
     */
    @Override
    public List getColumnKeys() {
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "5da05997-203e-42a8-a824-89e1f8650709");
        // we've stored them in an array...
        if (this.categoryKeys == null) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "9b14b2db-bb49-4440-a96b-8ed26733ae48");
            return new ArrayList();
        } else {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "57077c99-438a-4185-80df-02a1bf74a81d");
            return Collections.unmodifiableList(Arrays.asList(this.categoryKeys));
        }
    }

    /**
     * Sets the categories for the dataset.
     *
     * @param categoryKeys  an array of objects representing the categories in
     * the dataset.
     *
     * @see #getRowKeys()
     * @see #setSeriesKeys(Comparable[])
     */
    public void setCategoryKeys(Comparable[] categoryKeys) {
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "de7c25a6-233f-4a97-9457-3f891c5632a0");
        Args.nullNotPermitted(categoryKeys, "categoryKeys");
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "e4b15498-4126-431a-b4b7-fa7520e0d337");
        if (categoryKeys.length != getCategoryCount()) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "769be007-9f73-4d16-9ab4-f7faf1c1d740");
            throw new IllegalArgumentException("The number of categories does not match the data.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "bafed473-7f80-4940-982a-7ddbb05ec7aa");
        for (int i = 0; i < categoryKeys.length; i++) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "f13b77d0-27b0-403c-88f5-50f28e1f0b96");
            if (categoryKeys[i] == null) {
                writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "c1e0d747-6104-4efd-8beb-6b3599af889e");
                throw new IllegalArgumentException("DefaultIntervalCategoryDataset.setCategoryKeys(): " + "null category not permitted.");
            }
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "3fdcb578-ceb8-432c-a40e-57c405c34526");
        this.categoryKeys = categoryKeys;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "02474284-26b8-4cbd-a22e-3b1b8ac04331");
        fireDatasetChanged();
    }

    /**
     * Returns the data value for one category in a series.
     * <P>
     * This method is part of the CategoryDataset interface.  Not particularly
     * meaningful for this class...returns the end value.
     *
     * @param series    The required series (zero based index).
     * @param category  The required category.
     *
     * @return The data value for one category in a series (null possible).
     *
     * @see #getEndValue(Comparable, Comparable)
     */
    @Override
    public Number getValue(Comparable series, Comparable category) {
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "37dc7563-bd75-4103-9601-29b8538ea087");
        int seriesIndex = getSeriesIndex(series);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "772311fe-b580-4f74-8c28-47a298d0b108");
        if (seriesIndex < 0) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "555ebb7d-4d5b-4703-9d6f-865f754f344c");
            throw new UnknownKeyException("Unknown 'series' key.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "63c7a18c-48e0-483a-8d8b-db661931b595");
        int itemIndex = getColumnIndex(category);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "e5b322e8-ce94-4307-a59c-989735adc2fe");
        if (itemIndex < 0) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "6ac760e5-e873-4c77-839a-a19b1f339ba6");
            throw new UnknownKeyException("Unknown 'category' key.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "534f81c6-c174-4300-9608-53fa87b7db45");
        return getValue(seriesIndex, itemIndex);
    }

    /**
     * Returns the data value for one category in a series.
     * <P>
     * This method is part of the CategoryDataset interface.  Not particularly
     * meaningful for this class...returns the end value.
     *
     * @param series  the required series (zero based index).
     * @param category  the required category.
     *
     * @return The data value for one category in a series (null possible).
     *
     * @see #getEndValue(int, int)
     */
    @Override
    public Number getValue(int series, int category) {
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "13220fc5-2b87-4e71-916e-c13e02958a3b");
        return getEndValue(series, category);
    }

    /**
     * Returns the start data value for one category in a series.
     *
     * @param series  the required series.
     * @param category  the required category.
     *
     * @return The start data value for one category in a series
     * (possibly {@code null}).
     *
     * @see #getStartValue(int, int)
     */
    @Override
    public Number getStartValue(Comparable series, Comparable category) {
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "e60b6a3f-e308-4d4e-992d-d5bd946db7f1");
        int seriesIndex = getSeriesIndex(series);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "25f93b76-6267-4a1f-9155-803bb196ab38");
        if (seriesIndex < 0) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "61f31734-7c7c-4339-a57d-6f75cebc909b");
            throw new UnknownKeyException("Unknown 'series' key.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "89c72f68-8dcd-49ff-a3aa-af6b9a2a6930");
        int itemIndex = getColumnIndex(category);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "dcd261cb-f5df-47fd-a91f-adea555e5cf9");
        if (itemIndex < 0) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "3c641263-7336-4215-9e84-f0b228b6b5ce");
            throw new UnknownKeyException("Unknown 'category' key.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "9d2513ed-e84e-4354-a27e-40a795d8ed97");
        return getStartValue(seriesIndex, itemIndex);
    }

    /**
     * Returns the start data value for one category in a series.
     *
     * @param series  the required series (zero based index).
     * @param category  the required category.
     *
     * @return The start data value for one category in a series
     * (possibly {@code null}).
     *
     * @see #getStartValue(Comparable, Comparable)
     */
    @Override
    public Number getStartValue(int series, int category) {
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "37b1aca2-a83e-400d-a2cd-5d1c5af3c23d");
        // check arguments...
        if ((series < 0) || (series >= getSeriesCount())) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "7b152b86-7c3e-4f24-ba1b-1705550ef9c4");
            throw new IllegalArgumentException("DefaultIntervalCategoryDataset.getValue(): " + "series index out of range.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "80492178-f18e-4abc-973a-12113ded5c43");
        if ((category < 0) || (category >= getCategoryCount())) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "d402e168-92b2-4959-8bad-11b93cf63fa6");
            throw new IllegalArgumentException("DefaultIntervalCategoryDataset.getValue(): " + "category index out of range.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "13175ebb-4ee7-497e-9560-aea9298fa808");
        // fetch the value...
        return this.startData[series][category];
    }

    /**
     * Returns the end data value for one category in a series.
     *
     * @param series  the required series.
     * @param category  the required category.
     *
     * @return The end data value for one category in a series (null possible).
     *
     * @see #getEndValue(int, int)
     */
    @Override
    public Number getEndValue(Comparable series, Comparable category) {
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "c701ccac-cdec-470a-8ded-ba4ed74925fd");
        int seriesIndex = getSeriesIndex(series);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "6eee97fe-6868-49bc-bc36-b3651d0ff666");
        if (seriesIndex < 0) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "d4f928d4-e441-4958-b81d-7c9f7c40e1c3");
            throw new UnknownKeyException("Unknown 'series' key.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "d42f2a58-1ea9-4f86-b0d2-d01fac7dfdcf");
        int itemIndex = getColumnIndex(category);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "e8fc9601-ac85-4f43-8cab-28c3ada3be58");
        if (itemIndex < 0) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "f72ee3d8-39b3-42dc-a77e-9daa383427fe");
            throw new UnknownKeyException("Unknown 'category' key.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "eda7811c-3d92-4527-9b3b-f3015fdf8091");
        return getEndValue(seriesIndex, itemIndex);
    }

    /**
     * Returns the end data value for one category in a series.
     *
     * @param series  the required series (zero based index).
     * @param category  the required category.
     *
     * @return The end data value for one category in a series (null possible).
     *
     * @see #getEndValue(Comparable, Comparable)
     */
    @Override
    public Number getEndValue(int series, int category) {
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "ca54fe4f-be6d-4f64-bb05-4b0c27e10b45");
        if ((series < 0) || (series >= getSeriesCount())) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "074d41b1-43a6-4832-9752-6298a847ae88");
            throw new IllegalArgumentException("DefaultIntervalCategoryDataset.getValue(): " + "series index out of range.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "61d9c07e-644b-4945-a0ed-82b75d34d81e");
        if ((category < 0) || (category >= getCategoryCount())) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "05cfe7df-66bf-438b-9b49-6f6247e8d85a");
            throw new IllegalArgumentException("DefaultIntervalCategoryDataset.getValue(): " + "category index out of range.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "05a9ed65-93f6-4a3b-9d60-7667b3e74d1b");
        return this.endData[series][category];
    }

    /**
     * Sets the start data value for one category in a series.
     *
     * @param series  the series (zero-based index).
     * @param category  the category.
     *
     * @param value The value.
     *
     * @see #setEndValue(int, Comparable, Number)
     */
    public void setStartValue(int series, Comparable category, Number value) {
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "75b45005-5c0a-4999-9350-f2876cc5e9ad");
        // does the series exist?
        if ((series < 0) || (series > getSeriesCount() - 1)) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "14b025c2-58e1-43b1-bf82-74702ecb051d");
            throw new IllegalArgumentException("DefaultIntervalCategoryDataset.setValue: " + "series outside valid range.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "30a93aad-e2e7-4407-b86c-78761dcf4a7f");
        // is the category valid?
        int categoryIndex = getCategoryIndex(category);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "f59a0807-d5ed-411f-8593-1719b75e93e3");
        if (categoryIndex < 0) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "c9457e3e-d383-46ab-80fb-df1e6d2ae108");
            throw new IllegalArgumentException("DefaultIntervalCategoryDataset.setValue: " + "unrecognised category.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "33a5c981-07b2-4aea-9d64-c182f7484dc4");
        // update the data...
        this.startData[series][categoryIndex] = value;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "16eae9fd-da7e-475b-9f9d-da48bb7deedb");
        fireDatasetChanged();
    }

    /**
     * Sets the end data value for one category in a series.
     *
     * @param series  the series (zero-based index).
     * @param category  the category.
     *
     * @param value the value.
     *
     * @see #setStartValue(int, Comparable, Number)
     */
    public void setEndValue(int series, Comparable category, Number value) {
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "4079aa50-cb0e-4a6e-8448-1907c5a6d26a");
        // does the series exist?
        if ((series < 0) || (series > getSeriesCount() - 1)) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "31d2c29e-4aeb-4b98-8b6f-039288a610f2");
            throw new IllegalArgumentException("DefaultIntervalCategoryDataset.setValue: " + "series outside valid range.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "708c927c-6483-4267-9b32-7af94f851ce3");
        // is the category valid?
        int categoryIndex = getCategoryIndex(category);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "8628483e-d13b-4dff-8c2d-3f06797d3af2");
        if (categoryIndex < 0) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "269843de-e725-48f4-8af2-8c23401f27e5");
            throw new IllegalArgumentException("DefaultIntervalCategoryDataset.setValue: " + "unrecognised category.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "5c4b9065-1503-4f6c-b699-90b962302bfa");
        // update the data...
        this.endData[series][categoryIndex] = value;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "ec346219-6a04-441d-aa29-1783afd61e98");
        fireDatasetChanged();
    }

    /**
     * Returns the index for the given category.
     *
     * @param category  the category ({@code null} not permitted).
     *
     * @return The index.
     *
     * @see #getColumnIndex(Comparable)
     */
    public int getCategoryIndex(Comparable category) {
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "8b762ec7-a0ae-4876-a4cd-ff571d30ea8b");
        int result = -1;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "54e7a6d9-1e10-42c5-8d4c-11ef6a620f20");
        for (int i = 0; i < this.categoryKeys.length; i++) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "d49d5e83-0e38-42d1-9711-7bb191b6edd9");
            if (category.equals(this.categoryKeys[i])) {
                writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "002ab962-82b1-44a5-82af-cc6e32b991de");
                result = i;
                writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "0bf96383-da62-427a-a95b-414bbe191071");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "655aecb4-9aff-490d-bade-c5f18fe945c4");
        return result;
    }

    /**
     * Generates an array of keys, by appending a space plus an integer
     * (starting with 1) to the supplied prefix string.
     *
     * @param count  the number of keys required.
     * @param prefix  the name prefix.
     *
     * @return An array of <i>prefixN</i> with N = { 1 .. count}.
     */
    private Comparable[] generateKeys(int count, String prefix) {
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "d6869d05-e33f-42f9-b525-bff7016dc951");
        Comparable[] result = new Comparable[count];
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "731e3864-6e39-4c9f-a5e9-fecc597aa4c0");
        String name;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "a6d26886-6788-46b8-9a8d-82494f847701");
        for (int i = 0; i < count; i++) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "6c3767e6-9500-4712-a8e5-3b0a6da19000");
            name = prefix + (i + 1);
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "5eec34d8-5388-4457-9fcd-337530755411");
            result[i] = name;
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "f04d1998-f3f9-4911-9baa-82ad7c353a25");
        return result;
    }

    /**
     * Returns a column key.
     *
     * @param column  the column index.
     *
     * @return The column key.
     *
     * @see #getRowKey(int)
     */
    @Override
    public Comparable getColumnKey(int column) {
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "28a6c857-2b4b-4568-bcd3-f066da904ba3");
        return this.categoryKeys[column];
    }

    /**
     * Returns a column index.
     *
     * @param columnKey  the column key ({@code null} not permitted).
     *
     * @return The column index.
     *
     * @see #getCategoryIndex(Comparable)
     */
    @Override
    public int getColumnIndex(Comparable columnKey) {
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "0f8c75a0-a76c-47bf-9ce4-672f217335c5");
        Args.nullNotPermitted(columnKey, "columnKey");
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "793e3cc9-f909-40da-a05e-eae0591b9f4c");
        return getCategoryIndex(columnKey);
    }

    /**
     * Returns a row index.
     *
     * @param rowKey  the row key.
     *
     * @return The row index.
     *
     * @see #getSeriesIndex(Comparable)
     */
    @Override
    public int getRowIndex(Comparable rowKey) {
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "395877ba-f622-4df0-b1ad-99b28cb2a82a");
        return getSeriesIndex(rowKey);
    }

    /**
     * Returns a list of the series in the dataset.  This method supports the
     * {@link CategoryDataset} interface.
     *
     * @return A list of the series in the dataset.
     *
     * @see #getColumnKeys()
     */
    @Override
    public List getRowKeys() {
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "1078da28-34fb-4c37-80db-71e44ed7acb6");
        // we've stored them in an array...
        if (this.seriesKeys == null) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "eee4d8e4-37e5-4fac-8390-77862bb8a41b");
            return new java.util.ArrayList();
        } else {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "e683d3ba-47c9-40c6-97ef-07e4b3e3db4d");
            return Collections.unmodifiableList(Arrays.asList(this.seriesKeys));
        }
    }

    /**
     * Returns the name of the specified series.
     *
     * @param row  the index of the required row/series (zero-based).
     *
     * @return The name of the specified series.
     *
     * @see #getColumnKey(int)
     */
    @Override
    public Comparable getRowKey(int row) {
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "1c3eca0d-26f2-4dd7-ada8-c0aa2895e818");
        if ((row >= getRowCount()) || (row < 0)) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "3cfa3443-ce9d-4193-896b-7fa3a9667665");
            throw new IllegalArgumentException("The 'row' argument is out of bounds.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "6bdbcad8-f91b-4570-8525-3d3d9f89bfe6");
        return this.seriesKeys[row];
    }

    /**
     * Returns the number of categories in the dataset.  This method is part of
     * the {@link CategoryDataset} interface.
     *
     * @return The number of categories in the dataset.
     *
     * @see #getCategoryCount()
     * @see #getRowCount()
     */
    @Override
    public int getColumnCount() {
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "6c5daa73-e408-4a9b-a057-2d934f403f08");
        return this.categoryKeys.length;
    }

    /**
     * Returns the number of series in the dataset (possibly zero).
     *
     * @return The number of series in the dataset.
     *
     * @see #getSeriesCount()
     * @see #getColumnCount()
     */
    @Override
    public int getRowCount() {
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "e1f24955-c6ac-4c9c-9e17-e66ab151a159");
        return this.seriesKeys.length;
    }

    /**
     * Tests this dataset for equality with an arbitrary object.
     *
     * @param obj  the object ({@code null} permitted).
     *
     * @return A boolean.
     */
    @Override
    public boolean equals(Object obj) {
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "097283f0-89a7-4e1e-a491-0e94ba5b67d7");
        if (obj == this) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "5fc53f79-bc38-4379-b9ae-b50e9f66a6ba");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "258545f4-739b-488f-b5ce-e8453d46a79e");
        if (!(obj instanceof DefaultIntervalCategoryDataset)) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "eaafeb9c-65b0-4eb3-8323-905dbbc8cb69");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "cab6f5ed-b32d-439d-b99f-bd575b66425a");
        DefaultIntervalCategoryDataset that = (DefaultIntervalCategoryDataset) obj;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "f97945e4-42e1-486f-80fd-b9ebc00b0e56");
        if (!Arrays.equals(this.seriesKeys, that.seriesKeys)) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "0bf14cc2-47f3-4a29-b1c4-92630cdf52ed");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "df8bc974-72d7-4f71-b618-44785665d115");
        if (!Arrays.equals(this.categoryKeys, that.categoryKeys)) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "6eb41101-bcf9-42e2-a2b2-86c6968fb333");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "96348427-05d3-4fb6-a29f-166ed9ab8d6f");
        if (!equal(this.startData, that.startData)) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "d19a9dfe-c2e7-40e3-87fc-12b9a1da2603");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "ed050f0c-3c4a-43bd-961d-ab92198b265a");
        if (!equal(this.endData, that.endData)) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "fb09f0fa-0d99-4711-b322-b25ea7e39dd7");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "3a959c2b-5ebd-4235-af55-3130385948ed");
        // seem to be the same...
        return true;
    }

    /**
     * Returns a clone of this dataset.
     *
     * @return A clone.
     *
     * @throws CloneNotSupportedException if there is a problem cloning the
     * dataset.
     */
    @Override
    public Object clone() throws CloneNotSupportedException {
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "e9e19af5-3daf-47a6-bb9a-3780dda36dfb");
        DefaultIntervalCategoryDataset clone = (DefaultIntervalCategoryDataset) super.clone();
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "2c245a57-0893-4189-89cc-11fd3d105f25");
        clone.categoryKeys = (Comparable[]) this.categoryKeys.clone();
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "5d2b9260-07ed-4315-b0a1-273d887b7b25");
        clone.seriesKeys = (Comparable[]) this.seriesKeys.clone();
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "0a75db4b-6f22-474f-b4da-97804315bf4b");
        clone.startData = clone(this.startData);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "3ac0e261-1e53-4edc-a30e-ca31a35e61ea");
        clone.endData = clone(this.endData);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "6c1b7d7b-958c-47ac-9c33-1e091371b36d");
        return clone;
    }

    /**
     * Tests two double[][] arrays for equality.
     *
     * @param array1  the first array ({@code null} permitted).
     * @param array2  the second arrray ({@code null} permitted).
     *
     * @return A boolean.
     */
    private static boolean equal(Number[][] array1, Number[][] array2) {
        writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "7310f6e7-7c11-4b24-a028-cc922949160f");
        if (array1 == null) {
            writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "ec0f6c63-0e09-4255-96c4-f7c8ba12f617");
            return (array2 == null);
        }
        writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "5beda0d1-0571-4366-96ff-fac9182c7529");
        if (array2 == null) {
            writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "61398b1d-acbc-4d2f-be87-2274f6a66cc8");
            return false;
        }
        writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "f124667a-d0a5-40d3-b722-dc260c82b7af");
        if (array1.length != array2.length) {
            writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "11c771d6-6366-4ad9-98fa-03421503045f");
            return false;
        }
        writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "4df34c22-285c-4153-92e7-f9a324a37c71");
        for (int i = 0; i < array1.length; i++) {
            writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "28843691-6a2f-44d7-b5e5-e8f78d419005");
            if (!Arrays.equals(array1[i], array2[i])) {
                writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "447033b6-c16d-4cf2-a272-20e8d751e655");
                return false;
            }
        }
        writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "af68c9b7-6699-4b42-b11b-7974ab3d82de");
        return true;
    }

    /**
     * Clones a two dimensional array of {@code Number} objects.
     *
     * @param array  the array ({@code null} not permitted).
     *
     * @return A clone of the array.
     */
    private static Number[][] clone(Number[][] array) {
        writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "3b60964b-1948-4c94-be78-bcf4b51afba1");
        Args.nullNotPermitted(array, "array");
        writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "fdfd8f6a-7c15-47a6-b96b-373171b965c6");
        Number[][] result = new Number[array.length][];
        writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "e1979a12-a4a6-4497-b6d0-86d93f8fa380");
        for (int i = 0; i < array.length; i++) {
            writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "c550e311-ad76-4f09-9aeb-890899894aaa");
            Number[] child = array[i];
            writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "7e34a46a-2eea-46d6-a5b7-e38da8ff440d");
            Number[] copychild = new Number[child.length];
            writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "ec3f8c45-d609-40bd-bfe3-7067a954bd9c");
            System.arraycopy(child, 0, copychild, 0, child.length);
            writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "40319f13-3bc0-4321-a914-52da1e0d0dc8");
            result[i] = copychild;
        }
        writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_1_10.coverage", "453f4190-af7f-4b6b-99c1-645cb736220b");
        return result;
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
