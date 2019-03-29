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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "af16cca8-e40c-41fb-b075-77ba15ecaa65");
        int result = 0;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "243a34fa-18ea-40e5-afff-4ba4faad667c");
        if (this.startData != null) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "e25e824c-0259-4a81-94df-4fc6971a84ee");
            result = this.startData.length;
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "e5ccc5d2-b2c3-44fe-9a25-eb64ec2d7f60");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "a7d508d4-dee1-4665-b3be-f5fcfc1dba4d");
        int result = -1;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "dd8b5444-f51d-4802-99e9-db84c7d95be3");
        for (int i = 0; i < this.seriesKeys.length; i++) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "dd06ba79-c952-4ed1-a4ea-60d87992a380");
            if (seriesKey.equals(this.seriesKeys[i])) {
                writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "1bae6496-8538-4cbb-86a4-73bf6e966bb7");
                result = i;
                writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "06636027-5809-48f2-b9b1-4ac89bb96989");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "640247e9-62b1-4cc1-87f6-40ce04a4193d");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "7b59246c-b291-4b96-b66b-5adce1860fe6");
        if ((series >= getSeriesCount()) || (series < 0)) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "c9bd77d8-c515-48bb-9138-37a89460e0e3");
            throw new IllegalArgumentException("No such series : " + series);
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "3c19f741-8936-44cd-b5af-d7c763e5df5c");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "7f4c7cb7-715c-4350-934f-8b0b21514667");
        Args.nullNotPermitted(seriesKeys, "seriesKeys");
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "a1d06c14-2554-4abd-ae09-c4e6ab66d02c");
        if (seriesKeys.length != getSeriesCount()) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "f925f3bf-d6e5-4e8c-baae-e1725085a6db");
            throw new IllegalArgumentException("The number of series keys does not match the data.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "ad41c031-e609-4330-a4b1-ac3e445edf8e");
        this.seriesKeys = seriesKeys;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "10a4ff51-86aa-43d2-bdc3-6f063a77e580");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "b8314502-915b-40b1-82e7-ed168d2fd7b1");
        int result = 0;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "fe57d9ba-b1ad-48a1-833c-307d915ac914");
        if (this.startData != null) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "e9416d8a-ebf0-40bf-a2e1-f76131116f86");
            if (getSeriesCount() > 0) {
                writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "4bb1f99c-e4f4-48de-b0c4-0d2728198555");
                result = this.startData[0].length;
            }
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "e281376b-0005-4b7d-9952-5d31195f4b3c");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "46e6186f-cdaa-47ca-b3b1-62a817e32bf0");
        // we've stored them in an array...
        if (this.categoryKeys == null) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "0f6766a9-10f8-4219-9982-45b2ca707e89");
            return new ArrayList();
        } else {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "221c9b06-4b24-48e7-8fa2-47edd99e5d1d");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "dca0317b-170b-4945-a158-433aec0e3187");
        Args.nullNotPermitted(categoryKeys, "categoryKeys");
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "e3a1c941-c49e-47e1-b5a1-bc1d2d351ef9");
        if (categoryKeys.length != getCategoryCount()) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "a3c78039-5eac-4b85-bf87-101021db83a6");
            throw new IllegalArgumentException("The number of categories does not match the data.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "84ee9a75-9481-46f6-b47a-e4e7e74327b3");
        for (int i = 0; i < categoryKeys.length; i++) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "3b6b575a-eb11-4831-a456-d48d0bee531a");
            if (categoryKeys[i] == null) {
                writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "61c55cd5-484e-4c3a-bf72-bfee57d03c4b");
                throw new IllegalArgumentException("DefaultIntervalCategoryDataset.setCategoryKeys(): " + "null category not permitted.");
            }
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "1d1b6d95-6eff-4def-b81a-bee5c0150291");
        this.categoryKeys = categoryKeys;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "af0bd693-86d3-491f-bce6-f83e86f5535b");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "18110e2a-f06c-404c-9aee-120dfd7b262a");
        int seriesIndex = getSeriesIndex(series);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "d46bd6ec-2c27-4192-93c4-2c5b0296a93b");
        if (seriesIndex < 0) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "74ea9b8d-4050-4bcc-9f83-2fda6dc7ffc5");
            throw new UnknownKeyException("Unknown 'series' key.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "a82ce86f-241c-406d-9ff8-31c0b6ef877a");
        int itemIndex = getColumnIndex(category);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "dc3dcad0-cad1-4dc0-a13d-e1baf0d4a555");
        if (itemIndex < 0) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "a3d03d44-71b1-4756-ae80-f991851a638d");
            throw new UnknownKeyException("Unknown 'category' key.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "774e744b-263d-4c1f-81d9-130c33711cd4");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "59887214-c86d-4454-8ead-5b2f70024bc6");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "d05754d7-695d-4f96-8edb-47a96512e359");
        int seriesIndex = getSeriesIndex(series);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "8adb0912-1643-4376-b64e-11b91a8e39ae");
        if (seriesIndex < 0) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "5c19ba17-b64a-4a5c-bc80-29988b4da776");
            throw new UnknownKeyException("Unknown 'series' key.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "b71a538e-245b-413f-a3f4-5860b3c6da92");
        int itemIndex = getColumnIndex(category);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "7be4a075-955b-4e0e-b6b7-89e1a347b332");
        if (itemIndex < 0) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "7827ff6f-ca9b-4825-82fe-2b7edb96af03");
            throw new UnknownKeyException("Unknown 'category' key.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "3a3c34dc-7737-4ccf-9c93-cd71a0e6a29a");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "ace4135c-5e1e-4ca6-9ca4-87b33b81cc67");
        // check arguments...
        if ((series < 0) || (series >= getSeriesCount())) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "87d651ef-6aa7-4097-9e4f-af7dbb9d271c");
            throw new IllegalArgumentException("DefaultIntervalCategoryDataset.getValue(): " + "series index out of range.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "e59d045a-2874-4042-94b4-7af443e1e933");
        if ((category < 0) || (category >= getCategoryCount())) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "6fb861e6-dd7c-4ed8-9ec0-b46ca9b232f7");
            throw new IllegalArgumentException("DefaultIntervalCategoryDataset.getValue(): " + "category index out of range.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "f3837272-2ab9-490f-a9ce-8dbcfa00e84b");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "2e8f71b3-b897-4c45-b9f8-dcd8b74a2926");
        int seriesIndex = getSeriesIndex(series);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "e62a23c8-db5a-4666-972d-44eea2a59ec6");
        if (seriesIndex < 0) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "351f3aca-f602-4c9a-ad3c-d618f0466075");
            throw new UnknownKeyException("Unknown 'series' key.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "7a4eb5b6-11b7-4b4e-a8e2-ace69d0852cb");
        int itemIndex = getColumnIndex(category);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "9d0619de-300a-49d8-8e41-2de4bd46ce41");
        if (itemIndex < 0) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "bdf4bcd7-3291-4417-87c1-b231000d53af");
            throw new UnknownKeyException("Unknown 'category' key.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "8f795272-34a0-424b-99f1-9de35f9321a5");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "82afc26f-7ef9-45d9-b37f-8d0e4e106641");
        if ((series < 0) || (series >= getSeriesCount())) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "8d37f71c-e561-401f-871d-bcb249bc8ae8");
            throw new IllegalArgumentException("DefaultIntervalCategoryDataset.getValue(): " + "series index out of range.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "930278ad-5b21-4e66-bd80-16eef79cffc3");
        if ((category < 0) || (category >= getCategoryCount())) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "63de8749-2524-4161-a728-22d25b3a0d5b");
            throw new IllegalArgumentException("DefaultIntervalCategoryDataset.getValue(): " + "category index out of range.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "814d2c4c-4eb6-4493-972d-04e6460ac55d");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "696ab6ac-f032-4724-85ec-353214c6b938");
        // does the series exist?
        if ((series < 0) || (series > getSeriesCount() - 1)) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "7aebc787-c959-4140-8292-0f87aa9177b2");
            throw new IllegalArgumentException("DefaultIntervalCategoryDataset.setValue: " + "series outside valid range.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "96231c44-374d-4461-b0b5-ce4cbebc502f");
        // is the category valid?
        int categoryIndex = getCategoryIndex(category);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "03dfb173-56cb-47cd-872f-81dac3f6bb06");
        if (categoryIndex < 0) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "abc3e540-027c-4c68-a556-95b73d4bfa58");
            throw new IllegalArgumentException("DefaultIntervalCategoryDataset.setValue: " + "unrecognised category.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "0e6f5674-b1e8-461f-aa0e-192538887b58");
        // update the data...
        this.startData[series][categoryIndex] = value;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "bc266c1c-236e-4ff3-b105-f190be8af7a9");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "201bbcd6-725d-466d-94f0-574681e0c29f");
        // does the series exist?
        if ((series < 0) || (series > getSeriesCount() - 1)) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "83a5ee97-7832-43cf-96a9-5dcab14917bf");
            throw new IllegalArgumentException("DefaultIntervalCategoryDataset.setValue: " + "series outside valid range.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "c5dd1d9b-c73d-4d80-a178-3d64403e8bf4");
        // is the category valid?
        int categoryIndex = getCategoryIndex(category);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "3d123ddb-0bdd-49a4-9fe3-efa79ac56df6");
        if (categoryIndex < 0) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "90ff7dfb-f460-4ea5-9c4c-39bbcfa7af49");
            throw new IllegalArgumentException("DefaultIntervalCategoryDataset.setValue: " + "unrecognised category.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "cfbabcbc-7d6b-44d9-aa4b-5079be255a72");
        // update the data...
        this.endData[series][categoryIndex] = value;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "74e67511-e6c4-41cb-a24b-b248c8ae360b");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "117eee99-d6b1-472e-bdcb-c0e2f7e87ddd");
        int result = -1;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "b83f672c-823a-41cd-bec9-3c1090afa2ed");
        for (int i = 0; i < this.categoryKeys.length; i++) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "d14bcd32-e493-4aad-88cb-eb47d6611c3e");
            if (category.equals(this.categoryKeys[i])) {
                writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "83d22e88-4857-43cf-80e4-5fd148f411a1");
                result = i;
                writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "e0bb3ba0-1546-4908-a7bd-5c0f92ab3d91");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "1777dd04-49e6-43f3-96c7-05b466890ec3");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "ecc1261a-57a0-4965-af11-8d85f16d8902");
        Comparable[] result = new Comparable[count];
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "39a58c42-978d-4bb1-a16b-cf150996dc68");
        String name;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "80b56958-40c7-41f5-be20-2c64df7ffdf8");
        for (int i = 0; i < count; i++) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "b16dcc78-6c4e-4e57-8a5d-df48113671c9");
            name = prefix + (i + 1);
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "3bf7d257-e784-47dc-8b41-5778f652814b");
            result[i] = name;
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "0ca6f88b-5f26-4fdd-ac03-e1a4c6c3f113");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "210969fb-1e53-4138-bb17-e8f7be90e135");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "c5b27c47-d99a-4c78-9da2-3ce257b79452");
        Args.nullNotPermitted(columnKey, "columnKey");
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "7b6ae25f-d35c-41c6-8cd4-8c3980a88d20");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "8a83c96e-0bec-4fcb-b6ab-f83018662864");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "1df54c33-db67-4211-b0ed-ffbb1206f63b");
        // we've stored them in an array...
        if (this.seriesKeys == null) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "64b95ff8-e547-4971-9171-3e0f3d4b125b");
            return new java.util.ArrayList();
        } else {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "44cff2c2-663c-4463-9a3f-bd70afe49ae3");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "263c5cbc-1b28-471e-8e15-44a167d64444");
        if ((row >= getRowCount()) || (row < 0)) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "374da5a5-b685-4d10-9957-ac207a837287");
            throw new IllegalArgumentException("The 'row' argument is out of bounds.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "aeb4a4ce-e466-4b1a-894a-6562fbfebcaa");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "fb0c1fc0-563e-4734-830c-46057aa5cfc1");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "d10eaec7-7aeb-418b-884f-00423e1bee0d");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "0ce8ebd9-1259-447a-8d58-49ea8417c9cc");
        if (obj == this) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "da2527ea-4894-4f4a-8983-3b17b415e615");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "e0a2a0e9-e36e-4bc5-b4f9-3758ae1b4df5");
        if (!(obj instanceof DefaultIntervalCategoryDataset)) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "68239a5a-5612-438f-848d-5ed05ee6a5b9");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "42299bac-d078-4b27-a39f-a8a8d2b76eae");
        DefaultIntervalCategoryDataset that = (DefaultIntervalCategoryDataset) obj;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "c6fdf789-b35d-4a15-8778-8f6fdb43431f");
        if (!Arrays.equals(this.seriesKeys, that.seriesKeys)) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "4762139b-76f9-4b41-bc10-9adfddbd4ea0");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "c5610299-eec0-4381-bba1-b83abea5d6da");
        if (!Arrays.equals(this.categoryKeys, that.categoryKeys)) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "d410e14d-87be-47d3-8234-3af2737c4f2d");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "5cb9e13f-cbcf-4617-9aca-27d0abee5545");
        if (!equal(this.startData, that.startData)) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "85e92b85-d148-412d-8741-27dab7d2a403");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "a63caf80-b490-4e14-baa3-b7fe07f9a6fa");
        if (!equal(this.endData, that.endData)) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "9068f6e3-1e87-4399-9dbe-69e12b42f729");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "3f59c277-ecd3-4522-916a-d1bcb8b15b5b");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "485dcbf3-399e-4682-b005-f4ebba2a6336");
        DefaultIntervalCategoryDataset clone = (DefaultIntervalCategoryDataset) super.clone();
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "a59d352b-03d2-4274-8211-839e8f30aa83");
        clone.categoryKeys = (Comparable[]) this.categoryKeys.clone();
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "63c23c64-ce2d-4ce2-89ff-d750491c19c0");
        clone.seriesKeys = (Comparable[]) this.seriesKeys.clone();
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "2cf20e15-3796-4eed-a6c2-cf6ff1f010ac");
        clone.startData = clone(this.startData);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "1825cad4-d95d-42db-9bf3-0812a9ba4d53");
        clone.endData = clone(this.endData);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "dd21b26b-5334-4ad6-8ed3-b24d71f81d48");
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
        writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "da2bba1f-df70-421f-9735-19832b0937dc");
        if (array1 == null) {
            writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "37063c06-3a5f-4f6c-82b6-0b845c64bc66");
            return (array2 == null);
        }
        writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "6da700c8-e2dd-4a3b-8ad8-18fb4134d037");
        if (array2 == null) {
            writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "5a8f336e-2e60-498c-ba29-e0b9f5718588");
            return false;
        }
        writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "538c3d68-f8a0-47ab-8936-aa8ed8051a6d");
        if (array1.length != array2.length) {
            writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "41181b2d-7175-40b9-bd98-86ed3448fdc2");
            return false;
        }
        writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "942fa1ac-1ffe-4d77-adad-12cfd436353a");
        for (int i = 0; i < array1.length; i++) {
            writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "6429b140-a5e4-48b5-bc97-c794cfff3135");
            if (!Arrays.equals(array1[i], array2[i])) {
                writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "42c43f40-87ea-45be-af11-a8e5e06e02a3");
                return false;
            }
        }
        writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "7e5b181d-ae91-49e5-be46-880bf3ff1d69");
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
        writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "3b5ce58e-5a07-4b0a-b00b-1ae14a1f8dd4");
        Args.nullNotPermitted(array, "array");
        writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "460d6429-7d3f-4b08-8862-bc6ccf9ab6fd");
        Number[][] result = new Number[array.length][];
        writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "4d8aa3c8-de0e-4729-add0-0eb970b6dbe7");
        for (int i = 0; i < array.length; i++) {
            writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "8f8af166-9e4b-4511-afcd-29e11f57886a");
            Number[] child = array[i];
            writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "33496bd0-7ffb-4592-9214-0d37b2ef7758");
            Number[] copychild = new Number[child.length];
            writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "af4af053-67e1-42da-a409-8a7507d7816d");
            System.arraycopy(child, 0, copychild, 0, child.length);
            writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "5287a238-b583-474e-8833-47d1b70a70b0");
            result[i] = copychild;
        }
        writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_10_10.coverage", "6efd0efa-5523-4cfb-a664-6756d55bbba3");
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
