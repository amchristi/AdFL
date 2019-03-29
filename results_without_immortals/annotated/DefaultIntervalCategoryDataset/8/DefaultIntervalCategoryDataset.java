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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "4c5c92e3-6ee3-4416-8d0e-2fc85e366280");
        int result = 0;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "eb6c65f5-bf5c-4bf6-9cdc-5f1eb9adeb69");
        if (this.startData != null) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "af84a38b-e438-4310-be4c-9a3524d09caa");
            result = this.startData.length;
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "2e5fa818-2b59-4cff-9b2e-6684637f2ac1");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "e92a3f66-2e2e-4744-8ce9-84c348a0b6e8");
        int result = -1;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "2b3c4864-64de-40a4-bdf7-1ac534e45d4f");
        for (int i = 0; i < this.seriesKeys.length; i++) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "65358341-32cb-4084-90f2-05822b1d9ea1");
            if (seriesKey.equals(this.seriesKeys[i])) {
                writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "4783fd63-7e3b-48e1-b5ff-be57b01f3341");
                result = i;
                writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "9f65f284-fcf7-4a40-8ef5-9e8ec0d49c36");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "f1e5fc68-4e62-4f80-a8f9-95735ca53b08");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "4d4b321b-74a7-49e9-a75e-ceea74302a96");
        if ((series >= getSeriesCount()) || (series < 0)) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "6d2ca0e5-922a-4396-acfc-21038630f665");
            throw new IllegalArgumentException("No such series : " + series);
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "b3f2619e-dcce-4608-b0a1-046949a3d474");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "99a013e5-ab8d-484f-9219-eabcd7fbe8b5");
        Args.nullNotPermitted(seriesKeys, "seriesKeys");
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "ba918c21-dfcd-4de6-b05e-3765e5a943b6");
        if (seriesKeys.length != getSeriesCount()) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "f302f806-e96d-4adb-9d83-583b5e164725");
            throw new IllegalArgumentException("The number of series keys does not match the data.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "177e9f98-25aa-4ea5-9424-b5eac0f11e82");
        this.seriesKeys = seriesKeys;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "1a4d6e03-7ff6-40ba-a51b-6bcfd72b218d");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "0acc0a59-1e13-4f04-8cc0-3139671f0480");
        int result = 0;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "dde81ab2-7307-48f8-9ff1-b6dc15dc889c");
        if (this.startData != null) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "ab33ec41-707b-418e-bfdf-ec81fe0abc81");
            if (getSeriesCount() > 0) {
                writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "e3ebab58-0cea-4ccb-8942-84d157313a54");
                result = this.startData[0].length;
            }
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "5b289da8-1cc3-4db3-a544-06dbb9c31543");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "e699f81b-dc86-4f17-b487-a0c777a826ea");
        // we've stored them in an array...
        if (this.categoryKeys == null) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "e52b3137-ed4b-4cd7-ae17-73c39f51fbff");
            return new ArrayList();
        } else {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "1700ee3a-0eba-4ffc-9b3b-69c5f2e9a299");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "2c18624e-3dd0-4c52-b65b-24a35de84526");
        Args.nullNotPermitted(categoryKeys, "categoryKeys");
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "7a2e8d32-16df-4422-bf92-b1a9be93295d");
        if (categoryKeys.length != getCategoryCount()) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "93e7fc68-b40a-4de6-9807-3a6b2e53815a");
            throw new IllegalArgumentException("The number of categories does not match the data.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "6d7f1cce-6cc6-4b7d-83fb-7473dc323626");
        for (int i = 0; i < categoryKeys.length; i++) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "d333565b-e3db-43cc-b4c4-9105c4ec119b");
            if (categoryKeys[i] == null) {
                writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "f8780f93-3041-4043-95b8-7264b6d6898c");
                throw new IllegalArgumentException("DefaultIntervalCategoryDataset.setCategoryKeys(): " + "null category not permitted.");
            }
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "0596b868-e599-4ba0-9406-c9191a75d4d5");
        this.categoryKeys = categoryKeys;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "374987ce-18dc-4709-bb5a-e9fcac81952a");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "6233f3dc-4663-41d4-b961-266896ec3930");
        int seriesIndex = getSeriesIndex(series);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "625bdbd7-3d93-4577-9857-a19b02e2235b");
        if (seriesIndex < 0) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "d03bbe5a-5b7e-495c-944b-785a90e95d47");
            throw new UnknownKeyException("Unknown 'series' key.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "52539f70-b1e2-4315-aa7d-a2e0e591cea6");
        int itemIndex = getColumnIndex(category);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "b010f19a-a41e-4580-b225-51b77a9abcc7");
        if (itemIndex < 0) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "cb78fa9b-94ec-4578-8107-a80e71c406a6");
            throw new UnknownKeyException("Unknown 'category' key.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "0bf78503-ed3f-48f5-a7a4-1ac30db52225");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "f594f4a3-75d1-449f-a6a8-5005f075b70a");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "2a2dacf4-43cd-44b1-8897-92e82ab345b7");
        int seriesIndex = getSeriesIndex(series);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "f0635585-5ea1-47a0-b928-2b1f05dea72b");
        if (seriesIndex < 0) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "e048bbc4-8184-434d-b8fd-dae84e339c3b");
            throw new UnknownKeyException("Unknown 'series' key.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "599dd2f9-6dd8-4baa-be76-3a69c14750f5");
        int itemIndex = getColumnIndex(category);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "115510a0-dab8-4992-a10a-bd313abc231d");
        if (itemIndex < 0) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "8262d7e7-7aa6-47da-a3eb-13c452aa465b");
            throw new UnknownKeyException("Unknown 'category' key.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "dfe9de47-3aaa-4647-ab9d-53e01c7ec72d");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "22f23050-2601-41db-ab35-e20ffc0b796e");
        // check arguments...
        if ((series < 0) || (series >= getSeriesCount())) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "e4f45596-9d0f-4c76-881a-289db6e6197f");
            throw new IllegalArgumentException("DefaultIntervalCategoryDataset.getValue(): " + "series index out of range.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "a7747fa0-09a3-4153-b762-accfbd9de9aa");
        if ((category < 0) || (category >= getCategoryCount())) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "5473ab10-3ba0-477c-8cbe-94d508ff89f3");
            throw new IllegalArgumentException("DefaultIntervalCategoryDataset.getValue(): " + "category index out of range.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "6b4a89ee-9e15-4a59-8571-38bbc282a379");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "88dbd362-5f0d-4211-8f23-abfd446fbad4");
        int seriesIndex = getSeriesIndex(series);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "ad71ab97-8cc8-4443-a39d-fab93b0a7efa");
        if (seriesIndex < 0) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "b5f47703-dccd-479c-89d5-73298e8db99a");
            throw new UnknownKeyException("Unknown 'series' key.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "8a471479-c809-46c0-b069-ecbc754b8e7f");
        int itemIndex = getColumnIndex(category);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "ed4d4a5e-a76f-4151-9c07-61b371594a92");
        if (itemIndex < 0) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "592e55f6-9bb9-4bda-872a-c4f695aaaaa4");
            throw new UnknownKeyException("Unknown 'category' key.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "a0ff3883-eedf-44cc-b888-06035ac22c1e");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "9b4fc89d-6c6f-4472-9e0e-3a92e6f82d16");
        if ((series < 0) || (series >= getSeriesCount())) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "a3b8ee15-e0da-436b-a8e5-da85b906db25");
            throw new IllegalArgumentException("DefaultIntervalCategoryDataset.getValue(): " + "series index out of range.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "360a90f0-0c70-4507-84c3-fbd2890e7e53");
        if ((category < 0) || (category >= getCategoryCount())) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "e8bdcc07-916f-44b5-9bbe-88540a5244c9");
            throw new IllegalArgumentException("DefaultIntervalCategoryDataset.getValue(): " + "category index out of range.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "77629d39-eca3-49ad-bc2c-02e3e380475f");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "d59e662e-ea51-403c-a536-02096adeba1d");
        // does the series exist?
        if ((series < 0) || (series > getSeriesCount() - 1)) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "353af640-7d5c-4eb4-a7a2-e83d53dfb9d5");
            throw new IllegalArgumentException("DefaultIntervalCategoryDataset.setValue: " + "series outside valid range.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "44c6b18b-ad9c-462c-98e8-d45fbe3727a7");
        // is the category valid?
        int categoryIndex = getCategoryIndex(category);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "5057b583-13d5-4a4b-b808-bc327f98b67f");
        if (categoryIndex < 0) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "7d26d3e1-2afc-4b1a-98d0-926c7f7c2107");
            throw new IllegalArgumentException("DefaultIntervalCategoryDataset.setValue: " + "unrecognised category.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "06f02d18-76e3-472d-bb4b-a887768f0402");
        // update the data...
        this.startData[series][categoryIndex] = value;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "156f67b0-d860-4cb0-8f39-2adeea33526f");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "303a8811-8ce1-47b5-9fbd-e28dae9cf398");
        // does the series exist?
        if ((series < 0) || (series > getSeriesCount() - 1)) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "2f304efa-c67b-4e13-900c-6024c32582a0");
            throw new IllegalArgumentException("DefaultIntervalCategoryDataset.setValue: " + "series outside valid range.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "9d13222d-892b-48fe-93b5-a819b05d2e6f");
        // is the category valid?
        int categoryIndex = getCategoryIndex(category);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "c0a0ace2-2676-471f-90bc-a8b66a825370");
        if (categoryIndex < 0) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "00f31dea-957f-4c3c-b258-a7405a17fded");
            throw new IllegalArgumentException("DefaultIntervalCategoryDataset.setValue: " + "unrecognised category.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "ba139434-376c-4e8d-ac68-00f53ffce118");
        // update the data...
        this.endData[series][categoryIndex] = value;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "8c88e575-5221-4115-bfe3-49102e31ffa6");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "a9b8b9e7-2425-4b47-aa22-61d517985293");
        int result = -1;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "bf91df36-ce30-45db-9877-16e5a94616be");
        for (int i = 0; i < this.categoryKeys.length; i++) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "452faa1d-bfbc-45ec-b629-4ecb5ced6471");
            if (category.equals(this.categoryKeys[i])) {
                writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "537f729c-bd22-447c-83b0-13ffaec61b7c");
                result = i;
                writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "8c19d60b-0858-4129-a653-723171241c8c");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "978113db-2b85-4b13-ae3c-95b88717dc56");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "d22dc884-e05f-4edd-80d7-e65126bc0e5c");
        Comparable[] result = new Comparable[count];
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "70800e7b-5a5f-428a-95c0-01dae9a7d928");
        String name;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "77530c06-5577-42f6-a8f1-c3f5b46ae1e8");
        for (int i = 0; i < count; i++) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "4a1e3398-e400-4c60-8dc8-3a11ab4ff6e7");
            name = prefix + (i + 1);
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "51cd0d98-1474-48da-b1cd-ecb00af74a6f");
            result[i] = name;
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "c942b146-9aa8-419c-be6c-357d037b29a9");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "d2530b6b-12a8-4d77-aa5f-3803253b6bb2");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "b2404f74-dca4-43fd-a290-cf06705b10a7");
        Args.nullNotPermitted(columnKey, "columnKey");
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "8189bd18-b4b5-4c6e-9e67-db01feb1034c");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "cc91ba7e-a9dd-44ab-93ae-7959b3029bf1");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "e57dedc5-84a5-4ec9-af5c-bce1db5ecbda");
        // we've stored them in an array...
        if (this.seriesKeys == null) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "dcbb58d5-03df-4d9d-8d0e-a4bffc22b403");
            return new java.util.ArrayList();
        } else {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "63a0a2f9-9355-4020-a58d-b5dfea4d80da");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "f9220268-9dba-4f45-9e29-de52dee469da");
        if ((row >= getRowCount()) || (row < 0)) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "741a08b8-16f8-4647-bcc7-22f7f0990348");
            throw new IllegalArgumentException("The 'row' argument is out of bounds.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "dafd0480-3982-42fa-891c-6086ecf890e7");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "ce1a7b6c-640e-4237-9696-6b700ad515a5");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "f491744c-7da5-450a-bd85-7cc68c95e18d");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "fc9827ba-2f22-40d9-aab8-2372c04829bc");
        if (obj == this) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "ba954115-c907-4a70-a515-c3e8c75fa996");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "ba63b6ed-0554-4f41-8a29-83ec0a59ab6f");
        if (!(obj instanceof DefaultIntervalCategoryDataset)) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "876a1d53-5bd5-4d5b-8c4f-e677ed452906");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "c31a62c5-77f0-4e45-9019-0a1bb170f080");
        DefaultIntervalCategoryDataset that = (DefaultIntervalCategoryDataset) obj;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "cbe48fa4-e6e5-4105-b38d-c13addc6dd9c");
        if (!Arrays.equals(this.seriesKeys, that.seriesKeys)) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "9b4bb7cf-999d-4649-b8c9-7d39edde5459");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "2d778542-24f7-4f7a-8711-45864ca204ef");
        if (!Arrays.equals(this.categoryKeys, that.categoryKeys)) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "7c26fd3a-3a30-4b57-a918-e50951faf75c");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "8ed3d785-82b0-4bf9-a951-e206e24e5157");
        if (!equal(this.startData, that.startData)) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "5edd1d89-a966-4439-9ff9-9a6f813f953c");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "a50d8d03-a88f-46bd-8f97-bfced65ca3d1");
        if (!equal(this.endData, that.endData)) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "50125ee3-27ba-462c-92dc-c505e2fb5e45");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "3bd7b234-9207-411b-b909-439556ea3d8f");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "b3213c44-c88c-4a72-abdf-54d849867eea");
        DefaultIntervalCategoryDataset clone = (DefaultIntervalCategoryDataset) super.clone();
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "b098eca6-0d56-46ac-b22f-8d921fb033e6");
        clone.categoryKeys = (Comparable[]) this.categoryKeys.clone();
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "47e8d9bc-ef6c-482b-9b46-4e5edd998cd7");
        clone.seriesKeys = (Comparable[]) this.seriesKeys.clone();
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "0eb9baf3-a268-4103-be58-9e32f0f963da");
        clone.startData = clone(this.startData);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "156970b1-9903-46f2-98f8-f5e9deac4858");
        clone.endData = clone(this.endData);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "685bc459-caf0-4a10-9856-462048184033");
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
        writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "f267decc-9199-4823-a2de-ce220f2ee28a");
        if (array1 == null) {
            writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "e948016c-461e-42f6-8650-7ae94c136c82");
            return (array2 == null);
        }
        writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "7e12e3cb-66df-4f80-befd-835643fd6023");
        if (array2 == null) {
            writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "d354ef29-257f-487d-896d-562e04974aab");
            return false;
        }
        writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "6eeb41ec-9971-4adc-97fe-3fccb42c4f04");
        if (array1.length != array2.length) {
            writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "ed3d74b4-1259-4a9e-921b-c842a3b9dc20");
            return false;
        }
        writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "7bd95f71-c7d7-445d-b94b-72cc537a8262");
        for (int i = 0; i < array1.length; i++) {
            writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "096fb0d6-29a8-42ef-92c2-05d7a883279b");
            if (!Arrays.equals(array1[i], array2[i])) {
                writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "7fdfad39-3f6b-404e-bc07-43bf1c94d186");
                return false;
            }
        }
        writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "7d19312e-6a5a-4501-97ed-fe92932d5618");
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
        writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "41e1e53a-4e66-4997-a25d-2c20d4964ec3");
        Args.nullNotPermitted(array, "array");
        writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "3ae7ddc0-b79d-4b92-b86a-4742e6407e31");
        Number[][] result = new Number[array.length][];
        writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "8ca8f2a3-2f2c-4975-aa34-67db2ca6bd88");
        for (int i = 0; i < array.length; i++) {
            writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "9114478a-9bd3-4f31-9219-bc9815a6a11c");
            Number[] child = array[i];
            writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "475ad125-bdae-487c-902a-17424b80893f");
            Number[] copychild = new Number[child.length];
            writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "04e6b411-60fb-4e5c-b69b-dba39f76f07f");
            System.arraycopy(child, 0, copychild, 0, child.length);
            writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "902e55bf-4505-4718-af4c-f55f60ccaebb");
            result[i] = copychild;
        }
        writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_8_10.coverage", "c649463a-d4e4-41a8-b886-5f77c5905600");
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
