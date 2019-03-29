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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "15cddacc-c65f-4112-b560-a88b89e9b0f4");
        int result = 0;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "a00ef2ca-61d4-4241-96d8-69a75f2c17cd");
        if (this.startData != null) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "511afe94-49da-4a60-b790-5e09c1ef0d02");
            result = this.startData.length;
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "a061157a-00af-42fe-ab44-219db8c5ee85");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "95450ced-2cd6-4957-955f-69f98dff08a8");
        int result = -1;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "c383c217-903b-4538-a3e9-e153a6b8286b");
        for (int i = 0; i < this.seriesKeys.length; i++) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "8eb35a10-e2ee-4624-9b95-38cffc3a7c3f");
            if (seriesKey.equals(this.seriesKeys[i])) {
                writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "d11dcfd1-7179-4622-96cf-7f0899cf594b");
                result = i;
                writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "0d417586-1005-4796-a4bb-67a91957d2e3");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "21c6ad0a-a602-4be7-b738-73de3c94f0f2");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "55e834f2-ca3f-40dc-8797-7e4c8e126835");
        if ((series >= getSeriesCount()) || (series < 0)) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "0b3804aa-1779-4942-93d9-891553c8dfd4");
            throw new IllegalArgumentException("No such series : " + series);
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "36aad16d-df11-4d9c-817b-0353b0ead217");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "33f9f8a0-4fbb-4329-ad4c-8bc70c27f9f9");
        Args.nullNotPermitted(seriesKeys, "seriesKeys");
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "0d0ce949-7b24-4aa8-8427-ad9fecf44e75");
        if (seriesKeys.length != getSeriesCount()) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "076c79a2-7ac4-441a-917f-67706afdf35c");
            throw new IllegalArgumentException("The number of series keys does not match the data.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "6900d020-0ae0-48f5-a61c-e0ac2b1d341b");
        this.seriesKeys = seriesKeys;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "e2d26198-f8ab-4b3e-8674-b98bbeaeaaee");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "b322d328-4518-4ccf-a844-fee934fb0148");
        int result = 0;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "362bdd33-0cca-4177-b4a5-5bf1c2237e47");
        if (this.startData != null) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "045fc48e-e46f-4756-b48f-b9a384afe027");
            if (getSeriesCount() > 0) {
                writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "d5c1ae1b-13d2-4f0e-aaa9-6370ae19c864");
                result = this.startData[0].length;
            }
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "afdbdc48-cdd5-48aa-94f2-7cea34730e7e");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "94212c45-a0c0-414a-bab3-c6cc733084c9");
        // we've stored them in an array...
        if (this.categoryKeys == null) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "e9748d42-58f9-4a45-802d-1a2ab4eb4086");
            return new ArrayList();
        } else {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "497d742e-287a-4cea-a749-42d0963244a9");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "ddde753a-beb3-45ec-b0be-96bf5ff040b4");
        Args.nullNotPermitted(categoryKeys, "categoryKeys");
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "37234a9b-6dec-4d96-ac65-3e7461db1ab5");
        if (categoryKeys.length != getCategoryCount()) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "69aac053-784d-4382-8a9b-fd72538da011");
            throw new IllegalArgumentException("The number of categories does not match the data.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "a4b4c9b5-0c44-445f-96de-5e6feabe0889");
        for (int i = 0; i < categoryKeys.length; i++) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "c77675cf-0cf9-45d9-b1a8-41fb3a55060d");
            if (categoryKeys[i] == null) {
                writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "19846842-d0ed-472d-81d2-fb20987165a3");
                throw new IllegalArgumentException("DefaultIntervalCategoryDataset.setCategoryKeys(): " + "null category not permitted.");
            }
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "d2030f58-0456-4b63-a10e-5bb1ea24bd04");
        this.categoryKeys = categoryKeys;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "14183b9f-6faa-42f1-bdc8-7fde3570ec3a");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "580bd8f9-2f2d-4a8c-a349-a2c54241347d");
        int seriesIndex = getSeriesIndex(series);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "ddd433bc-e64b-4947-8bc6-58c4e05bc560");
        if (seriesIndex < 0) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "34102def-8f9a-4d5e-8435-010b07bd9d84");
            throw new UnknownKeyException("Unknown 'series' key.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "d8560e2c-e853-4e27-af26-e4d043644de3");
        int itemIndex = getColumnIndex(category);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "098c2c7f-79f0-4224-bf40-763cbfaadc8f");
        if (itemIndex < 0) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "f2afb9af-011f-49cf-bbf8-ae91021f544d");
            throw new UnknownKeyException("Unknown 'category' key.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "4ea2b6ad-ac17-406c-bbce-0197c8eb5fa8");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "da7d1703-f9b7-40ad-b7e6-a1be817cbda7");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "26606426-13b1-4e66-9c44-f223d5c7e5c4");
        int seriesIndex = getSeriesIndex(series);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "13b2399a-4925-4c96-9b11-7d7f8c8751d5");
        if (seriesIndex < 0) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "c6bc7eed-2b68-4d03-aca8-430f79e8fbae");
            throw new UnknownKeyException("Unknown 'series' key.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "a74c27a6-c8ff-4135-8569-ae844034c33b");
        int itemIndex = getColumnIndex(category);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "9684b22b-9e23-4548-b870-b02403153713");
        if (itemIndex < 0) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "44726f8e-e404-47e3-a6f2-b6e7a5d18060");
            throw new UnknownKeyException("Unknown 'category' key.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "8a565055-792d-47e4-bfe7-d8b7aedff548");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "fcee2837-737a-41fe-83f4-544efa704cf5");
        // check arguments...
        if ((series < 0) || (series >= getSeriesCount())) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "1fc214be-8422-4c94-b92c-8ab03f15bdcc");
            throw new IllegalArgumentException("DefaultIntervalCategoryDataset.getValue(): " + "series index out of range.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "701bfee3-bdf5-458b-b2f2-0134c60ccc36");
        if ((category < 0) || (category >= getCategoryCount())) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "3839c2b7-b55c-4983-b68c-20dc821e35a9");
            throw new IllegalArgumentException("DefaultIntervalCategoryDataset.getValue(): " + "category index out of range.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "8598863f-2f29-435f-89a3-1369217dafa7");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "d7b5dc30-2a7c-4e9e-bf31-cd6560614ef8");
        int seriesIndex = getSeriesIndex(series);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "b4fcb85e-d60d-4258-93e5-b9a0668061c5");
        if (seriesIndex < 0) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "fbc15822-13c8-4a70-938d-fe5617e81e79");
            throw new UnknownKeyException("Unknown 'series' key.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "31a2669d-1b10-4679-857d-9291bc0e756a");
        int itemIndex = getColumnIndex(category);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "71a446dc-ab7e-4a25-859d-82780a15ea1f");
        if (itemIndex < 0) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "d3d76cfe-c77e-4a66-9322-5eb3b28b341b");
            throw new UnknownKeyException("Unknown 'category' key.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "153f8dd7-943c-4cdd-95e7-3c037f4699e0");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "9e687679-86ac-4043-b8f3-090d77d77782");
        if ((series < 0) || (series >= getSeriesCount())) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "8a5a2718-558e-41db-adcc-37cfeb244721");
            throw new IllegalArgumentException("DefaultIntervalCategoryDataset.getValue(): " + "series index out of range.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "dac4a5e9-94ff-4cee-9427-a3a953ff8d2c");
        if ((category < 0) || (category >= getCategoryCount())) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "dc6a5b86-0e10-47a6-8cc4-c0bb008ec2b3");
            throw new IllegalArgumentException("DefaultIntervalCategoryDataset.getValue(): " + "category index out of range.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "b340b928-6de1-49ee-b359-2fb4d6e6ab5c");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "c4530063-3f4e-4c8b-a90f-4b96df78ddcd");
        // does the series exist?
        if ((series < 0) || (series > getSeriesCount() - 1)) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "ab10377b-b4b1-48f4-a7e1-2f28e329ba06");
            throw new IllegalArgumentException("DefaultIntervalCategoryDataset.setValue: " + "series outside valid range.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "c0273369-1b22-4a82-9b78-55439c007993");
        // is the category valid?
        int categoryIndex = getCategoryIndex(category);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "2aff365f-fc3c-41b9-a53d-72e8e4f1e2b5");
        if (categoryIndex < 0) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "c7309fda-7eb7-413f-b466-185375a631d7");
            throw new IllegalArgumentException("DefaultIntervalCategoryDataset.setValue: " + "unrecognised category.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "34a99cce-8c22-4ffc-8346-19c7901ae3db");
        // update the data...
        this.startData[series][categoryIndex] = value;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "36725fe3-32f4-43da-8dfd-7ebfface36a8");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "e49419c2-2675-4b0a-84b2-b222b07afe6b");
        // does the series exist?
        if ((series < 0) || (series > getSeriesCount() - 1)) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "7b97c0bb-f7c6-485a-a6a0-fd479069f124");
            throw new IllegalArgumentException("DefaultIntervalCategoryDataset.setValue: " + "series outside valid range.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "585c4651-4394-4ec0-b1cb-84fedb154dc6");
        // is the category valid?
        int categoryIndex = getCategoryIndex(category);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "23f2725e-509a-4cb4-b773-69a97591f8eb");
        if (categoryIndex < 0) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "55b304e9-c406-4953-816b-d07930e614e9");
            throw new IllegalArgumentException("DefaultIntervalCategoryDataset.setValue: " + "unrecognised category.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "d3e8e0f5-d196-4106-b0e2-24a9d13ae7c3");
        // update the data...
        this.endData[series][categoryIndex] = value;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "16406a92-539a-47b1-888a-8f270cf5769f");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "a1cda902-500f-417b-8f2c-dbdadc4e9cf4");
        int result = -1;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "994ecc53-826a-4e78-83bd-57a6ea81f436");
        for (int i = 0; i < this.categoryKeys.length; i++) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "2438ab34-3a96-4082-a471-f56d158273c4");
            if (category.equals(this.categoryKeys[i])) {
                writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "2190fdbf-1b78-41f8-b0af-02087e703747");
                result = i;
                writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "46808d04-5f69-4a19-843f-6574356d24f0");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "68bf887d-61a9-4b01-9050-dde43780c2bf");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "690eeeaf-c6e4-48d7-b6bd-c1ffd56c1e45");
        Comparable[] result = new Comparable[count];
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "006ccf1f-890d-491c-a7d9-e5530559287e");
        String name;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "94e8b8f0-3671-40df-bf15-5af3286c93a3");
        for (int i = 0; i < count; i++) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "cf21ffea-80e8-4b4a-ac31-722eb66bbf90");
            name = prefix + (i + 1);
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "ce933f28-190a-425b-bc7a-259995df8355");
            result[i] = name;
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "8572217e-307f-4181-a731-7f3fbc591735");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "d12f1852-9e11-49d6-a5f6-c5ce5ae8bfff");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "5571e52e-38ff-4022-a265-ed43f0d07c0a");
        Args.nullNotPermitted(columnKey, "columnKey");
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "84e9fab0-6187-473f-9741-fcf3d1525d8f");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "bdbfa649-c65e-42aa-86fd-fa9a3e982511");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "e6704472-4de1-4c82-a9c0-3f12a21c579c");
        // we've stored them in an array...
        if (this.seriesKeys == null) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "0782e97a-57e4-4463-8af4-2a6ce12ad944");
            return new java.util.ArrayList();
        } else {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "4eb197f4-f822-47aa-9b45-4f79abfd9345");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "35ae0947-22a6-4a65-9840-01b2248bd686");
        if ((row >= getRowCount()) || (row < 0)) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "f50ac917-008c-493c-860b-b6ed2eabf17c");
            throw new IllegalArgumentException("The 'row' argument is out of bounds.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "9621bd59-26b3-4a07-9799-a83c74569b85");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "a90cb99e-9606-46c4-a7ad-8a1d077dba8a");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "82139cc1-cf1f-4d29-a30b-b7f61fbb8fc6");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "6d28be81-6013-4ae5-ac68-4f928af3d606");
        if (obj == this) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "44b153c7-ce0d-4f3d-b418-cda509606943");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "f1ee40c1-4c1f-4409-89d7-22c9c88af7c8");
        if (!(obj instanceof DefaultIntervalCategoryDataset)) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "5d149a11-a9c2-4e88-bcb1-d7a049ef6e93");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "6141db1a-80b9-4f69-92cb-f05542388365");
        DefaultIntervalCategoryDataset that = (DefaultIntervalCategoryDataset) obj;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "375720be-18c3-469c-88c6-7d3f55ce29dd");
        if (!Arrays.equals(this.seriesKeys, that.seriesKeys)) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "01aec1e2-4a26-489c-9483-7eb44a4dac60");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "fba168c6-f98c-425f-8f6b-60221de4afd3");
        if (!Arrays.equals(this.categoryKeys, that.categoryKeys)) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "34577191-6735-488b-9c59-0b2996aee389");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "f99597d5-3aa1-4de5-be79-76c46a78081e");
        if (!equal(this.startData, that.startData)) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "41100bbf-93cd-4bc9-a0a8-a6af83c1c5da");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "e02b9ad0-7504-4bc5-a4b0-632c7e99fc45");
        if (!equal(this.endData, that.endData)) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "951446e9-6043-4271-aa81-8328dc6c6115");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "4384e04c-f2b4-43aa-a539-281b5e40cc7c");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "f65a5c59-b73c-4447-acc1-a4943fa3d643");
        DefaultIntervalCategoryDataset clone = (DefaultIntervalCategoryDataset) super.clone();
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "bd752dc7-8e3e-41cd-bcd9-291eeb24737e");
        clone.categoryKeys = (Comparable[]) this.categoryKeys.clone();
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "759d8908-732c-4e3d-a6a9-eff45b9145ab");
        clone.seriesKeys = (Comparable[]) this.seriesKeys.clone();
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "b3851dd1-d26d-4ed4-82ff-01c9f2985683");
        clone.startData = clone(this.startData);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "0d611bbf-5b9b-439d-b705-28b355b01b31");
        clone.endData = clone(this.endData);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "2f445016-e532-4de1-b9d4-0873d14e444c");
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
        writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "0214d5e9-8ab9-422b-8708-8512e3712b5f");
        if (array1 == null) {
            writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "166b5e94-717a-42b7-a9a7-16ff8d6d07e9");
            return (array2 == null);
        }
        writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "ad0d58ae-1fe4-4390-ace8-44144c633b8a");
        if (array2 == null) {
            writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "c19f2ddb-c514-424f-8aed-f3e54400b76c");
            return false;
        }
        writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "070c3030-8525-49a0-9cbb-1de51cc76512");
        if (array1.length != array2.length) {
            writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "25edbac2-d0cb-460d-9632-2737da06b86a");
            return false;
        }
        writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "d56d6a9d-2e35-49b5-8f9f-def5e67735bf");
        for (int i = 0; i < array1.length; i++) {
            writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "7d6cffd7-c9fa-4933-9a37-da6dbe73e326");
            if (!Arrays.equals(array1[i], array2[i])) {
                writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "536f8385-4221-4b6a-9feb-62810207adf0");
                return false;
            }
        }
        writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "cdf55dbf-5c06-46e1-947b-4713110d8ff3");
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
        writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "6f4b5214-d09f-45f9-91c8-5ccb7acf84e4");
        Args.nullNotPermitted(array, "array");
        writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "32410fc2-7ac9-468a-94fa-18b9ed244cee");
        Number[][] result = new Number[array.length][];
        writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "2ecf0152-1133-4a17-a7f0-f438483430cb");
        for (int i = 0; i < array.length; i++) {
            writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "76ddf91a-0db6-4e63-9f71-976bfda319d3");
            Number[] child = array[i];
            writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "b67a5ddc-413d-4353-9b82-3f8500ce3827");
            Number[] copychild = new Number[child.length];
            writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "760e8307-0c31-477e-90bc-23e00bf7192d");
            System.arraycopy(child, 0, copychild, 0, child.length);
            writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "c10196f7-f3d6-49f3-ac02-524666b39e88");
            result[i] = copychild;
        }
        writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_7_10.coverage", "eeb01547-cb11-4235-b021-e90eb06308a8");
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
