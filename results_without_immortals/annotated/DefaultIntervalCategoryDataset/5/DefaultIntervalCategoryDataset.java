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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "2cd259ea-ee6b-4bd6-a58c-a0f446cae40a");
        int result = 0;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "673dc4a4-bc05-415b-a0ee-316bec873ae1");
        if (this.startData != null) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "2d485636-3b7c-4497-9c86-36286cf8082b");
            result = this.startData.length;
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "b4e2907f-6c76-474e-8612-8498b0b84869");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "d557e4fc-5f60-42eb-9d18-31772cb048ee");
        int result = -1;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "f99ec9b1-8997-418e-9106-de89d0ae05aa");
        for (int i = 0; i < this.seriesKeys.length; i++) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "c300dc8c-ce57-4f57-90af-7f2a1f751115");
            if (seriesKey.equals(this.seriesKeys[i])) {
                writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "84a0c3bc-2b06-499f-ac9f-3869355405f3");
                result = i;
                writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "30d263e0-fccb-4781-a8e6-27cb257a4de1");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "8c5d5b45-2abc-4feb-95e2-fec62ab52379");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "87ab5e24-bef0-40d8-a421-bb964ca16944");
        if ((series >= getSeriesCount()) || (series < 0)) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "aa5ac7ff-e4b1-4694-a7a8-262c14803de1");
            throw new IllegalArgumentException("No such series : " + series);
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "ae6ebd6f-6a63-423b-b546-59790106aaaa");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "669ed8b7-99c1-424d-acf9-d0ecb13eb59f");
        Args.nullNotPermitted(seriesKeys, "seriesKeys");
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "67bd87e0-b427-4c28-95d9-18deec63ebef");
        if (seriesKeys.length != getSeriesCount()) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "ac69a586-832c-4aae-a0ec-a3fe2358f24d");
            throw new IllegalArgumentException("The number of series keys does not match the data.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "50acf39b-69e0-4818-b5fb-8c4ea2db13e4");
        this.seriesKeys = seriesKeys;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "702f7788-16d2-414a-a3c0-dad35efa9b7c");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "fdabc283-dfbf-4b2d-8b5c-f7deba84292b");
        int result = 0;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "85545ac3-44c6-4ea2-9a6d-67939e4bd654");
        if (this.startData != null) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "c3d7dc61-67c2-42b4-8f0d-789e9c0a673a");
            if (getSeriesCount() > 0) {
                writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "6758a2f2-d8c8-4617-aadc-7ddca3016c12");
                result = this.startData[0].length;
            }
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "32df0e07-7032-4095-b1c2-98a94aa43a42");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "07067f15-e05e-4adb-ab32-f4759fd13378");
        // we've stored them in an array...
        if (this.categoryKeys == null) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "567c6db1-9d19-4440-af21-1b2b52a3c519");
            return new ArrayList();
        } else {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "367f4671-064c-4b14-b89b-08d9b4e652f3");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "0402b45f-0bcb-4f9c-bcf3-edca4a2e51f0");
        Args.nullNotPermitted(categoryKeys, "categoryKeys");
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "aa4babd6-78df-4b64-b27e-62b3f5edefed");
        if (categoryKeys.length != getCategoryCount()) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "b7e4c989-77c3-489a-beb2-346462c77c65");
            throw new IllegalArgumentException("The number of categories does not match the data.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "17faa0da-3910-4043-93d9-da95089ce674");
        for (int i = 0; i < categoryKeys.length; i++) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "02df3b6f-04ac-4fcd-a20a-56e40a13f9ce");
            if (categoryKeys[i] == null) {
                writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "5dd3a9b6-07dd-4491-b496-9a12d507e957");
                throw new IllegalArgumentException("DefaultIntervalCategoryDataset.setCategoryKeys(): " + "null category not permitted.");
            }
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "91277fb6-42d4-42d2-8d59-ff8053643851");
        this.categoryKeys = categoryKeys;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "44633cfc-5969-4d8f-901b-452130410c05");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "5c239fde-8a79-4660-a85e-2c9425b5231d");
        int seriesIndex = getSeriesIndex(series);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "87feda8d-1d80-40e9-b653-c716961a7c32");
        if (seriesIndex < 0) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "c675c5ac-04ab-4efe-acf6-0031909115d2");
            throw new UnknownKeyException("Unknown 'series' key.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "41c0634f-82a3-46ff-85de-9209e0d95d65");
        int itemIndex = getColumnIndex(category);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "29d9d1c9-b82b-49f2-b947-d889c8ea8c47");
        if (itemIndex < 0) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "5e991ad2-75b1-419d-9de6-a890bdd72518");
            throw new UnknownKeyException("Unknown 'category' key.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "2386e7b9-2f90-40a2-8c69-34abe8c47e97");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "d14027b4-91f4-4865-99f1-47bdc3dde57a");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "05829af4-6030-4617-b59c-6fbc0362e8c0");
        int seriesIndex = getSeriesIndex(series);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "d8e48ea3-d44a-489e-860c-8be4adfcb684");
        if (seriesIndex < 0) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "020994d3-28bc-444f-977f-7d6ea513684a");
            throw new UnknownKeyException("Unknown 'series' key.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "63b71974-29ea-400a-b1d3-f4f8c96b322e");
        int itemIndex = getColumnIndex(category);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "1ffb37b8-a9e1-47de-bb02-4ad8a710c522");
        if (itemIndex < 0) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "63c77d66-6105-47fd-b5da-01c5fb5a99bf");
            throw new UnknownKeyException("Unknown 'category' key.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "2617a947-5bd1-43e1-b132-1c7f0d139e1d");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "0774a93a-c846-460d-98ef-7d45ea2fffeb");
        // check arguments...
        if ((series < 0) || (series >= getSeriesCount())) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "242e2e05-fd2d-411a-b212-561ab6dc487c");
            throw new IllegalArgumentException("DefaultIntervalCategoryDataset.getValue(): " + "series index out of range.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "5daab887-a1fb-492b-9296-1f2551b2e3de");
        if ((category < 0) || (category >= getCategoryCount())) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "2ad69069-4fc8-44d2-b401-cf03c323247e");
            throw new IllegalArgumentException("DefaultIntervalCategoryDataset.getValue(): " + "category index out of range.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "7ae76394-b791-4140-9b28-fdce801f9947");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "cafefe0c-4b76-4711-b890-d1901b347f90");
        int seriesIndex = getSeriesIndex(series);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "5cbe27c9-877f-4f7b-bafc-c8ae9551583d");
        if (seriesIndex < 0) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "b7127db7-54c5-460c-98b4-63281e4230af");
            throw new UnknownKeyException("Unknown 'series' key.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "157145c9-fd6a-4e93-9d23-2f2c641e3992");
        int itemIndex = getColumnIndex(category);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "e1d32acc-a3d0-4cd1-9cb0-5bbc60c20001");
        if (itemIndex < 0) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "bbeaa0e2-2792-4789-8c19-3249bdcfd6a7");
            throw new UnknownKeyException("Unknown 'category' key.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "fadb434f-7aa0-4c2c-a252-1a7a2ca64ca6");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "abeb4764-3d78-4493-9206-06e8b7af1503");
        if ((series < 0) || (series >= getSeriesCount())) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "9ae8a7ea-e9a1-4557-8887-6dd79e49f9fa");
            throw new IllegalArgumentException("DefaultIntervalCategoryDataset.getValue(): " + "series index out of range.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "5f8c403b-4c0a-43fe-b3dd-fc702ea24636");
        if ((category < 0) || (category >= getCategoryCount())) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "cda8bee6-eb3e-4fc4-80c8-5092f8bf7bc6");
            throw new IllegalArgumentException("DefaultIntervalCategoryDataset.getValue(): " + "category index out of range.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "9fcafd0e-60e9-432a-915e-38fd177b85e8");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "2f350cc8-3279-46c3-b2b5-6b145c0284d5");
        // does the series exist?
        if ((series < 0) || (series > getSeriesCount() - 1)) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "1e421899-0cca-4aa6-b67d-2b9531215776");
            throw new IllegalArgumentException("DefaultIntervalCategoryDataset.setValue: " + "series outside valid range.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "94012ec5-c862-4287-ad4f-f99b05f2d6fe");
        // is the category valid?
        int categoryIndex = getCategoryIndex(category);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "4f5544a3-d33c-4e99-8578-8027bd6adb30");
        if (categoryIndex < 0) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "e49b8c8f-0a1c-470d-a06a-1d1007c82843");
            throw new IllegalArgumentException("DefaultIntervalCategoryDataset.setValue: " + "unrecognised category.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "ec127158-d310-41d6-b861-b57cdcfad4fe");
        // update the data...
        this.startData[series][categoryIndex] = value;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "7b74043c-0c56-4b85-b70a-df4a32136bc4");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "448880e3-d1b1-4a2c-aff4-86dc3dc77ce0");
        // does the series exist?
        if ((series < 0) || (series > getSeriesCount() - 1)) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "e556d9f2-2299-4178-9c22-f3d73e05fd68");
            throw new IllegalArgumentException("DefaultIntervalCategoryDataset.setValue: " + "series outside valid range.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "2578c013-afd5-48d0-92a0-7d80370e70e2");
        // is the category valid?
        int categoryIndex = getCategoryIndex(category);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "561b09e7-161a-4fe5-a8c7-3f731edb56ae");
        if (categoryIndex < 0) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "846dbb8b-5e65-4fd4-a142-7aad4981832c");
            throw new IllegalArgumentException("DefaultIntervalCategoryDataset.setValue: " + "unrecognised category.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "03d660e1-be82-4833-8f09-e3a31d03a4a9");
        // update the data...
        this.endData[series][categoryIndex] = value;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "cf9aa08a-a9cf-4a3e-8f07-df2fe8dd0b6e");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "e8f4c16e-33b3-4fff-8399-1dae9e4f1423");
        int result = -1;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "1516e9cc-2e5e-49a2-842a-aa3584081503");
        for (int i = 0; i < this.categoryKeys.length; i++) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "f380ec30-ee39-4abe-a675-e28692d57b58");
            if (category.equals(this.categoryKeys[i])) {
                writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "c266547f-35ca-4d41-9e75-3c0c3c49ecb1");
                result = i;
                writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "e8bd6525-3607-4fab-910d-9e7af96f733a");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "a111ebda-a032-4c76-afc7-b08f0118bc66");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "bc1008cb-d49c-4ea1-b6b0-49c785e2ee3f");
        Comparable[] result = new Comparable[count];
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "33525e4a-598f-4b94-bc82-747a64200ad1");
        String name;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "d7039f69-2427-47da-bb0c-39ae303163f2");
        for (int i = 0; i < count; i++) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "dab756e4-63fb-46e6-ab78-831d5aeb6c49");
            name = prefix + (i + 1);
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "dc0eeb32-c01f-4631-8702-d617ae46e0c1");
            result[i] = name;
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "2cfd5668-f67f-4774-a81b-2a2a26f5b229");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "76a04e19-f793-43f7-80fd-f13916f93571");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "34be5021-33d3-45a1-ae18-8c478c134caf");
        Args.nullNotPermitted(columnKey, "columnKey");
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "107b03ee-0ad5-4bdc-b908-dcc14fc211a6");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "3892a223-132f-4dfe-bb73-9d66d83f97d1");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "d3d9f50b-8840-4208-a0d8-c86514b38ef3");
        // we've stored them in an array...
        if (this.seriesKeys == null) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "cd65af91-8d8f-4ecd-9b25-d5c223c0faf6");
            return new java.util.ArrayList();
        } else {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "0933a1f0-cff6-4063-b647-7902b8dfc9b5");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "60f2e07d-2361-4ea4-8f9e-e4bc034ee0bf");
        if ((row >= getRowCount()) || (row < 0)) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "089caf1f-6314-44f7-bf40-82e1e48a8429");
            throw new IllegalArgumentException("The 'row' argument is out of bounds.");
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "81ad55c2-07cf-45fb-aabe-4c562fa777c5");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "646bf081-3c66-4ab9-8358-22156564bf66");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "70d1a388-68f2-49ab-9e19-a0138544c4ff");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "10648880-749e-4776-b33f-d3d3e6d6aac4");
        if (obj == this) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "64ec5fc3-bd5f-45b2-b061-fe5b97d001ad");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "a49a9ce8-270f-4320-bad1-66b6658a5228");
        if (!(obj instanceof DefaultIntervalCategoryDataset)) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "2ea3a607-8970-4136-a68e-e0cd47c18b2b");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "9196f5e7-9db7-4c97-8fc1-3a498b0ee4e6");
        DefaultIntervalCategoryDataset that = (DefaultIntervalCategoryDataset) obj;
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "ebf7c1d1-53cf-4c20-b30e-978029234330");
        if (!Arrays.equals(this.seriesKeys, that.seriesKeys)) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "ba7009c7-20f9-4adc-8100-14614013ac2c");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "6b948e99-0fcd-4b45-adcc-97b5c78df64f");
        if (!Arrays.equals(this.categoryKeys, that.categoryKeys)) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "2d7b28c5-8003-412c-aabb-694236d98c1f");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "53f7399a-d9b6-4066-96eb-c86db58af925");
        if (!equal(this.startData, that.startData)) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "38b4ac70-73d6-4d2e-bc1b-f927f83630b3");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "ff20ca2c-ef13-4a1d-a188-ff7ce4f7d602");
        if (!equal(this.endData, that.endData)) {
            writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "646a5e0f-656b-4d91-8286-6b939cc3b522");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "f7fa811f-f8b2-43f7-a939-809b1bcf5956");
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
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "be20de90-3122-4650-b06e-8afa996b5eb2");
        DefaultIntervalCategoryDataset clone = (DefaultIntervalCategoryDataset) super.clone();
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "651157aa-33bb-45ce-aec7-a379d47b5bac");
        clone.categoryKeys = (Comparable[]) this.categoryKeys.clone();
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "20005234-8c9d-4621-acc3-753f5fab76c4");
        clone.seriesKeys = (Comparable[]) this.seriesKeys.clone();
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "5dd2f8e6-54a4-4983-9105-dd27e413ef74");
        clone.startData = clone(this.startData);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "c6071651-6820-4759-9593-a11e7d683d13");
        clone.endData = clone(this.endData);
        writeline("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "789e173a-f0ff-4d2f-a011-fff0bda6e086");
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
        writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "f06eaee2-3dc9-4809-8467-5cf9a4137820");
        if (array1 == null) {
            writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "878c72df-4a66-4567-a7e4-c6a802712cbd");
            return (array2 == null);
        }
        writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "1eadd5c7-9bd0-46a7-aeb2-e8a237096fc5");
        if (array2 == null) {
            writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "24b0c9a6-3e6b-4e7e-ac87-c57ed2a8d51a");
            return false;
        }
        writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "4237b56d-4a61-47a9-9055-7100baad427d");
        if (array1.length != array2.length) {
            writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "18603513-eb50-40a5-81c6-a4288a6b6358");
            return false;
        }
        writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "af3c73cd-4bc3-4973-b6a5-97339d72d9d8");
        for (int i = 0; i < array1.length; i++) {
            writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "b146f516-46de-468b-829f-a025bc31d72f");
            if (!Arrays.equals(array1[i], array2[i])) {
                writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "e9b364e8-e9d1-4fda-952a-788d13bb7007");
                return false;
            }
        }
        writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "170b2d57-ac53-4076-ba30-01045fe5c4af");
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
        writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "79c36f4e-3fe4-4154-a1cf-52f461e3aa41");
        Args.nullNotPermitted(array, "array");
        writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "8789606b-a9a2-46f2-9527-b37d4ab3b99f");
        Number[][] result = new Number[array.length][];
        writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "dbc5e22d-ada8-471f-88aa-dcb930adca7c");
        for (int i = 0; i < array.length; i++) {
            writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "b275539b-f2ec-4e64-9ca0-b66659d25ae2");
            Number[] child = array[i];
            writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "7b5e9f9d-b9fb-4df1-8f96-99ed58611594");
            Number[] copychild = new Number[child.length];
            writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "c2096a60-83d9-4e84-997f-172782512ed8");
            System.arraycopy(child, 0, copychild, 0, child.length);
            writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "cb896557-d364-46fb-a692-2516c1bcfe07");
            result[i] = copychild;
        }
        writelineStatic("/home/ubuntu/results/coverage/DefaultIntervalCategoryDataset/DefaultIntervalCategoryDataset_5_10.coverage", "0cda644b-85ff-4707-b60a-8aed314b5763");
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
