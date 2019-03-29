DefaultIntervalCategoryDataset
~~~
getSeriesCount
~~~
getSeriesIndex
~
break;
~~~
getSeriesKey
~
if ((series >= getSeriesCount()) || (series < 0)) {
    throw new IllegalArgumentException("No such series : " + series);
}
~~~
setSeriesKeys
~
throw new IllegalArgumentException("The number of series keys does not match the data.");
~
Args.nullNotPermitted(seriesKeys, "seriesKeys");
~
if (seriesKeys.length != getSeriesCount()) {
    throw new IllegalArgumentException("The number of series keys does not match the data.");
}
~
this.seriesKeys = seriesKeys;
~
fireDatasetChanged();
~~~
getCategoryCount
~~~
getColumnKeys
~~~
setCategoryKeys
~
throw new IllegalArgumentException("DefaultIntervalCategoryDataset.setCategoryKeys(): " + "null category not permitted.");
~
throw new IllegalArgumentException("The number of categories does not match the data.");
~
if (categoryKeys[i] == null) {
    throw new IllegalArgumentException("DefaultIntervalCategoryDataset.setCategoryKeys(): " + "null category not permitted.");
}
~
Args.nullNotPermitted(categoryKeys, "categoryKeys");
~
if (categoryKeys.length != getCategoryCount()) {
    throw new IllegalArgumentException("The number of categories does not match the data.");
}
~
for (int i = 0; i < categoryKeys.length; i++) {
    if (categoryKeys[i] == null) {
        throw new IllegalArgumentException("DefaultIntervalCategoryDataset.setCategoryKeys(): " + "null category not permitted.");
    }
}
~
this.categoryKeys = categoryKeys;
~
fireDatasetChanged();
~~~
getValue
~~~
getValue
~~~
getStartValue
~
if (seriesIndex < 0) {
    throw new UnknownKeyException("Unknown 'series' key.");
}
~
if (itemIndex < 0) {
    throw new UnknownKeyException("Unknown 'category' key.");
}
~~~
getStartValue
~
if (seriesIndex < 0) {
    throw new UnknownKeyException("Unknown 'series' key.");
}
~
if (itemIndex < 0) {
    throw new UnknownKeyException("Unknown 'category' key.");
}
~~~
getEndValue
~
if (seriesIndex < 0) {
    throw new UnknownKeyException("Unknown 'series' key.");
}
~
if (itemIndex < 0) {
    throw new UnknownKeyException("Unknown 'category' key.");
}
~~~
getEndValue
~
if (seriesIndex < 0) {
    throw new UnknownKeyException("Unknown 'series' key.");
}
~
if (itemIndex < 0) {
    throw new UnknownKeyException("Unknown 'category' key.");
}
~~~
setStartValue
~
throw new IllegalArgumentException("DefaultIntervalCategoryDataset.setValue: " + "unrecognised category.");
~
// is the category valid?
int categoryIndex = getCategoryIndex(category);
~
if (categoryIndex < 0) {
    throw new IllegalArgumentException("DefaultIntervalCategoryDataset.setValue: " + "unrecognised category.");
}
~
// update the data...
this.startData[series][categoryIndex] = value;
~
fireDatasetChanged();
~~~
setEndValue
~
throw new IllegalArgumentException("DefaultIntervalCategoryDataset.setValue: " + "unrecognised category.");
~
// is the category valid?
int categoryIndex = getCategoryIndex(category);
~
if (categoryIndex < 0) {
    throw new IllegalArgumentException("DefaultIntervalCategoryDataset.setValue: " + "unrecognised category.");
}
~
// update the data...
this.endData[series][categoryIndex] = value;
~
fireDatasetChanged();
~~~
getCategoryIndex
~
break;
~~~
generateKeys
~
String name;
~
for (int i = 0; i < count; i++) {
    name = prefix + (i + 1);
    result[i] = name;
}
~~~
getColumnKey
~~~
getColumnIndex
~
Args.nullNotPermitted(columnKey, "columnKey");
~~~
getRowIndex
~~~
getRowKeys
~~~
getRowKey
~
if ((row >= getRowCount()) || (row < 0)) {
    throw new IllegalArgumentException("The 'row' argument is out of bounds.");
}
~~~
getColumnCount
~~~
getRowCount
~~~
equals
~
if (obj == this) {
    return true;
}
~
if (!(obj instanceof DefaultIntervalCategoryDataset)) {
    return false;
}
~
DefaultIntervalCategoryDataset that = (DefaultIntervalCategoryDataset) obj;
~
if (!Arrays.equals(this.seriesKeys, that.seriesKeys)) {
    return false;
}
~
if (!Arrays.equals(this.categoryKeys, that.categoryKeys)) {
    return false;
}
~
if (!equal(this.startData, that.startData)) {
    return false;
}
~
if (!equal(this.endData, that.endData)) {
    return false;
}
~
// seem to be the same...
return true;
~~~
clone
~
clone.categoryKeys = (Comparable[]) this.categoryKeys.clone();
~
clone.seriesKeys = (Comparable[]) this.seriesKeys.clone();
~
clone.startData = clone(this.startData);
~
clone.endData = clone(this.endData);
~~~
equal
~
if (array1 == null) {
    return (array2 == null);
}
~
if (array2 == null) {
    return false;
}
~
if (array1.length != array2.length) {
    return false;
}
~
for (int i = 0; i < array1.length; i++) {
    if (!Arrays.equals(array1[i], array2[i])) {
        return false;
    }
}
~~~
clone
~
clone.categoryKeys = (Comparable[]) this.categoryKeys.clone();
~
clone.seriesKeys = (Comparable[]) this.seriesKeys.clone();
~
clone.startData = clone(this.startData);
~
clone.endData = clone(this.endData);
