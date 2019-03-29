StatisticalBarRenderer
~~~
getErrorIndicatorPaint
~~~
setErrorIndicatorPaint
~
fireChangeEvent();
~~~
getErrorIndicatorStroke
~~~
setErrorIndicatorStroke
~
fireChangeEvent();
~~~
findRangeBounds
~~~
drawItem
~
drawVerticalItem(g2, state, dataArea, plot, domainAxis, rangeAxis, statData, visibleRow, row, column);
~
return;
~
throw new IllegalArgumentException("Requires StatisticalCategoryDataset.");
~
drawHorizontalItem(g2, state, dataArea, plot, domainAxis, rangeAxis, statData, visibleRow, row, column);
~
if (orientation == PlotOrientation.VERTICAL) {
    drawVerticalItem(g2, state, dataArea, plot, domainAxis, rangeAxis, statData, visibleRow, row, column);
}
~
int visibleRow = state.getVisibleSeriesIndex(row);
~
if (visibleRow < 0) {
    return;
}
~
// defensive check
if (!(data instanceof StatisticalCategoryDataset)) {
    throw new IllegalArgumentException("Requires StatisticalCategoryDataset.");
}
~
StatisticalCategoryDataset statData = (StatisticalCategoryDataset) data;
~
PlotOrientation orientation = plot.getOrientation();
~
if (orientation == PlotOrientation.HORIZONTAL) {
    drawHorizontalItem(g2, state, dataArea, plot, domainAxis, rangeAxis, statData, visibleRow, row, column);
} else if (orientation == PlotOrientation.VERTICAL) {
    drawVerticalItem(g2, state, dataArea, plot, domainAxis, rangeAxis, statData, visibleRow, row, column);
}
~~~
drawHorizontalItem
~
value = lclip;
~
value = uclip;
~
if (value <= lclip) {
    value = lclip;
}
~
// bar is not visible
return;
~
value = uclip;
~
// bar is not visible
return;
~
value = lclip;
~
// cases 5, 6, 7 and 8
if (value >= uclip) {
    value = uclip;
} else {
    if (value <= lclip) {
        value = lclip;
    }
}
~
// cases 9, 10, 11 and 12
if (value <= lclip) {
    // bar is not visible
    return;
}
~
base = getLowerClip();
~
if (value >= uclip) {
    value = uclip;
}
~
g2.setStroke(stroke);
~
g2.setPaint(paint);
~
g2.draw(bar);
~
g2.setPaint(this.errorIndicatorPaint);
~
g2.setPaint(getItemOutlinePaint(row, column));
~
g2.setStroke(this.errorIndicatorStroke);
~
g2.setStroke(getItemOutlineStroke(row, column));
~
return;
~
// cases 1, 2, 3 and 4
if (value >= uclip) {
    // bar is not visible
    return;
}
~
base = uclip;
~
if (value <= lclip) {
    value = lclip;
}
~
if (lclip <= 0.0) {
    // cases 5, 6, 7 and 8
    if (value >= uclip) {
        value = uclip;
    } else {
        if (value <= lclip) {
            value = lclip;
        }
    }
} else {
    // cases 9, 10, 11 and 12
    if (value <= lclip) {
        // bar is not visible
        return;
    }
    base = getLowerClip();
    if (value >= uclip) {
        value = uclip;
    }
}
~
itemPaint = t.transform((GradientPaint) itemPaint, bar);
~
Stroke stroke = getItemOutlineStroke(row, column);
~
Paint paint = getItemOutlinePaint(row, column);
~
if (stroke != null && paint != null) {
    g2.setStroke(stroke);
    g2.setPaint(paint);
    g2.draw(bar);
}
~
double valueDelta = n.doubleValue();
~
double highVal = rangeAxis.valueToJava2D(meanValue.doubleValue() + valueDelta, dataArea, yAxisLocation);
~
double lowVal = rangeAxis.valueToJava2D(meanValue.doubleValue() - valueDelta, dataArea, yAxisLocation);
~
if (this.errorIndicatorPaint != null) {
    g2.setPaint(this.errorIndicatorPaint);
} else {
    g2.setPaint(getItemOutlinePaint(row, column));
}
~
if (this.errorIndicatorStroke != null) {
    g2.setStroke(this.errorIndicatorStroke);
} else {
    g2.setStroke(getItemOutlineStroke(row, column));
}
~
Line2D line;
~
line = new Line2D.Double(lowVal, rectY + rectHeight / 2.0d, highVal, rectY + rectHeight / 2.0d);
~
g2.draw(line);
~
line = new Line2D.Double(highVal, rectY + rectHeight * 0.25, highVal, rectY + rectHeight * 0.75);
~
g2.draw(line);
~
line = new Line2D.Double(lowVal, rectY + rectHeight * 0.25, lowVal, rectY + rectHeight * 0.75);
~
g2.draw(line);
~
drawItemLabel(g2, dataset, row, column, plot, generator, bar, (value < 0.0));
~
addItemEntity(entities, dataset, row, column, bar);
~
// BAR Y
double rectY = calculateBarW0(plot, PlotOrientation.HORIZONTAL, dataArea, domainAxis, state, visibleRow, column);
~
// BAR X
Number meanValue = dataset.getMeanValue(row, column);
~
if (meanValue == null) {
    return;
}
~
double value = meanValue.doubleValue();
~
double base = 0.0;
~
double lclip = getLowerClip();
~
double uclip = getUpperClip();
~
if (uclip <= 0.0) {
    // cases 1, 2, 3 and 4
    if (value >= uclip) {
        // bar is not visible
        return;
    }
    base = uclip;
    if (value <= lclip) {
        value = lclip;
    }
} else if (lclip <= 0.0) {
    // cases 5, 6, 7 and 8
    if (value >= uclip) {
        value = uclip;
    } else {
        if (value <= lclip) {
            value = lclip;
        }
    }
} else {
    // cases 9, 10, 11 and 12
    if (value <= lclip) {
        // bar is not visible
        return;
    }
    base = getLowerClip();
    if (value >= uclip) {
        value = uclip;
    }
}
~
RectangleEdge yAxisLocation = plot.getRangeAxisEdge();
~
double transY1 = rangeAxis.valueToJava2D(base, dataArea, yAxisLocation);
~
double transY2 = rangeAxis.valueToJava2D(value, dataArea, yAxisLocation);
~
double rectX = Math.min(transY2, transY1);
~
double rectHeight = state.getBarWidth();
~
double rectWidth = Math.abs(transY2 - transY1);
~
Rectangle2D bar = new Rectangle2D.Double(rectX, rectY, rectWidth, rectHeight);
~
Paint itemPaint = getItemPaint(row, column);
~
GradientPaintTransformer t = getGradientPaintTransformer();
~
if (t != null && itemPaint instanceof GradientPaint) {
    itemPaint = t.transform((GradientPaint) itemPaint, bar);
}
~
g2.setPaint(itemPaint);
~
g2.fill(bar);
~
// draw the outline...
if (isDrawBarOutline() && state.getBarWidth() > BAR_OUTLINE_WIDTH_THRESHOLD) {
    Stroke stroke = getItemOutlineStroke(row, column);
    Paint paint = getItemOutlinePaint(row, column);
    if (stroke != null && paint != null) {
        g2.setStroke(stroke);
        g2.setPaint(paint);
        g2.draw(bar);
    }
}
~
// standard deviation lines
Number n = dataset.getStdDevValue(row, column);
~
if (n != null) {
    double valueDelta = n.doubleValue();
    double highVal = rangeAxis.valueToJava2D(meanValue.doubleValue() + valueDelta, dataArea, yAxisLocation);
    double lowVal = rangeAxis.valueToJava2D(meanValue.doubleValue() - valueDelta, dataArea, yAxisLocation);
    if (this.errorIndicatorPaint != null) {
        g2.setPaint(this.errorIndicatorPaint);
    } else {
        g2.setPaint(getItemOutlinePaint(row, column));
    }
    if (this.errorIndicatorStroke != null) {
        g2.setStroke(this.errorIndicatorStroke);
    } else {
        g2.setStroke(getItemOutlineStroke(row, column));
    }
    Line2D line;
    line = new Line2D.Double(lowVal, rectY + rectHeight / 2.0d, highVal, rectY + rectHeight / 2.0d);
    g2.draw(line);
    line = new Line2D.Double(highVal, rectY + rectHeight * 0.25, highVal, rectY + rectHeight * 0.75);
    g2.draw(line);
    line = new Line2D.Double(lowVal, rectY + rectHeight * 0.25, lowVal, rectY + rectHeight * 0.75);
    g2.draw(line);
}
~
CategoryItemLabelGenerator generator = getItemLabelGenerator(row, column);
~
if (generator != null && isItemLabelVisible(row, column)) {
    drawItemLabel(g2, dataset, row, column, plot, generator, bar, (value < 0.0));
}
~
// add an item entity, if this information is being collected
EntityCollection entities = state.getEntityCollection();
~
if (entities != null) {
    addItemEntity(entities, dataset, row, column, bar);
}
~~~
drawVerticalItem
~
value = lclip;
~
value = uclip;
~
if (value <= lclip) {
    value = lclip;
}
~
// bar is not visible
return;
~
value = uclip;
~
// bar is not visible
return;
~
value = lclip;
~
// cases 5, 6, 7 and 8
if (value >= uclip) {
    value = uclip;
} else {
    if (value <= lclip) {
        value = lclip;
    }
}
~
// cases 9, 10, 11 and 12
if (value <= lclip) {
    // bar is not visible
    return;
}
~
base = getLowerClip();
~
if (value >= uclip) {
    value = uclip;
}
~
g2.setStroke(stroke);
~
g2.setPaint(paint);
~
g2.draw(bar);
~
g2.setPaint(this.errorIndicatorPaint);
~
g2.setPaint(getItemOutlinePaint(row, column));
~
g2.setStroke(this.errorIndicatorStroke);
~
g2.setStroke(getItemOutlineStroke(row, column));
~
return;
~
// cases 1, 2, 3 and 4
if (value >= uclip) {
    // bar is not visible
    return;
}
~
base = uclip;
~
if (value <= lclip) {
    value = lclip;
}
~
if (lclip <= 0.0) {
    // cases 5, 6, 7 and 8
    if (value >= uclip) {
        value = uclip;
    } else {
        if (value <= lclip) {
            value = lclip;
        }
    }
} else {
    // cases 9, 10, 11 and 12
    if (value <= lclip) {
        // bar is not visible
        return;
    }
    base = getLowerClip();
    if (value >= uclip) {
        value = uclip;
    }
}
~
itemPaint = t.transform((GradientPaint) itemPaint, bar);
~
Stroke stroke = getItemOutlineStroke(row, column);
~
Paint paint = getItemOutlinePaint(row, column);
~
if (stroke != null && paint != null) {
    g2.setStroke(stroke);
    g2.setPaint(paint);
    g2.draw(bar);
}
~
double valueDelta = n.doubleValue();
~
double highVal = rangeAxis.valueToJava2D(meanValue.doubleValue() + valueDelta, dataArea, yAxisLocation);
~
double lowVal = rangeAxis.valueToJava2D(meanValue.doubleValue() - valueDelta, dataArea, yAxisLocation);
~
if (this.errorIndicatorPaint != null) {
    g2.setPaint(this.errorIndicatorPaint);
} else {
    g2.setPaint(getItemOutlinePaint(row, column));
}
~
if (this.errorIndicatorStroke != null) {
    g2.setStroke(this.errorIndicatorStroke);
} else {
    g2.setStroke(getItemOutlineStroke(row, column));
}
~
Line2D line;
~
line = new Line2D.Double(rectX + rectWidth / 2.0d, lowVal, rectX + rectWidth / 2.0d, highVal);
~
g2.draw(line);
~
line = new Line2D.Double(rectX + rectWidth / 2.0d - 5.0d, highVal, rectX + rectWidth / 2.0d + 5.0d, highVal);
~
g2.draw(line);
~
line = new Line2D.Double(rectX + rectWidth / 2.0d - 5.0d, lowVal, rectX + rectWidth / 2.0d + 5.0d, lowVal);
~
g2.draw(line);
~
drawItemLabel(g2, dataset, row, column, plot, generator, bar, (value < 0.0));
~
addItemEntity(entities, dataset, row, column, bar);
~
// BAR X
double rectX = calculateBarW0(plot, PlotOrientation.VERTICAL, dataArea, domainAxis, state, visibleRow, column);
~
// BAR Y
Number meanValue = dataset.getMeanValue(row, column);
~
if (meanValue == null) {
    return;
}
~
double value = meanValue.doubleValue();
~
double base = 0.0;
~
double lclip = getLowerClip();
~
double uclip = getUpperClip();
~
if (uclip <= 0.0) {
    // cases 1, 2, 3 and 4
    if (value >= uclip) {
        // bar is not visible
        return;
    }
    base = uclip;
    if (value <= lclip) {
        value = lclip;
    }
} else if (lclip <= 0.0) {
    // cases 5, 6, 7 and 8
    if (value >= uclip) {
        value = uclip;
    } else {
        if (value <= lclip) {
            value = lclip;
        }
    }
} else {
    // cases 9, 10, 11 and 12
    if (value <= lclip) {
        // bar is not visible
        return;
    }
    base = getLowerClip();
    if (value >= uclip) {
        value = uclip;
    }
}
~
RectangleEdge yAxisLocation = plot.getRangeAxisEdge();
~
double transY1 = rangeAxis.valueToJava2D(base, dataArea, yAxisLocation);
~
double transY2 = rangeAxis.valueToJava2D(value, dataArea, yAxisLocation);
~
double rectY = Math.min(transY2, transY1);
~
double rectWidth = state.getBarWidth();
~
double rectHeight = Math.abs(transY2 - transY1);
~
Rectangle2D bar = new Rectangle2D.Double(rectX, rectY, rectWidth, rectHeight);
~
Paint itemPaint = getItemPaint(row, column);
~
GradientPaintTransformer t = getGradientPaintTransformer();
~
if (t != null && itemPaint instanceof GradientPaint) {
    itemPaint = t.transform((GradientPaint) itemPaint, bar);
}
~
g2.setPaint(itemPaint);
~
g2.fill(bar);
~
// draw the outline...
if (isDrawBarOutline() && state.getBarWidth() > BAR_OUTLINE_WIDTH_THRESHOLD) {
    Stroke stroke = getItemOutlineStroke(row, column);
    Paint paint = getItemOutlinePaint(row, column);
    if (stroke != null && paint != null) {
        g2.setStroke(stroke);
        g2.setPaint(paint);
        g2.draw(bar);
    }
}
~
// standard deviation lines
Number n = dataset.getStdDevValue(row, column);
~
if (n != null) {
    double valueDelta = n.doubleValue();
    double highVal = rangeAxis.valueToJava2D(meanValue.doubleValue() + valueDelta, dataArea, yAxisLocation);
    double lowVal = rangeAxis.valueToJava2D(meanValue.doubleValue() - valueDelta, dataArea, yAxisLocation);
    if (this.errorIndicatorPaint != null) {
        g2.setPaint(this.errorIndicatorPaint);
    } else {
        g2.setPaint(getItemOutlinePaint(row, column));
    }
    if (this.errorIndicatorStroke != null) {
        g2.setStroke(this.errorIndicatorStroke);
    } else {
        g2.setStroke(getItemOutlineStroke(row, column));
    }
    Line2D line;
    line = new Line2D.Double(rectX + rectWidth / 2.0d, lowVal, rectX + rectWidth / 2.0d, highVal);
    g2.draw(line);
    line = new Line2D.Double(rectX + rectWidth / 2.0d - 5.0d, highVal, rectX + rectWidth / 2.0d + 5.0d, highVal);
    g2.draw(line);
    line = new Line2D.Double(rectX + rectWidth / 2.0d - 5.0d, lowVal, rectX + rectWidth / 2.0d + 5.0d, lowVal);
    g2.draw(line);
}
~
CategoryItemLabelGenerator generator = getItemLabelGenerator(row, column);
~
if (generator != null && isItemLabelVisible(row, column)) {
    drawItemLabel(g2, dataset, row, column, plot, generator, bar, (value < 0.0));
}
~
// add an item entity, if this information is being collected
EntityCollection entities = state.getEntityCollection();
~
if (entities != null) {
    addItemEntity(entities, dataset, row, column, bar);
}
~~~
equals
~
return true;
~
return false;
~
if (obj == this) {
    return true;
}
~
if (!(obj instanceof StatisticalBarRenderer)) {
    return false;
}
~~~
writeObject
~
stream.defaultWriteObject();
~
SerialUtils.writePaint(this.errorIndicatorPaint, stream);
~
SerialUtils.writeStroke(this.errorIndicatorStroke, stream);
~~~
readObject
~
stream.defaultReadObject();
~
this.errorIndicatorPaint = SerialUtils.readPaint(stream);
~
this.errorIndicatorStroke = SerialUtils.readStroke(stream);
