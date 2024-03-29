package org.jfree.chart.renderer.category;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.GradientPaint;
import java.awt.Graphics2D;
import java.awt.Paint;
import java.awt.Stroke;
import java.awt.geom.Line2D;
import java.awt.geom.Rectangle2D;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import org.jfree.chart.axis.CategoryAxis;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.entity.EntityCollection;
import org.jfree.chart.event.RendererChangeEvent;
import org.jfree.chart.labels.CategoryItemLabelGenerator;
import org.jfree.chart.plot.CategoryPlot;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.ui.GradientPaintTransformer;
import org.jfree.chart.ui.RectangleEdge;
import org.jfree.chart.util.ObjectUtils;
import org.jfree.chart.util.PaintUtils;
import org.jfree.chart.util.PublicCloneable;
import org.jfree.chart.util.SerialUtils;
import org.jfree.data.Range;
import org.jfree.data.category.CategoryDataset;
import org.jfree.data.statistics.StatisticalCategoryDataset;

/**
 * A renderer that handles the drawing a bar plot where
 * each bar has a mean value and a standard deviation line.  The example shown
 * here is generated by the {@code StatisticalBarChartDemo1.java} program
 * included in the JFreeChart Demo Collection:
 * <br><br>
 * <img src="../../../../../images/StatisticalBarRendererSample.png"
 * alt="StatisticalBarRendererSample.png">
 */
public class StatisticalBarRenderer extends BarRenderer implements CategoryItemRenderer, Cloneable, PublicCloneable, Serializable {

    /** For serialization. */
    private static final long serialVersionUID = -4986038395414039117L;

    /** The paint used to show the error indicator. */
    private transient Paint errorIndicatorPaint;

    /**
     * The stroke used to draw the error indicators.
     *
     * @since 1.0.8
     */
    private transient Stroke errorIndicatorStroke;

    /**
     * Default constructor.
     */
    public StatisticalBarRenderer() {
        super();
        this.errorIndicatorPaint = Color.gray;
        this.errorIndicatorStroke = new BasicStroke(1.0f);
    }

    /**
     * Returns the paint used for the error indicators.
     *
     * @return The paint used for the error indicators (possibly
     *         {@code null}).
     *
     * @see #setErrorIndicatorPaint(Paint)
     */
    public Paint getErrorIndicatorPaint() {
        return this.errorIndicatorPaint;
    }

    /**
     * Sets the paint used for the error indicators (if {@code null},
     * the item outline paint is used instead) and sends a
     * {@link RendererChangeEvent} to all registered listeners.
     *
     * @param paint  the paint ({@code null} permitted).
     *
     * @see #getErrorIndicatorPaint()
     */
    public void setErrorIndicatorPaint(Paint paint) {
        this.errorIndicatorPaint = paint;
    }

    /**
     * Returns the stroke used to draw the error indicators.  If this is
     * {@code null}, the renderer will use the item outline stroke).
     *
     * @return The stroke (possibly {@code null}).
     *
     * @see #setErrorIndicatorStroke(Stroke)
     *
     * @since 1.0.8
     */
    public Stroke getErrorIndicatorStroke() {
        return this.errorIndicatorStroke;
    }

    /**
     * Sets the stroke used to draw the error indicators, and sends a
     * {@link RendererChangeEvent} to all registered listeners.  If you set
     * this to {@code null}, the renderer will use the item outline
     * stroke.
     *
     * @param stroke  the stroke ({@code null} permitted).
     *
     * @see #getErrorIndicatorStroke()
     *
     * @since 1.0.8
     */
    public void setErrorIndicatorStroke(Stroke stroke) {
        this.errorIndicatorStroke = stroke;
    }

    /**
     * Returns the range of values the renderer requires to display all the
     * items from the specified dataset. This takes into account the range
     * between the min/max values, possibly ignoring invisible series.
     *
     * @param dataset  the dataset ({@code null} permitted).
     *
     * @return The range (or {@code null} if the dataset is
     *         {@code null} or empty).
     */
    @Override
    public Range findRangeBounds(CategoryDataset dataset) {
        return findRangeBounds(dataset, true);
    }

    /**
     * Draws the bar with its standard deviation line range for a single
     * (series, category) data item.
     *
     * @param g2  the graphics device.
     * @param state  the renderer state.
     * @param dataArea  the data area.
     * @param plot  the plot.
     * @param domainAxis  the domain axis.
     * @param rangeAxis  the range axis.
     * @param data  the data.
     * @param row  the row index (zero-based).
     * @param column  the column index (zero-based).
     * @param pass  the pass index.
     */
    @Override
    public void drawItem(Graphics2D g2, CategoryItemRendererState state, Rectangle2D dataArea, CategoryPlot plot, CategoryAxis domainAxis, ValueAxis rangeAxis, CategoryDataset data, int row, int column, int pass) {
    }

    /**
     * Draws an item for a plot with a horizontal orientation.
     *
     * @param g2  the graphics device.
     * @param state  the renderer state.
     * @param dataArea  the data area.
     * @param plot  the plot.
     * @param domainAxis  the domain axis.
     * @param rangeAxis  the range axis.
     * @param dataset  the data.
     * @param visibleRow  the visible row index.
     * @param row  the row index (zero-based).
     * @param column  the column index (zero-based).
     */
    protected void drawHorizontalItem(Graphics2D g2, CategoryItemRendererState state, Rectangle2D dataArea, CategoryPlot plot, CategoryAxis domainAxis, ValueAxis rangeAxis, StatisticalCategoryDataset dataset, int visibleRow, int row, int column) {
    }

    /**
     * Draws an item for a plot with a vertical orientation.
     *
     * @param g2  the graphics device.
     * @param state  the renderer state.
     * @param dataArea  the data area.
     * @param plot  the plot.
     * @param domainAxis  the domain axis.
     * @param rangeAxis  the range axis.
     * @param dataset  the data.
     * @param visibleRow  the visible row index.
     * @param row  the row index (zero-based).
     * @param column  the column index (zero-based).
     */
    protected void drawVerticalItem(Graphics2D g2, CategoryItemRendererState state, Rectangle2D dataArea, CategoryPlot plot, CategoryAxis domainAxis, ValueAxis rangeAxis, StatisticalCategoryDataset dataset, int visibleRow, int row, int column) {
    }

    /**
     * Tests this renderer for equality with an arbitrary object.
     *
     * @param obj  the object ({@code null} permitted).
     *
     * @return A boolean.
     */
    @Override
    public boolean equals(Object obj) {
        StatisticalBarRenderer that = (StatisticalBarRenderer) obj;
        if (!PaintUtils.equal(this.errorIndicatorPaint, that.errorIndicatorPaint)) {
            return false;
        }
        if (!ObjectUtils.equal(this.errorIndicatorStroke, that.errorIndicatorStroke)) {
            return false;
        }
        return super.equals(obj);
    }

    /**
     * Provides serialization support.
     *
     * @param stream  the output stream.
     *
     * @throws IOException  if there is an I/O error.
     */
    private void writeObject(ObjectOutputStream stream) throws IOException {
        SerialUtils.writePaint(this.errorIndicatorPaint, stream);
        SerialUtils.writeStroke(this.errorIndicatorStroke, stream);
    }

    /**
     * Provides serialization support.
     *
     * @param stream  the input stream.
     *
     * @throws IOException  if there is an I/O error.
     * @throws ClassNotFoundException  if there is a classpath problem.
     */
    private void readObject(ObjectInputStream stream) throws IOException, ClassNotFoundException {
        this.errorIndicatorPaint = SerialUtils.readPaint(stream);
        this.errorIndicatorStroke = SerialUtils.readStroke(stream);
    }
}
