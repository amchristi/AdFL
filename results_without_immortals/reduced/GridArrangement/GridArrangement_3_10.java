package org.jfree.chart.block;

import java.awt.Graphics2D;
import java.awt.geom.Rectangle2D;
import java.io.Serializable;
import java.util.Iterator;
import java.util.List;
import org.jfree.chart.ui.Size2D;

/**
 * Arranges blocks in a grid within their container.
 */
public class GridArrangement implements Arrangement, Serializable {

    /** For serialization. */
    private static final long serialVersionUID = -2563758090144655938L;

    /** The rows. */
    private int rows;

    /** The columns. */
    private int columns;

    /**
     * Creates a new grid arrangement.
     *
     * @param rows  the row count.
     * @param columns  the column count.
     */
    public GridArrangement(int rows, int columns) {
        this.rows = rows;
        this.columns = columns;
    }

    /**
     * Adds a block and a key which can be used to determine the position of
     * the block in the arrangement.  This method is called by the container
     * (you don't need to call this method directly) and gives the arrangement
     * an opportunity to record the details if they are required.
     *
     * @param block  the block.
     * @param key  the key ({@code null} permitted).
     */
    @Override
    public void add(Block block, Object key) {
    }

    /**
     * Arranges the blocks within the specified container, subject to the given
     * constraint.
     *
     * @param container  the container ({@code null} not permitted).
     * @param constraint  the constraint.
     * @param g2  the graphics device.
     *
     * @return The size following the arrangement.
     */
    @Override
    public Size2D arrange(BlockContainer container, Graphics2D g2, RectangleConstraint constraint) {
        LengthConstraintType w = constraint.getWidthConstraintType();
        LengthConstraintType h = constraint.getHeightConstraintType();
        if (w == LengthConstraintType.NONE) {
            if (h == LengthConstraintType.NONE) {
                return arrangeNN(container, g2);
            } else if (h == LengthConstraintType.FIXED) {
                return arrangeNF(container, g2, constraint);
            } else if (h == LengthConstraintType.RANGE) {
                return arrangeNR(container, g2, constraint);
            }
        } else if (w == LengthConstraintType.FIXED) {
            if (h == LengthConstraintType.NONE) {
                return arrangeFN(container, g2, constraint);
            } else if (h == LengthConstraintType.FIXED) {
                return arrangeFF(container, g2, constraint);
            } else if (h == LengthConstraintType.RANGE) {
                return arrangeFR(container, g2, constraint);
            }
        } else if (w == LengthConstraintType.RANGE) {
            if (h == LengthConstraintType.NONE) {
                return arrangeRN(container, g2, constraint);
            } else if (h == LengthConstraintType.FIXED) {
            } else if (h == LengthConstraintType.RANGE) {
            }
        }
        throw new RuntimeException("Should never get to here!");
    }

    /**
     * Arranges the container with no constraint on the width or height.
     *
     * @param container  the container ({@code null} not permitted).
     * @param g2  the graphics device.
     *
     * @return The size.
     */
    protected Size2D arrangeNN(BlockContainer container, Graphics2D g2) {
        double maxW = 0.0;
        double maxH = 0.0;
        List blocks = container.getBlocks();
        Iterator iterator = blocks.iterator();
        while (iterator.hasNext()) {
            Block b = (Block) iterator.next();
            if (b != null) {
                Size2D s = b.arrange(g2, RectangleConstraint.NONE);
                maxW = Math.max(maxW, s.width);
                maxH = Math.max(maxH, s.height);
            }
        }
        double width = this.columns * maxW;
        double height = this.rows * maxH;
        RectangleConstraint c = new RectangleConstraint(width, height);
        return arrangeFF(container, g2, c);
    }

    /**
     * Arranges the container with a fixed overall width and height.
     *
     * @param container  the container ({@code null} not permitted).
     * @param g2  the graphics device.
     * @param constraint  the constraint ({@code null} not permitted).
     *
     * @return The size following the arrangement.
     */
    protected Size2D arrangeFF(BlockContainer container, Graphics2D g2, RectangleConstraint constraint) {
        double width = constraint.getWidth() / this.columns;
        double height = constraint.getHeight() / this.rows;
        return new Size2D(this.columns * width, this.rows * height);
    }

    /**
     * Arrange with a fixed width and a height within a given range.
     *
     * @param container  the container.
     * @param constraint  the constraint.
     * @param g2  the graphics device.
     *
     * @return The size of the arrangement.
     */
    protected Size2D arrangeFR(BlockContainer container, Graphics2D g2, RectangleConstraint constraint) {
        RectangleConstraint c1 = constraint.toUnconstrainedHeight();
        Size2D size1 = arrange(container, g2, c1);
        if (constraint.getHeightRange().contains(size1.getHeight())) {
            return size1;
        } else {
            double h = constraint.getHeightRange().constrain(size1.getHeight());
            RectangleConstraint c2 = constraint.toFixedHeight(h);
            return arrange(container, g2, c2);
        }
    }

    /**
     * Arrange with a fixed height and a width within a given range.
     *
     * @param container  the container.
     * @param constraint  the constraint.
     * @param g2  the graphics device.
     *
     * @return The size of the arrangement.
     */
    protected Size2D arrangeRF(BlockContainer container, Graphics2D g2, RectangleConstraint constraint) {
        RectangleConstraint c1 = constraint.toUnconstrainedWidth();
        Size2D size1 = arrange(container, g2, c1);
        if (constraint.getWidthRange().contains(size1.getWidth())) {
            return size1;
        } else {
            double w = constraint.getWidthRange().constrain(size1.getWidth());
            RectangleConstraint c2 = constraint.toFixedWidth(w);
            return arrange(container, g2, c2);
        }
    }

    /**
     * Arrange with a fixed width and no height constraint.
     *
     * @param container  the container.
     * @param constraint  the constraint.
     * @param g2  the graphics device.
     *
     * @return The size of the arrangement.
     */
    protected Size2D arrangeRN(BlockContainer container, Graphics2D g2, RectangleConstraint constraint) {
        RectangleConstraint c1 = constraint.toUnconstrainedWidth();
        Size2D size1 = arrange(container, g2, c1);
        if (constraint.getWidthRange().contains(size1.getWidth())) {
            return size1;
        } else {
            double w = constraint.getWidthRange().constrain(size1.getWidth());
            RectangleConstraint c2 = constraint.toFixedWidth(w);
            return arrange(container, g2, c2);
        }
    }

    /**
     * Arrange with a fixed height and no width constraint.
     *
     * @param container  the container.
     * @param constraint  the constraint.
     * @param g2  the graphics device.
     *
     * @return The size of the arrangement.
     */
    protected Size2D arrangeNR(BlockContainer container, Graphics2D g2, RectangleConstraint constraint) {
        RectangleConstraint c1 = constraint.toUnconstrainedHeight();
        Size2D size1 = arrange(container, g2, c1);
        if (constraint.getHeightRange().contains(size1.getHeight())) {
            return size1;
        } else {
            double h = constraint.getHeightRange().constrain(size1.getHeight());
            RectangleConstraint c2 = constraint.toFixedHeight(h);
            return arrange(container, g2, c2);
        }
    }

    /**
     * Arrange with ranges for both the width and height constraints.
     *
     * @param container  the container.
     * @param constraint  the constraint.
     * @param g2  the graphics device.
     *
     * @return The size of the arrangement.
     */
    protected Size2D arrangeRR(BlockContainer container, Graphics2D g2, RectangleConstraint constraint) {
        Size2D size1 = arrange(container, g2, RectangleConstraint.NONE);
        if (constraint.getWidthRange().contains(size1.getWidth())) {
            if (constraint.getHeightRange().contains(size1.getHeight())) {
                return size1;
            } else {
                double h = constraint.getHeightRange().constrain(size1.getHeight());
                RectangleConstraint cc = new RectangleConstraint(size1.getWidth(), h);
                return arrangeFF(container, g2, cc);
            }
        } else {
            if (constraint.getHeightRange().contains(size1.getHeight())) {
                double w = constraint.getWidthRange().constrain(size1.getWidth());
                RectangleConstraint cc = new RectangleConstraint(w, size1.getHeight());
                return arrangeFF(container, g2, cc);
            } else {
                double w = constraint.getWidthRange().constrain(size1.getWidth());
                double h = constraint.getHeightRange().constrain(size1.getHeight());
                RectangleConstraint cc = new RectangleConstraint(w, h);
                return arrangeFF(container, g2, cc);
            }
        }
    }

    /**
     * Arrange with a fixed width and a height within a given range.
     *
     * @param container  the container.
     * @param g2  the graphics device.
     * @param constraint  the constraint.
     *
     * @return The size of the arrangement.
     */
    protected Size2D arrangeFN(BlockContainer container, Graphics2D g2, RectangleConstraint constraint) {
        double width = constraint.getWidth() / this.columns;
        RectangleConstraint bc = constraint.toFixedWidth(width);
        List blocks = container.getBlocks();
        double maxH = 0.0;
        for (int r = 0; r < this.rows; r++) {
            for (int c = 0; c < this.columns; c++) {
                int index = r * this.columns + c;
                if (index >= blocks.size()) {
                    break;
                }
                Block b = (Block) blocks.get(index);
                if (b != null) {
                    Size2D s = b.arrange(g2, bc);
                    maxH = Math.max(maxH, s.getHeight());
                }
            }
        }
        RectangleConstraint cc = constraint.toFixedHeight(maxH * this.rows);
        return arrange(container, g2, cc);
    }

    /**
     * Arrange with a fixed height and no constraint for the width.
     *
     * @param container  the container.
     * @param g2  the graphics device.
     * @param constraint  the constraint.
     *
     * @return The size of the arrangement.
     */
    protected Size2D arrangeNF(BlockContainer container, Graphics2D g2, RectangleConstraint constraint) {
        double height = constraint.getHeight() / this.rows;
        RectangleConstraint bc = constraint.toFixedHeight(height);
        List blocks = container.getBlocks();
        double maxW = 0.0;
        for (int r = 0; r < this.rows; r++) {
            for (int c = 0; c < this.columns; c++) {
                int index = r * this.columns + c;
                Block b = (Block) blocks.get(index);
                if (b != null) {
                    Size2D s = b.arrange(g2, bc);
                    maxW = Math.max(maxW, s.getWidth());
                }
            }
        }
        RectangleConstraint cc = constraint.toFixedWidth(maxW * this.columns);
        return arrange(container, g2, cc);
    }

    /**
     * Clears any cached layout information retained by the arrangement.
     */
    @Override
    public void clear() {
    }

    /**
     * Compares this layout manager for equality with an arbitrary object.
     *
     * @param obj  the object.
     *
     * @return A boolean.
     */
    @Override
    public boolean equals(Object obj) {
        GridArrangement that = (GridArrangement) obj;
        if (this.columns != that.columns) {
            return false;
        }
        if (this.rows != that.rows) {
            return false;
        }
        return true;
    }
}
