package org.jfree.chart.block;

import java.awt.Graphics2D;
import java.awt.geom.Rectangle2D;
import java.io.Serializable;
import java.util.Iterator;
import java.util.List;
import org.jfree.chart.ui.Size2D;
import java.io.*;

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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "b7ef1533-be01-4875-aa44-dfdd52ec8450");
        LengthConstraintType w = constraint.getWidthConstraintType();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "503f842a-bd49-498c-b856-0fb096475c5f");
        LengthConstraintType h = constraint.getHeightConstraintType();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "4edc0e1c-5ba4-4479-805d-f6e01d70cf0c");
        if (w == LengthConstraintType.NONE) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "3e63ac53-225b-47d1-85f8-4f2c3b0906e4");
            if (h == LengthConstraintType.NONE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "f9dd5f71-4675-49e4-b3cb-7e6f584591da");
                return arrangeNN(container, g2);
            } else if (h == LengthConstraintType.FIXED) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "02a9363a-d8b3-4f08-b4f3-88bee0dd3349");
                return arrangeNF(container, g2, constraint);
            } else if (h == LengthConstraintType.RANGE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "54794d8a-1346-420a-9cf1-1154344b7a71");
                return arrangeNR(container, g2, constraint);
            }
        } else if (w == LengthConstraintType.FIXED) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "8c4f344c-46ea-4f01-acdb-003e2af0bf36");
            if (h == LengthConstraintType.NONE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "f8fe0089-d2a5-479a-968a-c9d3b3a87cde");
                return arrangeFN(container, g2, constraint);
            } else if (h == LengthConstraintType.FIXED) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "567e9e85-9a82-42fd-acf7-5704665ba9c2");
                return arrangeFF(container, g2, constraint);
            } else if (h == LengthConstraintType.RANGE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "8a73d7f5-d380-4e7b-b7c8-5f011395e712");
                return arrangeFR(container, g2, constraint);
            }
        } else if (w == LengthConstraintType.RANGE) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "f25d928d-d7ce-458a-a079-a2747c437568");
            if (h == LengthConstraintType.NONE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "60a9883d-8340-4b4b-a33f-1ac4f530ee02");
                return arrangeRN(container, g2, constraint);
            } else if (h == LengthConstraintType.FIXED) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "3c7677e9-17fd-4592-b17d-8f92cf916c49");
                return arrangeRF(container, g2, constraint);
            } else if (h == LengthConstraintType.RANGE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "2f23024f-5684-410f-b967-7eb783ee1165");
                return arrangeRR(container, g2, constraint);
            }
        }
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "43ba1bec-1ed8-475b-bbbf-f69ee5ffdaa3");
        double maxW = 0.0;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "ca685f9e-5099-468b-ad9e-f0f5124ff9ce");
        double maxH = 0.0;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "9f23fde9-baff-4af0-8ae3-b09e048595d4");
        List blocks = container.getBlocks();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "3ffb1cbb-4043-4e69-9f61-9130d82bf219");
        Iterator iterator = blocks.iterator();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "47153d12-b3da-4f0d-8a2a-5a660ca51ed5");
        while (iterator.hasNext()) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "19c7566a-3dcb-4a63-a4a3-06e315de4d0b");
            Block b = (Block) iterator.next();
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "efbee1d0-cb4e-47aa-a568-2f6d1fa3d371");
            if (b != null) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "d5460713-c5bf-476d-a042-32df2767d61a");
                Size2D s = b.arrange(g2, RectangleConstraint.NONE);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "7fe555e6-d970-419e-b87d-81ed38297a26");
                maxW = Math.max(maxW, s.width);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "0e67e637-fe14-4ae1-a2f4-ebc7a23706f2");
                maxH = Math.max(maxH, s.height);
            }
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "ef805fba-8430-4c43-b8c3-3b8d7ee2434a");
        double width = this.columns * maxW;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "fad2fd82-a7a4-4759-b036-a0e4d17e7ff6");
        double height = this.rows * maxH;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "63d47fbb-d2e6-4697-828f-514f6368ab7a");
        RectangleConstraint c = new RectangleConstraint(width, height);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "c6c87384-09fe-498b-8148-67dd85a391e5");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "779f6d66-a82f-407e-aa0e-e2d61dc828e2");
        double width = constraint.getWidth() / this.columns;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "789537af-59a0-4863-b016-58fc37d89a9b");
        double height = constraint.getHeight() / this.rows;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "32782a36-bd0b-4916-a3d1-0ae3e8d70074");
        List blocks = container.getBlocks();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "d8defabc-57a9-4a21-b2dc-4e3518e8b45c");
        for (int c = 0; c < this.columns; c++) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "754eb2ab-a7bb-4fb6-8961-6993a6de5269");
            for (int r = 0; r < this.rows; r++) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "d64423eb-bf01-4c73-be5d-2d5db2fcafb0");
                int index = r * this.columns + c;
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "719157e4-eeb7-4287-a8f4-6149134aaa0c");
                if (index >= blocks.size()) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "081e93ed-f330-40a9-a770-a3cede645867");
                    break;
                }
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "ae9e4c28-31aa-4423-a07c-7f035be6c60c");
                Block b = (Block) blocks.get(index);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "90c7064c-e754-4bd9-b3a8-33c170d96007");
                if (b != null) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "dc6338bc-43b1-4755-abca-95dbaf74996d");
                    b.setBounds(new Rectangle2D.Double(c * width, r * height, width, height));
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "dc1f4a48-4584-4dd3-98fb-5b1637169ad6");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "b1e3fe60-173f-454f-9520-81dd5976d6c8");
        RectangleConstraint c1 = constraint.toUnconstrainedHeight();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "f324a725-faef-41de-9111-bbe11f968ba7");
        Size2D size1 = arrange(container, g2, c1);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "ce4f2d44-ff92-4ee1-916a-3196f1db3fb7");
        if (constraint.getHeightRange().contains(size1.getHeight())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "78b8f1ac-a5e0-44f9-a237-3c25a7965290");
            return size1;
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "6292c0d2-b105-4f93-a296-c0829442f7de");
            double h = constraint.getHeightRange().constrain(size1.getHeight());
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "fd385158-49b8-4aaa-b71c-5018e87b520b");
            RectangleConstraint c2 = constraint.toFixedHeight(h);
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "f2a75ebe-c7a9-4b57-8010-60dab8814b8b");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "a3059903-2dcb-44b9-bf38-4cf2ff8d86a1");
        RectangleConstraint c1 = constraint.toUnconstrainedWidth();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "63abfe1d-5846-4cba-a18b-50fb47cfe2c5");
        Size2D size1 = arrange(container, g2, c1);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "bc59af2a-3302-4086-8a6d-4cbb504d4660");
        if (constraint.getWidthRange().contains(size1.getWidth())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "ff3e0e60-0f27-4269-a89b-21aae66cda41");
            return size1;
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "4c657f44-6037-4231-985e-f4d323544fd6");
            double w = constraint.getWidthRange().constrain(size1.getWidth());
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "7153ca98-a380-431b-8a27-091108ec63b2");
            RectangleConstraint c2 = constraint.toFixedWidth(w);
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "5bb06728-54cf-47d5-bfb5-aae29fd49c5d");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "6f818d7b-dd88-4f7b-b785-c2b7108448d6");
        RectangleConstraint c1 = constraint.toUnconstrainedWidth();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "0ae89f9c-6568-4d51-8329-090b89a031cf");
        Size2D size1 = arrange(container, g2, c1);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "0daacb1e-4fc0-4a0a-8722-313254454944");
        if (constraint.getWidthRange().contains(size1.getWidth())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "043b43ed-ce22-4442-bc18-fd61f2c0c3e3");
            return size1;
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "83efce00-255f-43b6-982c-0a48e02a817a");
            double w = constraint.getWidthRange().constrain(size1.getWidth());
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "6e35a088-e2c7-4044-9d0c-9b1b01ef275d");
            RectangleConstraint c2 = constraint.toFixedWidth(w);
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "9b4ae85b-b8e7-42d8-addc-dd7e663e0688");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "af3bcdcb-f677-4879-9405-535e39cfcf9a");
        RectangleConstraint c1 = constraint.toUnconstrainedHeight();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "7365affe-d847-4271-8430-fc4fd44e241d");
        Size2D size1 = arrange(container, g2, c1);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "bf4db80f-f4fc-464b-8e70-d9b8fe68bd54");
        if (constraint.getHeightRange().contains(size1.getHeight())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "3d4e7138-3106-41fc-b5fa-c28711f86102");
            return size1;
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "1cd1d396-019d-463e-9623-2701c77944ee");
            double h = constraint.getHeightRange().constrain(size1.getHeight());
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "28d512b4-49ee-4d42-8373-41bc5600705f");
            RectangleConstraint c2 = constraint.toFixedHeight(h);
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "e9830d1d-0a65-4ddb-9aa4-8e6430cfd558");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "f7f61350-ea0d-4132-9312-31cbbe2f2a30");
        Size2D size1 = arrange(container, g2, RectangleConstraint.NONE);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "d049669d-9b4d-421a-9882-59a2513a83a1");
        if (constraint.getWidthRange().contains(size1.getWidth())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "2ac897de-0405-4c2b-a380-954c7b078978");
            if (constraint.getHeightRange().contains(size1.getHeight())) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "aed0bd14-120e-45db-81ab-3267964be1a5");
                return size1;
            } else {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "90f58605-6ecf-4e73-9cdd-508ac6c0ed9b");
                double h = constraint.getHeightRange().constrain(size1.getHeight());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "eb85f98c-5b27-455c-a75e-c39c91d1182d");
                RectangleConstraint cc = new RectangleConstraint(size1.getWidth(), h);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "ae860fb9-947f-42c2-973f-1e23b6c54e8c");
                return arrangeFF(container, g2, cc);
            }
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "ae676a7f-03a0-4b3d-a499-9a9af74a4aaf");
            if (constraint.getHeightRange().contains(size1.getHeight())) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "e3150145-8522-432a-ad8e-3a4340f1461b");
                double w = constraint.getWidthRange().constrain(size1.getWidth());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "b55cb6d5-de7f-4cb3-87fa-730e1e4b4cde");
                RectangleConstraint cc = new RectangleConstraint(w, size1.getHeight());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "df32883a-c911-42b0-93bb-b7b8fe88c437");
                return arrangeFF(container, g2, cc);
            } else {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "a4129ab2-2b6b-4718-bfef-5f0c01353d83");
                double w = constraint.getWidthRange().constrain(size1.getWidth());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "f78c8b3f-9f84-4ae1-8d1f-1866d2f94c5c");
                double h = constraint.getHeightRange().constrain(size1.getHeight());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "416a390e-a949-4d1d-b7f1-2f95e3866bca");
                RectangleConstraint cc = new RectangleConstraint(w, h);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "bdcc1c3d-7036-4552-baab-56261d80358c");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "94497e20-e29b-464d-a911-459deb084c57");
        double width = constraint.getWidth() / this.columns;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "104c9b49-2852-4fcc-a8a5-8ff57feee348");
        RectangleConstraint bc = constraint.toFixedWidth(width);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "ef7c25c8-a005-4fe5-962a-5850e4c23f53");
        List blocks = container.getBlocks();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "0a8460dc-606e-4201-9cac-d03228ba05b9");
        double maxH = 0.0;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "16b8a3fe-427a-48cb-af30-e79bcaef9583");
        for (int r = 0; r < this.rows; r++) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "85ad26d5-f314-4028-bd32-51ab97198e64");
            for (int c = 0; c < this.columns; c++) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "80e4ec7b-dadf-40bd-819b-fef40389a5a7");
                int index = r * this.columns + c;
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "36fd0a15-a50b-4240-b06b-ec06e4652e30");
                if (index >= blocks.size()) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "e169299b-ef1b-408d-aaba-d286baf0edc1");
                    break;
                }
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "6d94ec3b-0645-4835-96c3-91a36c9a65da");
                Block b = (Block) blocks.get(index);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "a4c80aff-0603-4a18-8ae7-aae0d40ef497");
                if (b != null) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "363cc96d-8725-4800-a82a-edcc498558bf");
                    Size2D s = b.arrange(g2, bc);
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "34a1c670-b73d-4f86-8d70-7eac21fd7370");
                    maxH = Math.max(maxH, s.getHeight());
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "87ce2ec4-0b86-4bdf-9067-26f3c25d7bdc");
        RectangleConstraint cc = constraint.toFixedHeight(maxH * this.rows);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "c5edae5e-1d86-44f0-b249-ed44d0e7198f");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "eabb8ddd-bdd4-4225-8740-cb25ff113a42");
        double height = constraint.getHeight() / this.rows;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "cdad611f-e52b-4360-9399-a38c68ce0fab");
        RectangleConstraint bc = constraint.toFixedHeight(height);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "2e19580a-4f70-4512-b11a-0812f8fb1dde");
        List blocks = container.getBlocks();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "281bdc7b-2130-46ca-a5f1-7e496e6c7512");
        double maxW = 0.0;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "89bcf123-8e21-4dd5-b7f7-6e169def82f7");
        for (int r = 0; r < this.rows; r++) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "93724d84-3ace-4386-8cc7-96d7baddecd2");
            for (int c = 0; c < this.columns; c++) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "70a77f5b-e01a-4b20-83b3-c6815ab706e7");
                int index = r * this.columns + c;
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "134adc2e-ca00-4a46-ad47-15e9c4ffb59c");
                if (index >= blocks.size()) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "38cb80de-a333-4bbd-bac9-517193153710");
                    break;
                }
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "45814390-0508-4938-8997-acb80392f6e1");
                Block b = (Block) blocks.get(index);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "3230db97-fc9c-4a4b-9aad-0bc144a99d00");
                if (b != null) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "2b636673-8c31-4267-82d6-1f3edce65d59");
                    Size2D s = b.arrange(g2, bc);
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "4cd37fd9-85b1-4b05-b94b-9836682efcce");
                    maxW = Math.max(maxW, s.getWidth());
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "e9d2a643-6016-41a0-91a6-90f4a3db8e40");
        RectangleConstraint cc = constraint.toFixedWidth(maxW * this.columns);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "d73faa33-077d-4bd0-a7af-b4ff8fb15b92");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "8f181c29-5f7b-479b-bebb-64a59982478e");
        if (obj == this) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "22509fc8-7b2e-450a-b1b8-b7d9f975a2ea");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "3822d5b9-7f9f-4d55-bb43-3e27be73bc5e");
        if (!(obj instanceof GridArrangement)) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "b9b81b1b-c51d-4e6a-934c-b4c553cbb3e3");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "1953c2f9-a171-4a58-a704-c8e75d058e56");
        GridArrangement that = (GridArrangement) obj;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "e1d84574-e7cd-4031-ae49-d984bbc87ed6");
        if (this.columns != that.columns) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "5aa80b9a-faf3-4b04-a181-3ea3f014cd63");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "02d7eeff-d4a5-4a8b-93d2-6186a6a79e74");
        if (this.rows != that.rows) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "340deba7-1e33-40bf-8b50-0a96c58e3fdb");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_5_10.coverage", "3276d35b-f15a-495d-bace-9eb8f4749b7f");
        return true;
    }

    public void writeline(String fullFilePath, String text) {
        try {
            java.io.File file = new File(fullFilePath);
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
