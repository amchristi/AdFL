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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "2e6d4058-c4b7-403b-9ea9-bf76df378f65");
        LengthConstraintType w = constraint.getWidthConstraintType();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "6f80804c-4eb9-4af6-a643-e85169b069cb");
        LengthConstraintType h = constraint.getHeightConstraintType();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "b0795f65-c784-4a64-95bf-fb1cb90fdeb0");
        if (w == LengthConstraintType.NONE) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "3bff6b7d-c512-442a-adac-6623eb89f218");
            if (h == LengthConstraintType.NONE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "ae9ad757-a1f0-4bcd-8412-6e315b065879");
                return arrangeNN(container, g2);
            } else if (h == LengthConstraintType.FIXED) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "9d23c6e3-336e-426f-8248-16c836485251");
                return arrangeNF(container, g2, constraint);
            } else if (h == LengthConstraintType.RANGE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "2560a8d1-f0e4-47de-87fc-9a4a743ecd7f");
                return arrangeNR(container, g2, constraint);
            }
        } else if (w == LengthConstraintType.FIXED) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "d19b9ae1-b194-4e71-a59a-397dae558cca");
            if (h == LengthConstraintType.NONE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "f7e658d2-6bcc-4700-bf3b-c8d44122d6e9");
                return arrangeFN(container, g2, constraint);
            } else if (h == LengthConstraintType.FIXED) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "c44b4504-d160-414f-8793-3b5fcdbd7b11");
                return arrangeFF(container, g2, constraint);
            } else if (h == LengthConstraintType.RANGE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "773c7b80-736b-425c-8ca5-e53206b33c8e");
                return arrangeFR(container, g2, constraint);
            }
        } else if (w == LengthConstraintType.RANGE) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "65ac3049-fdb8-4ec3-a472-3f690ec556e1");
            if (h == LengthConstraintType.NONE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "716f8bce-3d3e-4f1a-8552-2b5d6887bc13");
                return arrangeRN(container, g2, constraint);
            } else if (h == LengthConstraintType.FIXED) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "a91de9a6-1787-408a-9843-1cf1ed878d64");
                return arrangeRF(container, g2, constraint);
            } else if (h == LengthConstraintType.RANGE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "b16ed844-50c4-44ff-aea3-b8f461951dfb");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "d962d68a-d01e-4bc0-bc2a-c7e1f0882e97");
        double maxW = 0.0;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "1432cf3e-f074-4a4a-a5f7-c9aba19d14f7");
        double maxH = 0.0;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "d3c9b206-cf1a-413a-8f48-373abc7b8242");
        List blocks = container.getBlocks();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "3093ec23-1f0d-4504-bee9-23a446f296ea");
        Iterator iterator = blocks.iterator();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "31130c93-82c9-4f4c-a4ee-22e5ff42443d");
        while (iterator.hasNext()) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "5a91695e-b7f2-4299-861f-a208b3457465");
            Block b = (Block) iterator.next();
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "edb32a4d-6cf6-4c5b-8811-21075c1c0ef6");
            if (b != null) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "c4565875-c705-44fb-bc61-2889a238189d");
                Size2D s = b.arrange(g2, RectangleConstraint.NONE);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "8772a831-7a7a-4b08-9cc8-dfbdd77e8a02");
                maxW = Math.max(maxW, s.width);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "5f8676fa-5e6d-44f3-9c04-4a74f5176d39");
                maxH = Math.max(maxH, s.height);
            }
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "7a3d01d0-09d2-4c94-bf60-3591cbb1765e");
        double width = this.columns * maxW;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "cb3a1fd5-cd16-4484-84f2-65cc36d85dfe");
        double height = this.rows * maxH;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "17d83bb3-62f2-4a09-8206-a6b5791e42da");
        RectangleConstraint c = new RectangleConstraint(width, height);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "f0fbbf67-b32c-4853-9a65-476b1d2208ef");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "9105f6d6-7722-477e-8188-10f85c39c730");
        double width = constraint.getWidth() / this.columns;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "3690644b-286a-4f41-8425-5800a7d3884a");
        double height = constraint.getHeight() / this.rows;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "2d76cc35-989b-4ac3-bf28-62d5451b08dd");
        List blocks = container.getBlocks();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "3d53efee-ca64-4a03-aa35-38150bc53078");
        for (int c = 0; c < this.columns; c++) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "48c91d45-b3e5-4e61-859f-db50c8800feb");
            for (int r = 0; r < this.rows; r++) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "9df7ad22-bea5-40dc-8981-facce819f804");
                int index = r * this.columns + c;
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "1c40c534-a996-49e1-8019-2b553d084d50");
                if (index >= blocks.size()) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "9fc805a6-7f1c-43c5-9e86-4863bcf33b6f");
                    break;
                }
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "f71d35ce-1562-4b9b-9fbd-0c8a743c8905");
                Block b = (Block) blocks.get(index);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "c78ff3e7-13d0-4f85-ab6d-db23fadbe4a8");
                if (b != null) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "cfc62bc2-dd45-4b24-81da-043c7a6d80a3");
                    b.setBounds(new Rectangle2D.Double(c * width, r * height, width, height));
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "af24a4d8-389d-466e-bdc8-899453508b80");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "c1a32295-8c95-47dc-9c1e-d08991ab7390");
        RectangleConstraint c1 = constraint.toUnconstrainedHeight();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "7ebc8542-9400-482a-b0f9-a7281c7ff123");
        Size2D size1 = arrange(container, g2, c1);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "439ad847-f76a-48c2-a04b-e4afe11eae66");
        if (constraint.getHeightRange().contains(size1.getHeight())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "c539e523-91df-4fee-91fa-1d20a946a9c3");
            return size1;
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "cd473963-8544-4c9b-a747-03f0aebf8eeb");
            double h = constraint.getHeightRange().constrain(size1.getHeight());
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "a17149fb-fb9d-4328-b524-f76404350ad1");
            RectangleConstraint c2 = constraint.toFixedHeight(h);
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "79a03d7b-8ef7-4fa9-8e2d-211200085ff2");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "7c86c152-d294-40d1-83ee-e8164603a13f");
        RectangleConstraint c1 = constraint.toUnconstrainedWidth();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "06b21091-6f6c-41a3-8653-6426b362ba8c");
        Size2D size1 = arrange(container, g2, c1);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "b51df0e8-0b09-455f-8eb5-53d71e38beee");
        if (constraint.getWidthRange().contains(size1.getWidth())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "3612fe1f-95ab-4b03-92ad-4ad653882a94");
            return size1;
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "d49ff4c5-1829-4195-9b92-ffd3a2ac8f93");
            double w = constraint.getWidthRange().constrain(size1.getWidth());
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "167d1d55-a7c5-45cd-8967-f84c29ea9ab1");
            RectangleConstraint c2 = constraint.toFixedWidth(w);
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "8aa747fc-d82e-44e5-84be-2d641382fbd8");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "0e841446-bb4b-4fd8-88e3-5f2876393a09");
        RectangleConstraint c1 = constraint.toUnconstrainedWidth();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "dc2d8e2f-5071-4cf4-addf-3cc83122f5e7");
        Size2D size1 = arrange(container, g2, c1);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "45143c63-f7d7-454d-8f0f-37c92b29227d");
        if (constraint.getWidthRange().contains(size1.getWidth())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "263b82bb-7d74-419f-83e9-347ce07d6102");
            return size1;
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "3deb6d9d-48e4-479d-9860-f71eaaa3c0c4");
            double w = constraint.getWidthRange().constrain(size1.getWidth());
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "42615beb-88f0-42de-9a7f-a93efcf75b62");
            RectangleConstraint c2 = constraint.toFixedWidth(w);
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "169ab6c1-655e-4a50-94f2-cb24bd18d4cf");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "7b5d34ae-c7a1-435f-8e29-110d3a238016");
        RectangleConstraint c1 = constraint.toUnconstrainedHeight();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "7b579f7a-a83e-4bfa-a87a-993c9b51c8b2");
        Size2D size1 = arrange(container, g2, c1);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "cd4f5b2a-cd1b-4435-a207-7da9e4f07f45");
        if (constraint.getHeightRange().contains(size1.getHeight())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "015aa9fe-fbc8-4677-8a1f-cbcd12e441da");
            return size1;
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "1e52633f-6cb2-46ba-ac44-5f1b9a61d270");
            double h = constraint.getHeightRange().constrain(size1.getHeight());
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "64d909c9-dd76-461a-b522-631e88860fa8");
            RectangleConstraint c2 = constraint.toFixedHeight(h);
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "670b1ec3-e3ca-48d2-815b-236844a6f5e5");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "1c0fdb07-d958-41af-9abb-cd4de62fcc55");
        Size2D size1 = arrange(container, g2, RectangleConstraint.NONE);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "3922f217-8b52-470f-bf69-648588317364");
        if (constraint.getWidthRange().contains(size1.getWidth())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "d6c54b11-5d30-40a1-8482-a5b260f0619e");
            if (constraint.getHeightRange().contains(size1.getHeight())) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "71eabc76-9ab9-4860-9a4e-ce456ac9482a");
                return size1;
            } else {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "37a1d727-b607-42bc-9e71-ba50d8fd8919");
                double h = constraint.getHeightRange().constrain(size1.getHeight());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "7cb1a1ad-8307-4ec9-bd50-2be17782bcf1");
                RectangleConstraint cc = new RectangleConstraint(size1.getWidth(), h);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "37c18da3-f96c-4894-b68a-934334b8cd6f");
                return arrangeFF(container, g2, cc);
            }
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "ba3a47f6-fe78-4ee8-93e8-e9334f592ce1");
            if (constraint.getHeightRange().contains(size1.getHeight())) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "bb29ef0e-1a15-4cea-a711-3d5864927fa0");
                double w = constraint.getWidthRange().constrain(size1.getWidth());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "2d6b6f23-06f5-4f02-988e-abbdb300b28c");
                RectangleConstraint cc = new RectangleConstraint(w, size1.getHeight());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "3c897ce2-ee61-40e6-8a1f-3f067a316db7");
                return arrangeFF(container, g2, cc);
            } else {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "7cfc633a-882b-4820-b0cb-bcc51fc5580d");
                double w = constraint.getWidthRange().constrain(size1.getWidth());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "277a8339-0976-4d08-9e78-0d3a56a1f995");
                double h = constraint.getHeightRange().constrain(size1.getHeight());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "ef0d343f-5016-4b12-98ef-84cea9ffc3a0");
                RectangleConstraint cc = new RectangleConstraint(w, h);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "5794a535-701f-4184-b072-420501865e01");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "9c0677c4-8943-4282-a35e-416d79de40ec");
        double width = constraint.getWidth() / this.columns;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "041e9525-8383-414f-b04c-c5b757c8ad7d");
        RectangleConstraint bc = constraint.toFixedWidth(width);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "84b45100-48f2-442b-b8e7-b58b632fa639");
        List blocks = container.getBlocks();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "8105ffc6-0a6b-4f6f-9142-d9144b98fb34");
        double maxH = 0.0;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "1252d193-bac1-4b7f-8570-fcb0e8a14fb4");
        for (int r = 0; r < this.rows; r++) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "cde7b309-baa7-4b8d-87cc-b415074a19e1");
            for (int c = 0; c < this.columns; c++) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "ee870089-1f8d-4533-baf6-0dd9bb224142");
                int index = r * this.columns + c;
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "31d2ad8a-c372-46d4-b95d-14e6254f35d4");
                if (index >= blocks.size()) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "82ca64e3-caa9-43a9-8b6c-606c9201677a");
                    break;
                }
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "a1ac92f6-b078-4ef4-bc50-861d554282b0");
                Block b = (Block) blocks.get(index);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "05ed4d7d-bac6-40b7-aff9-42775e3c137f");
                if (b != null) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "8f1b528c-868d-468a-8e17-7b46ddb80f07");
                    Size2D s = b.arrange(g2, bc);
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "2f816c01-8ff1-4873-8103-e0ba7dbc6a91");
                    maxH = Math.max(maxH, s.getHeight());
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "c6a92d8d-1216-4193-88b9-3f549416ac8c");
        RectangleConstraint cc = constraint.toFixedHeight(maxH * this.rows);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "d15cd38f-895d-4f1d-b8ba-23e6dc851f90");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "a2dc3cff-8d79-4c89-b6c2-a06e785dd31b");
        double height = constraint.getHeight() / this.rows;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "8b431023-dee7-4830-91a3-267c357081a3");
        RectangleConstraint bc = constraint.toFixedHeight(height);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "1cd393b8-a895-4ac4-ae0b-82d515a87586");
        List blocks = container.getBlocks();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "87781245-ceba-48d4-9785-2ce261a8af39");
        double maxW = 0.0;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "a8a2d6ee-3e33-410e-bfa0-97233dc25bd3");
        for (int r = 0; r < this.rows; r++) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "3ff6df06-f312-4d45-8d7b-a7c84611229c");
            for (int c = 0; c < this.columns; c++) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "5f713a6b-fedd-4225-adca-54d2e7d1870d");
                int index = r * this.columns + c;
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "30f71be6-ca08-4e34-b220-315ba327e3cb");
                if (index >= blocks.size()) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "0f2c4e7d-4f6f-4bb7-9997-2d0b367b1436");
                    break;
                }
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "03e8608c-3f51-4bf4-9fcd-de3c755e8bcf");
                Block b = (Block) blocks.get(index);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "ec904606-1828-469e-a980-d352f7bcd0ea");
                if (b != null) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "61518e67-7725-403b-81ea-86d5f5e73ce5");
                    Size2D s = b.arrange(g2, bc);
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "3734c2d1-ed1e-4ad3-a040-cbeb0d386095");
                    maxW = Math.max(maxW, s.getWidth());
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "172a4cdb-8920-458f-bcd0-f888646312af");
        RectangleConstraint cc = constraint.toFixedWidth(maxW * this.columns);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "5474acf3-13eb-4131-8204-b3c3873fbc90");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "a247e405-6c1f-4f8d-8a47-8f3cb93bf695");
        if (obj == this) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "478dfa60-74e1-4eec-ac5c-4cffb9068362");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "a707972b-a893-4e26-a354-725aa34d8767");
        if (!(obj instanceof GridArrangement)) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "9333b2c2-9f9e-4b2f-b86e-30aed6b14e2d");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "b5c9c6c7-0108-4995-a144-116c94468138");
        GridArrangement that = (GridArrangement) obj;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "a0e32d49-7892-45d0-83bd-e67ee5618fc8");
        if (this.columns != that.columns) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "b8eb1ca1-49a1-4821-967a-0671fa8f317a");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "b7c507f4-e112-4faf-a7a7-3591960b4b58");
        if (this.rows != that.rows) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "a0d73c70-411e-4f36-9d80-0a58b2c63042");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_6_10.coverage", "4dc3f214-f369-47a1-8b44-2343185b9367");
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
