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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "796a7b4c-e53f-4a97-b903-65d84bfe3293");
        LengthConstraintType w = constraint.getWidthConstraintType();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "30bec245-b538-4abc-ae09-51c9cbd8f58d");
        LengthConstraintType h = constraint.getHeightConstraintType();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "65b4c910-e52f-4e1b-a7a3-f3d7efa398fe");
        if (w == LengthConstraintType.NONE) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "841be828-9aa8-44cf-95e6-5d3d41377655");
            if (h == LengthConstraintType.NONE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "e5aab3e9-a016-4762-91e5-3e7ab5995673");
                return arrangeNN(container, g2);
            } else if (h == LengthConstraintType.FIXED) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "824ff0aa-c8cb-4585-8188-37d96778abf9");
                return arrangeNF(container, g2, constraint);
            } else if (h == LengthConstraintType.RANGE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "9c1c4c4c-268b-44b4-a1ba-69e9cdbf6205");
                return arrangeNR(container, g2, constraint);
            }
        } else if (w == LengthConstraintType.FIXED) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "b2faebc5-5707-4efa-9914-e352e3e816d3");
            if (h == LengthConstraintType.NONE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "deeda9ce-4262-419b-846b-91a169a87a75");
                return arrangeFN(container, g2, constraint);
            } else if (h == LengthConstraintType.FIXED) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "1c2a72f8-6c54-41f7-af38-981132edfd91");
                return arrangeFF(container, g2, constraint);
            } else if (h == LengthConstraintType.RANGE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "dae8ae1a-1148-4d36-b590-5775108831ce");
                return arrangeFR(container, g2, constraint);
            }
        } else if (w == LengthConstraintType.RANGE) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "e661a2b8-2bd5-4234-9990-b61ef6f21cd6");
            if (h == LengthConstraintType.NONE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "7a24f977-f0fb-451c-916d-df64fb28b242");
                return arrangeRN(container, g2, constraint);
            } else if (h == LengthConstraintType.FIXED) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "e7a26487-4d66-4ec2-a7de-32c2ba767ab6");
                return arrangeRF(container, g2, constraint);
            } else if (h == LengthConstraintType.RANGE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "b1fc6ee4-f53c-45e6-9e31-2e67dc6fca5d");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "0a3f1cbf-368b-4cd4-913c-b3e78419ade4");
        double maxW = 0.0;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "7502d941-8364-4a8f-afc0-1482b8017e7a");
        double maxH = 0.0;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "ee88558a-e8df-4169-9e04-362ebfcecc09");
        List blocks = container.getBlocks();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "31194a00-4807-4797-ad3e-694c194f185e");
        Iterator iterator = blocks.iterator();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "43b348b5-80c4-40ee-8b01-1b5546641fa8");
        while (iterator.hasNext()) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "eb1095a0-085f-4f31-9755-1df14b1bed42");
            Block b = (Block) iterator.next();
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "a5df48eb-22c8-4257-b1eb-e4b662e8ab7b");
            if (b != null) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "d2d17032-f042-4f7e-b4fa-b78e6a5974d7");
                Size2D s = b.arrange(g2, RectangleConstraint.NONE);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "0afd8d3a-80a4-4138-bdd5-df4ef6ee4a65");
                maxW = Math.max(maxW, s.width);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "5ffea031-dce6-4a4e-98fd-ad2c61e949df");
                maxH = Math.max(maxH, s.height);
            }
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "c17f6e4d-2c62-489a-acc6-77a3dc3f379b");
        double width = this.columns * maxW;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "e74b54f4-6020-4b52-b897-4f8380dcc66d");
        double height = this.rows * maxH;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "0d33f701-e4b2-4d94-a07f-890e1a4658e1");
        RectangleConstraint c = new RectangleConstraint(width, height);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "fd432406-e3db-4d0e-8d32-0e8589df8426");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "9c1f8f2d-e935-4b51-b2a9-976c868fe137");
        double width = constraint.getWidth() / this.columns;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "e7c11d07-2e02-4eba-8ff6-d3a542ddc234");
        double height = constraint.getHeight() / this.rows;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "7c7bdfd0-8684-4e2f-b52b-2724362e36d9");
        List blocks = container.getBlocks();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "c9790777-a2ae-4642-9685-52a7e365d75f");
        for (int c = 0; c < this.columns; c++) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "10c3d4f1-abf8-4a90-afca-bd4e89d82460");
            for (int r = 0; r < this.rows; r++) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "e90963d9-d012-4d4e-9559-2cd9f9f22bf3");
                int index = r * this.columns + c;
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "53c89655-6f3e-46a6-bedc-ca7242f3056e");
                if (index >= blocks.size()) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "1de6d9b2-2ea1-4c29-9295-8e0ac881fa4d");
                    break;
                }
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "9bceaeba-ddc5-445c-bc93-8b9b73ef99f1");
                Block b = (Block) blocks.get(index);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "ca7ea489-0927-45b0-b77e-2078fea40e39");
                if (b != null) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "710d3e2d-586e-477e-a9b8-221656bcf366");
                    b.setBounds(new Rectangle2D.Double(c * width, r * height, width, height));
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "e53c307c-cb5e-46b5-a3f3-69639d173e1f");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "d5930eab-f230-4565-b803-595da5872bef");
        RectangleConstraint c1 = constraint.toUnconstrainedHeight();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "f76cdf7f-bc1a-4830-b1b8-ea6b92c5bece");
        Size2D size1 = arrange(container, g2, c1);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "db6b693a-8ef5-4436-a2a4-e3a6bce345d9");
        if (constraint.getHeightRange().contains(size1.getHeight())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "5e99e03f-ca17-4799-b531-65178057129e");
            return size1;
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "734c3836-9fff-484a-9b94-afb7946880f7");
            double h = constraint.getHeightRange().constrain(size1.getHeight());
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "06f0bcc5-b641-4f57-8f6b-d620279f1ba5");
            RectangleConstraint c2 = constraint.toFixedHeight(h);
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "b6725f03-41dc-4f25-b9c2-ceddbb569f48");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "3781745b-841e-4c00-b816-cec863782f8c");
        RectangleConstraint c1 = constraint.toUnconstrainedWidth();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "9b781485-867a-4a54-99bc-8edbe5fe718b");
        Size2D size1 = arrange(container, g2, c1);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "daaff15f-8df5-4f09-8b47-d972ecad9d99");
        if (constraint.getWidthRange().contains(size1.getWidth())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "58a4647f-6b95-447a-99b9-148d05373ebc");
            return size1;
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "88eb7583-2e8c-4f22-9a60-78f8f37befbf");
            double w = constraint.getWidthRange().constrain(size1.getWidth());
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "0e0eab8d-dfb1-48fc-b7aa-1b2ced7d1198");
            RectangleConstraint c2 = constraint.toFixedWidth(w);
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "aeba9380-0759-424a-b4d1-61ae58d7ee33");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "2853fed8-baa6-42a2-8336-2373eba75b55");
        RectangleConstraint c1 = constraint.toUnconstrainedWidth();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "245ac37c-d92b-4cbd-96df-260b94decf46");
        Size2D size1 = arrange(container, g2, c1);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "cab5fa3f-58f4-4656-a2b7-8612d7c2211c");
        if (constraint.getWidthRange().contains(size1.getWidth())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "3870b2a7-f51d-4960-b2d7-cf58b99a97f9");
            return size1;
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "7ddeb2b3-1507-4306-99ab-74d9319278c4");
            double w = constraint.getWidthRange().constrain(size1.getWidth());
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "58a4f346-df2b-4942-a67b-2134d40f4607");
            RectangleConstraint c2 = constraint.toFixedWidth(w);
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "a06b20dd-9e21-48eb-9c7c-68c4db59bfa9");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "f94fa5e0-a8bf-4706-93e8-13c978a4596b");
        RectangleConstraint c1 = constraint.toUnconstrainedHeight();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "ee33e241-e9d1-43e6-9443-c21a57841383");
        Size2D size1 = arrange(container, g2, c1);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "71e58007-c889-48c0-b473-919783a30d73");
        if (constraint.getHeightRange().contains(size1.getHeight())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "f093c6fe-e6a9-4aa5-b034-6d3328afdb56");
            return size1;
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "1eb195b2-6f90-4935-93e3-cb91902c5ae3");
            double h = constraint.getHeightRange().constrain(size1.getHeight());
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "4cc79d22-0e8d-4865-8e7a-a863d3183676");
            RectangleConstraint c2 = constraint.toFixedHeight(h);
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "3681996b-23d3-4f70-8f95-91420af72f78");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "3e4bc30f-58a7-427c-9eab-07c10ed13e8d");
        Size2D size1 = arrange(container, g2, RectangleConstraint.NONE);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "25ec592d-1a85-41b7-9594-687c548a2098");
        if (constraint.getWidthRange().contains(size1.getWidth())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "5fe5737e-1898-4157-a2e9-22fa4f747c7b");
            if (constraint.getHeightRange().contains(size1.getHeight())) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "549def2b-c9c8-499d-bbb4-44a11c22ed90");
                return size1;
            } else {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "057fbe44-c910-41e6-b1b3-71e820c72b23");
                double h = constraint.getHeightRange().constrain(size1.getHeight());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "94a68fc2-bc3b-4e77-8255-b0b7c360e9bb");
                RectangleConstraint cc = new RectangleConstraint(size1.getWidth(), h);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "29bb97f9-7510-4153-a521-af9a1763b792");
                return arrangeFF(container, g2, cc);
            }
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "e563bfec-c191-4b52-8f1f-5342387bc103");
            if (constraint.getHeightRange().contains(size1.getHeight())) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "5a19d69b-8b58-45df-950d-6880e906a162");
                double w = constraint.getWidthRange().constrain(size1.getWidth());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "298181d5-45d9-4585-97d8-1eef22a45948");
                RectangleConstraint cc = new RectangleConstraint(w, size1.getHeight());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "a0a2437f-7707-4330-99c4-a1e274058506");
                return arrangeFF(container, g2, cc);
            } else {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "13fa89b8-35e8-46d2-b9bf-fd79537ebca5");
                double w = constraint.getWidthRange().constrain(size1.getWidth());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "5ed5667c-4807-4085-aeb3-f6757a5e99c0");
                double h = constraint.getHeightRange().constrain(size1.getHeight());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "25ef4e6e-1da2-435b-afe3-ba3aa30531d5");
                RectangleConstraint cc = new RectangleConstraint(w, h);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "bc648317-c2c4-4bcd-bc7e-711836307ecc");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "19aac752-5cf6-4902-aac0-17ec6e479d41");
        double width = constraint.getWidth() / this.columns;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "25f2b021-20c4-4371-a89d-4824e233e4aa");
        RectangleConstraint bc = constraint.toFixedWidth(width);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "ad83e515-81bd-4df7-badc-01aa9355ad38");
        List blocks = container.getBlocks();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "45556db8-6b60-47eb-8f45-587cf9518d9a");
        double maxH = 0.0;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "685e4ce4-e57e-4b72-81a8-8f71c72655ad");
        for (int r = 0; r < this.rows; r++) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "75d113f3-a3a9-4410-a904-751396ac2b3e");
            for (int c = 0; c < this.columns; c++) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "d33eed21-88a5-4cbe-9a05-dcceae30ae65");
                int index = r * this.columns + c;
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "2dfcca2b-7d9b-4ccd-b9db-c0dad34656f0");
                if (index >= blocks.size()) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "c07c7198-8793-429b-a429-9fe8b001e410");
                    break;
                }
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "413cfef7-1e67-4538-b36e-a2cf37a58f05");
                Block b = (Block) blocks.get(index);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "90fcc0a1-f328-4707-8eda-7c889ad5e2a9");
                if (b != null) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "447458ac-b0f7-454e-af8c-5f2944b63adf");
                    Size2D s = b.arrange(g2, bc);
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "88310af9-19d1-43bf-aa8b-1af3e56ce89e");
                    maxH = Math.max(maxH, s.getHeight());
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "03aa2fe1-ed5c-41c3-b309-cee404b2469a");
        RectangleConstraint cc = constraint.toFixedHeight(maxH * this.rows);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "17fe8c38-6ef1-49d0-9741-e3f4856810e1");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "01aa6019-94fb-4b27-9cb9-92b8e7a65971");
        double height = constraint.getHeight() / this.rows;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "f03d5243-90c9-4a14-b0de-dbda48280a2b");
        RectangleConstraint bc = constraint.toFixedHeight(height);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "8fae5586-41c7-449f-add1-55511f1aec1f");
        List blocks = container.getBlocks();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "642f60a7-6aa5-4122-a3a2-e0411867f6ae");
        double maxW = 0.0;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "fc219efe-dfbc-4aa0-96a8-ffdf341414f5");
        for (int r = 0; r < this.rows; r++) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "c59575d7-1377-427b-be72-0b9453bec76b");
            for (int c = 0; c < this.columns; c++) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "6dbcf000-c178-4fec-906d-bcf398a02017");
                int index = r * this.columns + c;
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "406a87b9-ad86-4c1b-a55a-a7bd38802cc1");
                if (index >= blocks.size()) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "c2828001-76d6-4222-9506-8ac6353c1fe8");
                    break;
                }
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "0449a8b8-7d24-49fa-bcb3-5e4dce397601");
                Block b = (Block) blocks.get(index);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "a897a00c-b596-452e-8d2c-44e6ff7e2684");
                if (b != null) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "ac20a2ed-8dbc-47e4-95ec-a48b7b105f11");
                    Size2D s = b.arrange(g2, bc);
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "183aeb02-d133-4246-a678-7756c3c04c97");
                    maxW = Math.max(maxW, s.getWidth());
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "aafa75c1-2b6b-4a32-87e6-4378621bf08c");
        RectangleConstraint cc = constraint.toFixedWidth(maxW * this.columns);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "e596f363-29ad-48ee-b953-49532c0abddf");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "568be310-f0c4-4d59-b2a5-2fe78b1b1335");
        if (obj == this) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "dcebfacd-92ba-4da3-976f-15ddb25fe06d");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "e87859cd-acdf-4367-8f42-1f0d95bcd4be");
        if (!(obj instanceof GridArrangement)) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "8abb2193-132b-470b-9b92-f4b5e41c0b3f");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "08f5d6a8-753e-4294-be16-28987e1d12dd");
        GridArrangement that = (GridArrangement) obj;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "a4acd85f-bec4-423c-9ff2-7346bf1a791e");
        if (this.columns != that.columns) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "2a32c910-5923-4f29-9740-df97d842f61b");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "7381692a-611c-499c-91f6-096f2c330f27");
        if (this.rows != that.rows) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "2d518a97-a099-430a-95af-93755db47ab0");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_9_10.coverage", "140afc0d-3043-4696-9030-62e747a005c0");
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
