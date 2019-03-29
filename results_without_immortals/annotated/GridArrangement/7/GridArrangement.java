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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "a023cb43-c29e-4e15-8313-e47d98c3e6cc");
        LengthConstraintType w = constraint.getWidthConstraintType();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "430758af-a3bd-49f9-91e4-20693d5637a1");
        LengthConstraintType h = constraint.getHeightConstraintType();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "1e025e3d-18fa-4609-83f8-6802236b3773");
        if (w == LengthConstraintType.NONE) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "a69eeb21-7ad5-49b4-857b-e33dae3aa11e");
            if (h == LengthConstraintType.NONE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "e51e3f8d-b470-4e9a-8e57-a9ab2c4597bd");
                return arrangeNN(container, g2);
            } else if (h == LengthConstraintType.FIXED) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "3bdad3c1-0f03-4316-9c57-8e678aee8e64");
                return arrangeNF(container, g2, constraint);
            } else if (h == LengthConstraintType.RANGE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "cdaf9d21-261c-47a9-9f85-8124cf74ff24");
                return arrangeNR(container, g2, constraint);
            }
        } else if (w == LengthConstraintType.FIXED) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "39ffcc6c-1850-41c5-abcd-6627263f00eb");
            if (h == LengthConstraintType.NONE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "2a5cad41-413b-49dc-bd78-f1d54d7a27d8");
                return arrangeFN(container, g2, constraint);
            } else if (h == LengthConstraintType.FIXED) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "ad914b38-42fb-4580-aa57-a32b878390b8");
                return arrangeFF(container, g2, constraint);
            } else if (h == LengthConstraintType.RANGE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "3c121f27-8f66-4e6c-b9c2-1b9b4350b97e");
                return arrangeFR(container, g2, constraint);
            }
        } else if (w == LengthConstraintType.RANGE) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "8d073429-c7b8-434a-a45a-5f86b503ce47");
            if (h == LengthConstraintType.NONE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "2615b42c-4d8c-4451-b1c7-3e4e462a299d");
                return arrangeRN(container, g2, constraint);
            } else if (h == LengthConstraintType.FIXED) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "47ba6af6-218e-4eb2-8c01-a0335eb256dc");
                return arrangeRF(container, g2, constraint);
            } else if (h == LengthConstraintType.RANGE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "4c49ea6e-44f2-40af-8ca2-f00cf8641ab5");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "7bbe893a-8a99-45d5-8131-1f87a2e09d93");
        double maxW = 0.0;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "468f2bc2-50af-49be-815a-fe4b8095567f");
        double maxH = 0.0;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "c221a062-3658-4cf2-b97e-85b3049d2c0a");
        List blocks = container.getBlocks();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "ab17007e-0aae-43ea-997e-2626ff1a3f3f");
        Iterator iterator = blocks.iterator();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "7e6800c2-1774-4bed-bf39-d15ce4c73536");
        while (iterator.hasNext()) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "32a5be5f-8fbb-404b-965c-acb99d1e464a");
            Block b = (Block) iterator.next();
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "0c3179b0-29dc-415d-aa61-543c15309b44");
            if (b != null) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "8fb0db1e-73b0-4ce0-9642-d3b2cc6d335b");
                Size2D s = b.arrange(g2, RectangleConstraint.NONE);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "ee71bdf4-d6eb-4620-9c21-7d0fc4e5fa5c");
                maxW = Math.max(maxW, s.width);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "3eacfc64-e0bd-4d8a-a833-05051b311826");
                maxH = Math.max(maxH, s.height);
            }
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "ea1a7d9e-a8f4-4514-8fac-d3045f732eca");
        double width = this.columns * maxW;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "aebf8703-1db0-4663-99e7-24c235fd2462");
        double height = this.rows * maxH;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "034c5cee-4c6d-4355-bcaa-da92a9afe7ee");
        RectangleConstraint c = new RectangleConstraint(width, height);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "616618ef-4f0e-4b3c-a8ae-5497fc7e3b87");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "7b51eb38-79cb-4a60-b750-1ee32366cdbe");
        double width = constraint.getWidth() / this.columns;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "576dcc02-768a-4b45-bdcb-a7454ae45b1a");
        double height = constraint.getHeight() / this.rows;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "76b92f86-4ea2-417d-976b-55b0d768b357");
        List blocks = container.getBlocks();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "4792890d-30e8-4fec-bc6c-6f700e4c19d4");
        for (int c = 0; c < this.columns; c++) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "bb328348-cdd7-4a87-9a29-fb403a924cff");
            for (int r = 0; r < this.rows; r++) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "8d510f9b-5ad8-4c09-bf7d-67f136e7810a");
                int index = r * this.columns + c;
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "6d1574e4-142a-4f9d-bd9c-1c7533591965");
                if (index >= blocks.size()) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "3b1111aa-98cb-4fb4-bd9f-76d7225abb74");
                    break;
                }
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "b9008195-d66c-443d-959a-24bebc7968f3");
                Block b = (Block) blocks.get(index);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "bd2a93fd-3031-45a5-ab33-3930085ba908");
                if (b != null) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "c48e2cef-a156-4a3f-8fd6-8bab9a189c23");
                    b.setBounds(new Rectangle2D.Double(c * width, r * height, width, height));
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "356d4dbf-0c48-4140-92e8-b05d6e812965");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "dff90ff3-8164-4ba5-a751-4b9e569385d7");
        RectangleConstraint c1 = constraint.toUnconstrainedHeight();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "1f36486e-f11c-4c0d-baab-6a8140508219");
        Size2D size1 = arrange(container, g2, c1);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "b2c4289a-86fb-45c7-8628-605ef1c2dedd");
        if (constraint.getHeightRange().contains(size1.getHeight())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "4f180c38-a6c0-410d-8c45-5df7db16dfbc");
            return size1;
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "f6700a5b-5191-489e-ac04-83bb0ef1dca6");
            double h = constraint.getHeightRange().constrain(size1.getHeight());
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "d38dc64b-c8ae-4393-b149-e8646dcb9cd9");
            RectangleConstraint c2 = constraint.toFixedHeight(h);
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "d7a364ba-813b-40f5-b400-7147928d8c21");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "854efefc-d644-48f1-b70e-35bcf4085e8f");
        RectangleConstraint c1 = constraint.toUnconstrainedWidth();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "a12f737e-03b9-487e-88f2-a9d0fc8168de");
        Size2D size1 = arrange(container, g2, c1);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "ce436a23-dd0c-43c6-a188-62a5e773f10a");
        if (constraint.getWidthRange().contains(size1.getWidth())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "2f24bf0e-6722-4046-854f-403ae83a1d93");
            return size1;
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "ab4ec360-6c57-4908-8c6d-77034950d891");
            double w = constraint.getWidthRange().constrain(size1.getWidth());
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "84857535-a05c-49d0-b90d-97fd51c5ca94");
            RectangleConstraint c2 = constraint.toFixedWidth(w);
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "9531ea47-cfa9-46fe-bf62-11abd097111d");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "9ad20d8e-129b-4079-a1ec-1787d65373ba");
        RectangleConstraint c1 = constraint.toUnconstrainedWidth();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "0f940990-5047-47fe-9e12-b4a548b97417");
        Size2D size1 = arrange(container, g2, c1);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "661ac936-ebfa-4867-8583-7f14d0a7d8d4");
        if (constraint.getWidthRange().contains(size1.getWidth())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "e2c2d42a-67a6-4722-b0fd-e21cdae347eb");
            return size1;
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "c7a164b0-891e-424c-aa5f-5f3361d1b6ed");
            double w = constraint.getWidthRange().constrain(size1.getWidth());
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "77aa77a9-8213-43ba-a8fd-987d3bd31de3");
            RectangleConstraint c2 = constraint.toFixedWidth(w);
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "8c4b4e07-83c1-446d-a6ac-491acbd30627");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "a0b5f5df-6000-4c0c-b3d4-bc04ae64599d");
        RectangleConstraint c1 = constraint.toUnconstrainedHeight();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "6deac899-b64b-436e-8d43-8af8f6cc6317");
        Size2D size1 = arrange(container, g2, c1);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "cbddaad0-7998-447b-ab46-73795212fa47");
        if (constraint.getHeightRange().contains(size1.getHeight())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "d392a85b-293d-410f-97f8-b25c64b42118");
            return size1;
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "d1a6c9f4-76fb-4213-b149-475f5dcd9042");
            double h = constraint.getHeightRange().constrain(size1.getHeight());
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "f6846602-432c-4a7e-ad1c-446ec6f54011");
            RectangleConstraint c2 = constraint.toFixedHeight(h);
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "e446d43a-76bd-44b2-a675-de75b7e70a50");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "05388a7c-c6e8-4366-b03d-aa5f2a14a658");
        Size2D size1 = arrange(container, g2, RectangleConstraint.NONE);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "f838be19-9dd5-41b2-b2eb-082fe12dde3f");
        if (constraint.getWidthRange().contains(size1.getWidth())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "b6fb1518-465c-40e5-8f88-bc65bda148be");
            if (constraint.getHeightRange().contains(size1.getHeight())) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "b3b201b4-5c2c-4a25-b7b1-f4cbea300298");
                return size1;
            } else {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "95d15660-735e-40d2-b116-71a48fd0e1a1");
                double h = constraint.getHeightRange().constrain(size1.getHeight());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "481e3949-ed6d-46e6-b621-0a5c14097363");
                RectangleConstraint cc = new RectangleConstraint(size1.getWidth(), h);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "70a760a9-c574-48ac-a201-8ef566399f2e");
                return arrangeFF(container, g2, cc);
            }
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "cac66450-2d7a-4075-888a-0d4f86c5e778");
            if (constraint.getHeightRange().contains(size1.getHeight())) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "7ccfa9a4-f42e-4aaa-a3e8-2b9fad179786");
                double w = constraint.getWidthRange().constrain(size1.getWidth());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "925e4630-0db7-4671-abea-835dfecee88f");
                RectangleConstraint cc = new RectangleConstraint(w, size1.getHeight());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "2cd7c847-c796-442d-83e9-5befb8b85c44");
                return arrangeFF(container, g2, cc);
            } else {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "508a1451-53d2-4465-bda5-46be48226712");
                double w = constraint.getWidthRange().constrain(size1.getWidth());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "12e1cbc0-857f-46de-8e77-089db9741c5a");
                double h = constraint.getHeightRange().constrain(size1.getHeight());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "60516259-d66d-4bc2-968a-9342c7668520");
                RectangleConstraint cc = new RectangleConstraint(w, h);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "238af802-a66b-4eb9-8d53-e464743fc1bc");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "c2bc2834-9b11-40b6-a007-d35653a782f3");
        double width = constraint.getWidth() / this.columns;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "841e38cf-c69c-4d7d-832e-1b1459e04307");
        RectangleConstraint bc = constraint.toFixedWidth(width);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "b567b32f-ff5e-4aac-b93d-9d493e475d59");
        List blocks = container.getBlocks();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "672b210c-ab44-4d41-87a8-9d0045922144");
        double maxH = 0.0;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "a3d485b0-9683-485e-b059-e7726a5085ad");
        for (int r = 0; r < this.rows; r++) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "baf6e273-cc51-4193-97d0-0e94f82d0cab");
            for (int c = 0; c < this.columns; c++) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "3c7aa5ed-6f1d-48ae-8a24-032fbe2107d9");
                int index = r * this.columns + c;
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "86b86c1f-adb3-4aaa-978f-e5ce09713fe0");
                if (index >= blocks.size()) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "86ee3b25-b616-4dcf-b31e-90c82b6d0178");
                    break;
                }
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "c3d1d35f-1e1a-4caa-9fc8-659c5957ebaa");
                Block b = (Block) blocks.get(index);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "26396e96-832e-4ea2-afb8-e492f024a84a");
                if (b != null) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "2cf77fbd-9549-4273-b41c-12bd5f1a1bb5");
                    Size2D s = b.arrange(g2, bc);
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "43511a98-cf25-49d1-bb0f-1383e9548551");
                    maxH = Math.max(maxH, s.getHeight());
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "3d736272-7b5b-4751-9576-ff07caa87986");
        RectangleConstraint cc = constraint.toFixedHeight(maxH * this.rows);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "709ef2b3-87ed-459f-92ed-2171b1a2ded0");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "70604f6d-7651-41ee-bfa1-d32789611fd3");
        double height = constraint.getHeight() / this.rows;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "c3248bef-ca3d-490e-9ada-c6d97c8d9abe");
        RectangleConstraint bc = constraint.toFixedHeight(height);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "eb2174cb-c039-4f0f-b74a-9a7d50b01c9e");
        List blocks = container.getBlocks();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "98dc31d1-a541-4a3b-8e9d-99b2495f1aef");
        double maxW = 0.0;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "e8e30d2c-6a56-4236-99f5-189d4546bfc8");
        for (int r = 0; r < this.rows; r++) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "c3fb30b6-f8d7-4702-b587-0f6918a1b3db");
            for (int c = 0; c < this.columns; c++) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "43d7516c-90c0-4e26-844f-0e87d352ae00");
                int index = r * this.columns + c;
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "33a80d08-d2b5-4560-9e59-e82923a11517");
                if (index >= blocks.size()) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "83589247-05ba-463d-80d7-923ed5eb0bb0");
                    break;
                }
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "c611cd8e-0c9f-4abf-916c-35d158b78507");
                Block b = (Block) blocks.get(index);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "c9b09ccf-69f1-4036-b9c2-a8d77aedd9ff");
                if (b != null) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "5bff2968-7a4f-44aa-9e55-416d31dcba7a");
                    Size2D s = b.arrange(g2, bc);
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "8af9d8bc-d5b6-41c4-b793-265e0e2ada91");
                    maxW = Math.max(maxW, s.getWidth());
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "02201a89-be21-4b02-923c-8428077d4f06");
        RectangleConstraint cc = constraint.toFixedWidth(maxW * this.columns);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "86358e2f-ce32-46df-8a86-bd9c3cc0d755");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "977cd6b9-7f4b-4779-bf37-ebdcb45d4ea6");
        if (obj == this) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "6c9e22b4-9538-483e-8c06-974533880f8a");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "bd177dbb-c9f2-42e3-9cd6-45c7bc0babcc");
        if (!(obj instanceof GridArrangement)) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "4f60eaf4-1065-4f0e-84a6-90402a49b587");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "926006a3-ba02-4ea7-89d0-42eafe6d7209");
        GridArrangement that = (GridArrangement) obj;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "795ee264-2f82-40de-976f-f15dac96e290");
        if (this.columns != that.columns) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "27f3bcc3-833e-46be-9fa3-22dfdd76193a");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "a2c27350-2853-4bb3-9846-ba055e418c7c");
        if (this.rows != that.rows) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "acb75791-cc63-4cca-a8de-ed1ec49d3b95");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_7_10.coverage", "ed886d32-b9f1-45cc-af18-fe4582858315");
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
