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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "33b8bdc6-9fed-4d39-9399-be7f39c3a28c");
        LengthConstraintType w = constraint.getWidthConstraintType();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "86ddfe04-49fa-4c07-aefb-50c0eca7b17a");
        LengthConstraintType h = constraint.getHeightConstraintType();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "4c4ada43-6f4d-43e8-a4db-3d795db6b190");
        if (w == LengthConstraintType.NONE) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "56da9e2d-256c-4647-a774-a2f1c45ddd87");
            if (h == LengthConstraintType.NONE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "10a8e6cc-eb30-4189-9f5c-eeceeb02db73");
                return arrangeNN(container, g2);
            } else if (h == LengthConstraintType.FIXED) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "f04cdb06-d19e-43fa-bc2b-89314b1bb210");
                return arrangeNF(container, g2, constraint);
            } else if (h == LengthConstraintType.RANGE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "a7022df5-5643-4fd9-97fe-c38a69bc6657");
                return arrangeNR(container, g2, constraint);
            }
        } else if (w == LengthConstraintType.FIXED) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "153246d5-dcb9-4f2f-b8cb-d6fd702d29f0");
            if (h == LengthConstraintType.NONE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "1e1b633b-6575-4cc3-9db2-dda45a7ff72f");
                return arrangeFN(container, g2, constraint);
            } else if (h == LengthConstraintType.FIXED) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "0e8008b4-8600-4a06-b9c0-87444cc9a9ac");
                return arrangeFF(container, g2, constraint);
            } else if (h == LengthConstraintType.RANGE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "87552ffc-a100-4bfe-96c2-e4098f00371f");
                return arrangeFR(container, g2, constraint);
            }
        } else if (w == LengthConstraintType.RANGE) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "8d8ae9a5-0ccd-4d67-be0f-dd659646cdef");
            if (h == LengthConstraintType.NONE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "21afa1bf-40d4-4c9e-9393-e5afc36cde2c");
                return arrangeRN(container, g2, constraint);
            } else if (h == LengthConstraintType.FIXED) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "5bbbafb5-ed4b-4de8-ada2-5d073d29b1c8");
                return arrangeRF(container, g2, constraint);
            } else if (h == LengthConstraintType.RANGE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "1520d50d-2c6f-4c9a-aa47-e0d88fdda4e5");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "41de3589-cc47-409c-ad2d-60e6627ec3f4");
        double maxW = 0.0;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "d8566aee-93dc-4b08-82e2-26f26e3f3eca");
        double maxH = 0.0;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "a2a45e4a-03bf-402a-89c8-419162bb112c");
        List blocks = container.getBlocks();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "006ecef6-a24e-4fa8-8f82-1181d3240c7b");
        Iterator iterator = blocks.iterator();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "b28d9444-a06b-4041-87c2-d514cf721398");
        while (iterator.hasNext()) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "0e61d6c9-e349-4780-b053-b9436e732e41");
            Block b = (Block) iterator.next();
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "2c7ebab0-1a04-477a-8f24-3a591c6d2768");
            if (b != null) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "a94e29c3-7292-4e6d-b907-2dc9397281c4");
                Size2D s = b.arrange(g2, RectangleConstraint.NONE);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "794b43a9-0e74-435f-999d-d3c31dd5d0c7");
                maxW = Math.max(maxW, s.width);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "7cbf5635-dc7e-47b6-8051-ed7426defaf4");
                maxH = Math.max(maxH, s.height);
            }
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "e4580995-67d2-4601-ba95-3c427ee68590");
        double width = this.columns * maxW;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "a4fdddc9-bc85-4818-807c-1a653fb5f1cb");
        double height = this.rows * maxH;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "68bbcc75-8aee-429b-bb12-b19e556680de");
        RectangleConstraint c = new RectangleConstraint(width, height);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "55a5c7ee-cc6c-4a9c-87a0-04b1a972994f");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "b6332c44-aa5b-4b98-914d-6683fc466d0a");
        double width = constraint.getWidth() / this.columns;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "7891f9ac-d541-4328-8750-14a318fa781a");
        double height = constraint.getHeight() / this.rows;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "2dc7d7ad-ea68-4a67-acd8-56fa62b61633");
        List blocks = container.getBlocks();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "62ab8b27-c4cd-4043-bee3-42e7324e5ef4");
        for (int c = 0; c < this.columns; c++) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "a04c43c9-182e-4c93-9361-a5fd262724a5");
            for (int r = 0; r < this.rows; r++) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "ee811e9b-060b-4821-b36f-d0437d6ac5f6");
                int index = r * this.columns + c;
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "a16f9915-7d21-4d01-b9c7-450a2054be0c");
                if (index >= blocks.size()) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "3b9dc7ea-997e-432b-b9b4-022c3912f652");
                    break;
                }
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "efff2b91-9ad9-47cc-a4a7-5750744a53ea");
                Block b = (Block) blocks.get(index);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "e25ab82c-bee8-4379-8cd8-fc8e4dab29bf");
                if (b != null) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "21f99bbd-d833-4a96-a334-23a9aa15ea7e");
                    b.setBounds(new Rectangle2D.Double(c * width, r * height, width, height));
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "bf964d32-b8dd-4bd4-b396-a046a1a0b757");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "1fe41bfb-2dbe-48ac-9426-078048029013");
        RectangleConstraint c1 = constraint.toUnconstrainedHeight();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "e44225ac-58c2-4a9b-9720-30692aed0d7a");
        Size2D size1 = arrange(container, g2, c1);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "86b44c79-d5f7-43a9-9899-67c95f07d215");
        if (constraint.getHeightRange().contains(size1.getHeight())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "aac44d8d-08c7-4b17-959f-9979ce9d7678");
            return size1;
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "51629187-7935-4b1d-97b1-14ccc04401de");
            double h = constraint.getHeightRange().constrain(size1.getHeight());
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "c2ce9cd1-18fa-487c-af43-77d770c74111");
            RectangleConstraint c2 = constraint.toFixedHeight(h);
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "9e95f20f-2502-4200-8607-81915eafc293");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "5d074a2d-ef20-4e6e-9702-2cef646ef4c6");
        RectangleConstraint c1 = constraint.toUnconstrainedWidth();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "2ed3d00f-4b6a-409d-a638-652245e73889");
        Size2D size1 = arrange(container, g2, c1);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "e7161efa-b3db-4451-a3b5-2e298e1bb557");
        if (constraint.getWidthRange().contains(size1.getWidth())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "ad17bbd8-42ad-413d-ba37-f9af16cc1921");
            return size1;
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "dce39dec-9dc1-4c04-ac1f-71acd81b63ea");
            double w = constraint.getWidthRange().constrain(size1.getWidth());
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "0bf3837d-f10a-4999-b1d5-566cdf291276");
            RectangleConstraint c2 = constraint.toFixedWidth(w);
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "b90f5084-762f-4eb9-b6e5-e2906e967b8e");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "d60730c1-fad2-4a31-8cb1-dc4adda8470a");
        RectangleConstraint c1 = constraint.toUnconstrainedWidth();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "35b80940-822c-47fe-ac8c-493b0e0955da");
        Size2D size1 = arrange(container, g2, c1);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "f3933b7c-c86f-4d6e-b8b9-7ae741579f42");
        if (constraint.getWidthRange().contains(size1.getWidth())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "e315c270-9126-4bed-af7f-02c29852649a");
            return size1;
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "6adafe59-92af-4b64-a877-6bafb9e2c9f0");
            double w = constraint.getWidthRange().constrain(size1.getWidth());
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "fae283c5-d605-4384-ac12-45a353e25518");
            RectangleConstraint c2 = constraint.toFixedWidth(w);
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "75db32ab-f6b5-47b7-be25-c7de5e6b1fd4");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "e62c9050-0894-4ebe-bafa-ae974319c489");
        RectangleConstraint c1 = constraint.toUnconstrainedHeight();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "8f4d3922-7ec4-4641-9204-46cc292f3db6");
        Size2D size1 = arrange(container, g2, c1);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "5cf501dd-f47c-42dc-81c9-b9adf56b6be1");
        if (constraint.getHeightRange().contains(size1.getHeight())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "2eb6f7fd-f97e-4cfe-92f0-cce851e2b178");
            return size1;
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "23ef477e-5898-4b50-86b0-4238adbd0c4c");
            double h = constraint.getHeightRange().constrain(size1.getHeight());
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "a2e584e8-7f72-4704-9c65-e1e86dad5564");
            RectangleConstraint c2 = constraint.toFixedHeight(h);
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "84ae62eb-965d-482a-896b-2803bd6d0a1a");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "27be0c4f-45ff-4a86-b8ad-ebcbb8a40a3b");
        Size2D size1 = arrange(container, g2, RectangleConstraint.NONE);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "ba1b94e4-b2e2-475e-8f63-bcd39a62ead7");
        if (constraint.getWidthRange().contains(size1.getWidth())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "eaa29ed3-97bb-4143-8561-8fcc0e114278");
            if (constraint.getHeightRange().contains(size1.getHeight())) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "749dd7a6-23f4-4224-9211-8e09a7fe1277");
                return size1;
            } else {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "9715e694-38cb-446b-93c4-9f79afa44629");
                double h = constraint.getHeightRange().constrain(size1.getHeight());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "77f47669-86a4-4b96-9c79-03bfbd709ff6");
                RectangleConstraint cc = new RectangleConstraint(size1.getWidth(), h);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "b8a1f0d9-9795-4673-bf94-ea201eb15763");
                return arrangeFF(container, g2, cc);
            }
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "f4b84e9a-9f5c-418d-a582-e2fca517e010");
            if (constraint.getHeightRange().contains(size1.getHeight())) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "6c719c83-9264-4335-b0ca-43112fd4ff20");
                double w = constraint.getWidthRange().constrain(size1.getWidth());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "4ed3b927-311e-4d27-9e6d-2d5242a6ad94");
                RectangleConstraint cc = new RectangleConstraint(w, size1.getHeight());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "80abf26a-345f-4d5c-9fea-88de96b0abee");
                return arrangeFF(container, g2, cc);
            } else {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "fac1c695-4b3d-4ec9-976e-d64d22b3e914");
                double w = constraint.getWidthRange().constrain(size1.getWidth());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "db428599-7fd3-4637-b92e-fb0b9021ce68");
                double h = constraint.getHeightRange().constrain(size1.getHeight());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "18b5a4e8-5ca8-4347-89cb-5f518da9f67c");
                RectangleConstraint cc = new RectangleConstraint(w, h);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "9b70475b-10b2-43a5-b68b-a95d9652019b");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "a87a6624-7ede-40e1-a6d4-91d898fae7cf");
        double width = constraint.getWidth() / this.columns;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "e71a9e7f-1e98-48a5-90ce-b3211737df1e");
        RectangleConstraint bc = constraint.toFixedWidth(width);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "af0035ae-f202-4818-b72a-5587d8b94ff9");
        List blocks = container.getBlocks();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "49c71fc4-bf2b-4dfd-b6e5-2c8a62636b70");
        double maxH = 0.0;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "0c061ca7-8441-43da-bff1-35ebecbe861e");
        for (int r = 0; r < this.rows; r++) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "c4b98fe3-1552-4f3f-a73c-50a225d24863");
            for (int c = 0; c < this.columns; c++) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "7aec8774-20de-4e2f-861c-d0bf57100630");
                int index = r * this.columns + c;
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "9c03f9db-b8c1-44f5-be9e-d3ccab99320e");
                if (index >= blocks.size()) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "b44d6dba-f7a5-4e74-bde1-25a3ab872939");
                    break;
                }
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "a5a3f9bd-dd5e-4498-b21e-f04a24006c2c");
                Block b = (Block) blocks.get(index);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "dafb0338-df8a-46bf-b764-38784c2caed2");
                if (b != null) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "715bb142-c4bb-4fba-b7a8-c3bd0ceb95d7");
                    Size2D s = b.arrange(g2, bc);
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "f5bbdaa7-8fcf-45a8-a71a-8c9da6ef3364");
                    maxH = Math.max(maxH, s.getHeight());
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "7b3909ef-1038-4f62-8f15-d4e7cfcf33e0");
        RectangleConstraint cc = constraint.toFixedHeight(maxH * this.rows);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "c552039d-499c-43dc-8d02-c8f66ab94253");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "ba3ecb1a-c632-4891-838a-551209184e10");
        double height = constraint.getHeight() / this.rows;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "71c7653a-6508-4c21-917a-0296117370e4");
        RectangleConstraint bc = constraint.toFixedHeight(height);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "aa0a88c3-c0f9-4073-8c16-615f24f82faf");
        List blocks = container.getBlocks();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "92cc43ba-802f-4286-81b0-2bc47f2f5140");
        double maxW = 0.0;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "fc35f879-a14e-49c1-8d07-1a960c3cfc1f");
        for (int r = 0; r < this.rows; r++) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "5329e686-5af3-4b83-ad18-d90a9137bd24");
            for (int c = 0; c < this.columns; c++) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "3aa4e24b-adaa-44fc-ac59-78c522213265");
                int index = r * this.columns + c;
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "58f5155f-f1f7-488f-9591-3858a76a3a7e");
                if (index >= blocks.size()) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "f3cfb9d9-e837-41f1-aae3-5d3ebf6882c4");
                    break;
                }
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "c8bc9582-c5b0-4a94-a623-823494e5fc51");
                Block b = (Block) blocks.get(index);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "bf630543-05ca-4258-a55b-f1f9d56ee780");
                if (b != null) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "60af0bab-4e24-4e86-be41-4e7061ea3cb3");
                    Size2D s = b.arrange(g2, bc);
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "e4a2773d-e967-4978-b63c-84461ced5838");
                    maxW = Math.max(maxW, s.getWidth());
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "5d5d9917-3f75-466c-a9f7-c071b2fd5c4e");
        RectangleConstraint cc = constraint.toFixedWidth(maxW * this.columns);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "fb234972-c7db-4a41-a31e-32fe93aa25de");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "3de43e09-56c1-4828-9a54-6dfcdb06cf45");
        if (obj == this) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "9a745e17-9c4a-4593-8127-83b148ee0465");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "e1c80dc0-4104-46f5-930c-5df20da04948");
        if (!(obj instanceof GridArrangement)) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "1c6e15b1-67a7-4f01-951c-7d0ef95d0068");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "a305d029-fd97-4126-892e-7dc2a82c07c4");
        GridArrangement that = (GridArrangement) obj;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "75b2a136-e62b-4f94-a07d-46887c94bead");
        if (this.columns != that.columns) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "8cd475a4-ee5d-43df-a61e-c2340506dc6b");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "1a5d8f39-130d-4f74-83d0-1e548e403945");
        if (this.rows != that.rows) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "fcc05d7f-6b74-4efb-aa7f-09c06a8bef04");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_2_10.coverage", "2081108f-9f3c-4130-b181-72dd0b916139");
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
