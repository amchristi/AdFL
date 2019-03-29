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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "2d0169f3-3370-45b0-a895-5bd4f5d3968d");
        LengthConstraintType w = constraint.getWidthConstraintType();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "b0487c69-8d39-435e-b4e2-ca04af2d564c");
        LengthConstraintType h = constraint.getHeightConstraintType();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "faeda3d1-9ee3-45a0-bc46-d1275c79ba71");
        if (w == LengthConstraintType.NONE) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "a3f1edfd-f3f7-46d1-87b8-4e28267c6f18");
            if (h == LengthConstraintType.NONE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "1c45bf80-d3b2-4647-bbf5-b96e07433556");
                return arrangeNN(container, g2);
            } else if (h == LengthConstraintType.FIXED) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "edd71f2e-94ef-4a6c-933d-481b1785ee1a");
                return arrangeNF(container, g2, constraint);
            } else if (h == LengthConstraintType.RANGE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "cd4437a2-8a99-4441-b5be-44b3ed484422");
                return arrangeNR(container, g2, constraint);
            }
        } else if (w == LengthConstraintType.FIXED) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "fb094de3-d382-4416-bdf3-9d1fdc58b360");
            if (h == LengthConstraintType.NONE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "7f3e7649-be7d-4288-86ed-44f035474db1");
                return arrangeFN(container, g2, constraint);
            } else if (h == LengthConstraintType.FIXED) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "19a0fe4d-24fd-4f76-8f39-08d6e03feac2");
                return arrangeFF(container, g2, constraint);
            } else if (h == LengthConstraintType.RANGE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "4fefe5d2-15c1-4239-9337-bf591e4761f4");
                return arrangeFR(container, g2, constraint);
            }
        } else if (w == LengthConstraintType.RANGE) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "045d2413-e4ab-4afd-9a20-7b9c4446145a");
            if (h == LengthConstraintType.NONE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "d9fe8e25-c775-495c-acc9-d5c77b6ccfb6");
                return arrangeRN(container, g2, constraint);
            } else if (h == LengthConstraintType.FIXED) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "9c3a709e-280f-48a8-ad01-bd788ca3486e");
                return arrangeRF(container, g2, constraint);
            } else if (h == LengthConstraintType.RANGE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "dcefcb39-0a38-4531-98cc-2bbc63bfa727");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "64d9df3b-386e-4f43-aa85-9001a88691db");
        double maxW = 0.0;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "14ae7f19-2ac9-4e1b-ab1c-59ceda9f852d");
        double maxH = 0.0;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "3a5c28c0-75f0-4136-a2a5-5e410b254f1f");
        List blocks = container.getBlocks();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "d33bdcc7-8785-4d36-b13b-aadca92b31dc");
        Iterator iterator = blocks.iterator();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "8bf682cd-8af0-4a7d-90a5-ab7c501b2252");
        while (iterator.hasNext()) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "9044843c-7f4f-4b07-8c46-5f4162aaa72f");
            Block b = (Block) iterator.next();
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "98eaa1a7-64fa-4393-9019-5dda0336c8ea");
            if (b != null) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "b203dfb8-7686-40d4-abf4-4659e7a3fc2f");
                Size2D s = b.arrange(g2, RectangleConstraint.NONE);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "3ebde8cd-45a3-45e4-aa65-5f26d0306ea9");
                maxW = Math.max(maxW, s.width);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "4185a29e-1032-4711-8e30-fe56427a4544");
                maxH = Math.max(maxH, s.height);
            }
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "5e270ca1-0c42-4e8b-8ce9-033b1c4db5cb");
        double width = this.columns * maxW;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "34c360c3-e0db-4c55-a746-26c63affc519");
        double height = this.rows * maxH;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "400b0ab4-e84c-43d5-9d9f-fa0c25a672ea");
        RectangleConstraint c = new RectangleConstraint(width, height);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "761b2236-268e-4ac0-88a6-4493d40341fa");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "3795a549-ca62-4325-81db-468d86d6e32a");
        double width = constraint.getWidth() / this.columns;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "f446989d-9e5f-414b-956e-5a995547828b");
        double height = constraint.getHeight() / this.rows;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "c5e4e24b-cf17-4263-a4b5-224c4facc5ba");
        List blocks = container.getBlocks();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "53ac9022-0406-4866-844e-43d481c33005");
        for (int c = 0; c < this.columns; c++) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "d27a75fa-e4e3-4c4a-ac1c-59f6db4530d2");
            for (int r = 0; r < this.rows; r++) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "73e880d6-da10-48c4-a762-5145adbe3822");
                int index = r * this.columns + c;
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "0d114b5b-3b88-4c63-9382-36d25dc8a819");
                if (index >= blocks.size()) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "2e6e4c5b-b5dc-4b78-8f21-951f13fa8361");
                    break;
                }
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "00a0e32a-7ddf-4a5f-84b9-3ac33e3223c0");
                Block b = (Block) blocks.get(index);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "0e1fc11d-93a0-47c9-b75a-defd1dabcb59");
                if (b != null) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "e93d9f29-f96a-469a-a79a-b3bd9aa96486");
                    b.setBounds(new Rectangle2D.Double(c * width, r * height, width, height));
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "1f7119b7-f0ec-4d40-a6bc-c651553a347c");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "bf8acf02-a036-44e4-8154-b1b55b15350f");
        RectangleConstraint c1 = constraint.toUnconstrainedHeight();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "46d00526-cf76-4a2a-a208-1e69a99756ce");
        Size2D size1 = arrange(container, g2, c1);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "9f8bcfc6-3e10-48ae-b4b7-20d25a8fc79c");
        if (constraint.getHeightRange().contains(size1.getHeight())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "6c9dc6cc-0027-45a5-8d79-3ec7b08223b2");
            return size1;
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "6018d5b1-7167-4187-a6e8-fd4ed48b9827");
            double h = constraint.getHeightRange().constrain(size1.getHeight());
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "27e2eb3d-60ac-4ab4-813b-c2c47bd28849");
            RectangleConstraint c2 = constraint.toFixedHeight(h);
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "be164e24-ac35-4dc0-abcf-5a6fdd6d420a");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "6bd2fdb5-04b4-4e23-a4b1-fbe924d0f6fb");
        RectangleConstraint c1 = constraint.toUnconstrainedWidth();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "e44cd829-de58-4a03-a2ff-e490f0662478");
        Size2D size1 = arrange(container, g2, c1);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "455e3718-5276-4f92-befa-4aafaa7a08ef");
        if (constraint.getWidthRange().contains(size1.getWidth())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "b1077a0a-dd37-4512-8545-1fdbdd36a9c2");
            return size1;
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "d385668b-a8f2-4d58-b666-f8933c17f500");
            double w = constraint.getWidthRange().constrain(size1.getWidth());
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "a86df4fa-6c1c-486a-b88d-a88c8ab5545c");
            RectangleConstraint c2 = constraint.toFixedWidth(w);
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "e52defbc-266b-4f27-a84a-89c5ef7dd3d3");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "93aad789-9fd3-4719-baf5-afe5b338a572");
        RectangleConstraint c1 = constraint.toUnconstrainedWidth();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "d2d77321-051c-4f85-8188-2af8495e8b86");
        Size2D size1 = arrange(container, g2, c1);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "723fe212-573e-4286-82eb-ddd6af55f2c6");
        if (constraint.getWidthRange().contains(size1.getWidth())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "f994646c-944d-4dd6-9e03-e75f7354f5f0");
            return size1;
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "71e6517a-c9ed-4d1a-a06d-dcf9c91c490d");
            double w = constraint.getWidthRange().constrain(size1.getWidth());
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "ad9a5076-1a69-4ddc-9dc6-e8b7b666c877");
            RectangleConstraint c2 = constraint.toFixedWidth(w);
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "57ab1966-e446-469e-ad30-b9a0be7a3ca4");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "9d21802b-c70c-46d0-a405-f893d5f7a831");
        RectangleConstraint c1 = constraint.toUnconstrainedHeight();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "b39848c7-e916-415c-be51-f4f37f8f2da6");
        Size2D size1 = arrange(container, g2, c1);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "b0515d8e-05bf-4e83-896f-b4e0888afda8");
        if (constraint.getHeightRange().contains(size1.getHeight())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "a3035afd-cb26-4703-919e-059883da25cc");
            return size1;
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "43a0ef3a-60a6-43de-b3f6-1a696a3bd0a6");
            double h = constraint.getHeightRange().constrain(size1.getHeight());
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "4e1f5324-2030-4150-96d8-25c986c0a591");
            RectangleConstraint c2 = constraint.toFixedHeight(h);
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "76560e3f-8514-4486-ac64-b63b91ef1a2c");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "2c08c70c-2214-480f-9a83-5e512562fff6");
        Size2D size1 = arrange(container, g2, RectangleConstraint.NONE);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "319c47f8-6843-48c2-a296-1fe95f89fb79");
        if (constraint.getWidthRange().contains(size1.getWidth())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "09277f80-b4d8-4ee0-a572-da0edda07b0a");
            if (constraint.getHeightRange().contains(size1.getHeight())) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "741a8c7a-9843-4044-99a9-57682f8d4706");
                return size1;
            } else {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "006b0e67-44a8-4cf2-b6e8-397261f88dff");
                double h = constraint.getHeightRange().constrain(size1.getHeight());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "9cb8144f-37c0-44d8-b5eb-f72d78bc4c90");
                RectangleConstraint cc = new RectangleConstraint(size1.getWidth(), h);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "4efa87dc-d780-4284-9155-ede0b63b6987");
                return arrangeFF(container, g2, cc);
            }
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "90a1001a-3680-42c2-9a51-b9b7e56b9165");
            if (constraint.getHeightRange().contains(size1.getHeight())) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "8571c10d-4644-4fce-a57b-d8aa104cd138");
                double w = constraint.getWidthRange().constrain(size1.getWidth());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "d427a5e7-fcf3-442b-9852-7c34696efb0b");
                RectangleConstraint cc = new RectangleConstraint(w, size1.getHeight());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "a4bee5f8-5e12-4e36-a5b8-fadfcb47c9e4");
                return arrangeFF(container, g2, cc);
            } else {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "906afc16-b5ab-401c-9eb5-3af5f0dcd154");
                double w = constraint.getWidthRange().constrain(size1.getWidth());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "4f3911af-dcf5-4c8f-a434-cb560f8b0adb");
                double h = constraint.getHeightRange().constrain(size1.getHeight());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "acfb1690-71d0-49ec-9501-e79bd65b019d");
                RectangleConstraint cc = new RectangleConstraint(w, h);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "dd621040-3484-44c6-98c7-c5ce5d87d71f");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "4282383e-6a4f-4483-b922-223fbb70ac84");
        double width = constraint.getWidth() / this.columns;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "7863520e-a083-413a-8644-2bdfa5de7108");
        RectangleConstraint bc = constraint.toFixedWidth(width);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "c2008a35-2df2-4033-b861-b49bd6271cea");
        List blocks = container.getBlocks();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "8bf27d38-85b7-4924-88e0-95c7ebb38058");
        double maxH = 0.0;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "1442d0f3-09b7-4753-8600-17c864c875a3");
        for (int r = 0; r < this.rows; r++) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "5bec37d9-0a0b-446d-bd5e-26df3c2d1696");
            for (int c = 0; c < this.columns; c++) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "4575891e-4807-4efe-90de-68b5ec699bea");
                int index = r * this.columns + c;
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "61a1affc-fb0a-4d68-8e77-c1a304605ff6");
                if (index >= blocks.size()) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "e0b50bff-adad-45c4-a4de-5648f7c2ca5b");
                    break;
                }
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "d9cb46b0-d2cc-4706-bb3f-d933378df2ef");
                Block b = (Block) blocks.get(index);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "b42adf20-04bf-478a-8e21-3e593d992876");
                if (b != null) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "1014c9e6-a0f7-4f1d-bff0-ed40a72075f2");
                    Size2D s = b.arrange(g2, bc);
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "9de1dd49-e89c-4b8a-8dfd-0bab64d3d3ff");
                    maxH = Math.max(maxH, s.getHeight());
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "46f5b8f2-a302-4fab-9a0a-c7065281fc0a");
        RectangleConstraint cc = constraint.toFixedHeight(maxH * this.rows);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "ac5849dd-4c70-4286-b150-9de5421a3146");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "f24aa3f0-6994-42e0-a577-cef56c948892");
        double height = constraint.getHeight() / this.rows;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "5737cef2-8059-496f-b80f-49fe1f09f2ae");
        RectangleConstraint bc = constraint.toFixedHeight(height);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "29a8f766-0456-43c6-9a25-b333ca180ea7");
        List blocks = container.getBlocks();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "39be008d-944a-4d91-a338-73584dee7539");
        double maxW = 0.0;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "de4aec78-170a-488e-b0e4-d6b61a83a8df");
        for (int r = 0; r < this.rows; r++) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "20ec5895-72f3-41b8-9758-8d3c1a0fc91c");
            for (int c = 0; c < this.columns; c++) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "91f9b8a5-87bf-40ef-87be-0949aac36b7a");
                int index = r * this.columns + c;
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "8ddcb1e2-993c-4f06-971c-04b806077e87");
                if (index >= blocks.size()) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "00a85f55-5430-465e-846b-bd4d9bdf8655");
                    break;
                }
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "73c4db67-ca65-444d-84da-6e246a908599");
                Block b = (Block) blocks.get(index);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "6d4c412e-94a3-46e0-87ab-516c36071999");
                if (b != null) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "1c7a1aa3-e31d-40ba-b897-4b22d4a0a434");
                    Size2D s = b.arrange(g2, bc);
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "102db4dd-9ea8-4771-bc86-bc5be30eb5fa");
                    maxW = Math.max(maxW, s.getWidth());
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "9bec63db-23d7-4d07-8fd6-5fa5be36b97d");
        RectangleConstraint cc = constraint.toFixedWidth(maxW * this.columns);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "0637ec9a-3d44-453a-9ef5-c913265215c5");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "e9b038d3-07f1-472d-80da-d1e35da110a7");
        if (obj == this) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "6e88a0e8-8938-423b-bf31-0d2546967fc0");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "0ceeab86-a85a-4a9f-b7a9-f6cc52f5a839");
        if (!(obj instanceof GridArrangement)) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "3028e47d-9edf-4dfc-a8a7-7e38c6dc419b");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "3536d7b2-3314-472c-b352-0e761cbfa1dc");
        GridArrangement that = (GridArrangement) obj;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "9701a281-ebbd-4785-bf98-43fd073bf07f");
        if (this.columns != that.columns) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "35aa8644-0cde-4696-a785-980a93ce3e68");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "72417ba5-8d95-40e1-a9f3-881a5177e186");
        if (this.rows != that.rows) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "86f5e518-e40e-4653-bf7e-580825661df8");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_1_10.coverage", "f6f413d9-bd53-4aad-87ae-04e1d6bec95a");
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
