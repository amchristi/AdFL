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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "ce88ebac-3f54-401b-ae20-a15dc0b8cd05");
        LengthConstraintType w = constraint.getWidthConstraintType();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "52517fee-660d-40b0-9210-2fedb7ec7b67");
        LengthConstraintType h = constraint.getHeightConstraintType();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "80b3dd2e-b2b5-47fd-9589-c434ac408aa6");
        if (w == LengthConstraintType.NONE) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "e64294a0-33a1-41cb-92fe-fb6ad7528b7d");
            if (h == LengthConstraintType.NONE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "034d5318-326d-44cc-a83f-28211af0eb64");
                return arrangeNN(container, g2);
            } else if (h == LengthConstraintType.FIXED) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "f9508ab1-0590-4495-bfb2-c980c9ef0c63");
                return arrangeNF(container, g2, constraint);
            } else if (h == LengthConstraintType.RANGE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "a538b53e-c98c-4b8c-a4e0-3d032094ca7d");
                return arrangeNR(container, g2, constraint);
            }
        } else if (w == LengthConstraintType.FIXED) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "44e2bd7b-07ab-4c22-8553-91bc4252acba");
            if (h == LengthConstraintType.NONE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "e01565b7-1ee0-4b74-b5dd-859f5de30026");
                return arrangeFN(container, g2, constraint);
            } else if (h == LengthConstraintType.FIXED) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "ca865774-8a59-40ec-8e09-9b7ab1edc405");
                return arrangeFF(container, g2, constraint);
            } else if (h == LengthConstraintType.RANGE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "2a7b6e8f-26b3-41f3-93e6-aa69cde4ec41");
                return arrangeFR(container, g2, constraint);
            }
        } else if (w == LengthConstraintType.RANGE) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "bd05ea5e-fc7c-4f70-8085-3d308a746b1d");
            if (h == LengthConstraintType.NONE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "a4230483-b4b6-4015-9f33-c1debd8a9f08");
                return arrangeRN(container, g2, constraint);
            } else if (h == LengthConstraintType.FIXED) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "01cea2d5-9e61-49d2-b182-177892d1c8df");
                return arrangeRF(container, g2, constraint);
            } else if (h == LengthConstraintType.RANGE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "15dd234f-fe09-4b59-b157-c4f003f04eb1");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "00b607ff-7450-4db0-942b-3e52151aa7aa");
        double maxW = 0.0;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "7539fe74-abb9-4bae-b61c-5d50ca8b0b2e");
        double maxH = 0.0;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "44120b4f-a23f-46ce-8843-9b4ad90da447");
        List blocks = container.getBlocks();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "7af66f8f-2127-4d9c-b98b-fbee60c3f699");
        Iterator iterator = blocks.iterator();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "d5c3fedd-7425-46c9-b41a-b34425dacf9c");
        while (iterator.hasNext()) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "f2a49aed-2bb9-481c-a769-4e43790ba29c");
            Block b = (Block) iterator.next();
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "92eb2d3e-5492-4e19-b59a-182665b043f8");
            if (b != null) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "d0a1b27c-80c0-469a-a126-eb434c6ea7ba");
                Size2D s = b.arrange(g2, RectangleConstraint.NONE);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "cd47dfcb-6e26-429d-80f0-7036a23b3836");
                maxW = Math.max(maxW, s.width);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "e259b1b2-e4a1-4d4f-be90-546ef5a8f6a2");
                maxH = Math.max(maxH, s.height);
            }
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "abb1731e-b285-42e7-9726-22b6aa5f3f35");
        double width = this.columns * maxW;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "757f1921-d3ab-4d7f-9c30-45595650e7e7");
        double height = this.rows * maxH;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "2e7e2f0e-1d22-46c4-894d-0c6f2ba79813");
        RectangleConstraint c = new RectangleConstraint(width, height);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "eb87a3f1-04ec-402c-b0ea-8f21db664eac");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "4ad7b5b7-52f0-4008-81f2-cce8fa670c18");
        double width = constraint.getWidth() / this.columns;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "8671f7fd-51bb-4c2c-9b16-7702a376303e");
        double height = constraint.getHeight() / this.rows;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "5f413f5d-c7d6-4628-978b-751fef8f19fc");
        List blocks = container.getBlocks();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "5b916156-2838-474c-a868-efc91b0eaaac");
        for (int c = 0; c < this.columns; c++) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "d10f6646-460c-4c98-aa23-feb7af5001b5");
            for (int r = 0; r < this.rows; r++) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "db7c4eca-f95e-4542-af37-467538e1126e");
                int index = r * this.columns + c;
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "e741e095-7501-4601-a644-691dcc349a5f");
                if (index >= blocks.size()) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "6cf18c30-d09c-46ef-a654-c432ab6de93f");
                    break;
                }
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "886b1bf2-fcfd-4dd1-95f8-d91caecf14ae");
                Block b = (Block) blocks.get(index);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "072acaae-568b-439f-864f-137d124e041f");
                if (b != null) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "c3ae2f5c-d5f3-4f4f-865d-3347c240aa88");
                    b.setBounds(new Rectangle2D.Double(c * width, r * height, width, height));
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "09069862-42bb-4840-af48-d969d32f3307");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "ff08748c-fcf6-4a7e-b434-b78052c47993");
        RectangleConstraint c1 = constraint.toUnconstrainedHeight();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "f46f55bb-8091-45b2-bb15-7d8a6abb1645");
        Size2D size1 = arrange(container, g2, c1);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "cfada2b2-d9e7-41f2-9c9a-3e6b70c87ae6");
        if (constraint.getHeightRange().contains(size1.getHeight())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "f23a38d2-9f65-4bbc-8fdf-baee29b999ee");
            return size1;
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "c7a0837f-174f-4065-8e27-e7275d380c2b");
            double h = constraint.getHeightRange().constrain(size1.getHeight());
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "f57ad4a2-881b-4f14-a697-8d04d9ebd898");
            RectangleConstraint c2 = constraint.toFixedHeight(h);
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "8d13125a-f508-4372-a4b2-350db073bc2c");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "7a0ea98e-7fd5-4a67-b3c0-dcbd836d17df");
        RectangleConstraint c1 = constraint.toUnconstrainedWidth();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "4009248e-90fa-4206-8d69-cb8078cd6191");
        Size2D size1 = arrange(container, g2, c1);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "e31966fb-b3be-4be5-a907-4c7e5f1bc10c");
        if (constraint.getWidthRange().contains(size1.getWidth())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "f8425c9c-da3a-4166-ae39-977e484b7f34");
            return size1;
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "3bfaa1d5-57ab-4703-a61f-96916cfd342a");
            double w = constraint.getWidthRange().constrain(size1.getWidth());
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "97820ff5-829c-4ea4-b989-bf3ce56054f4");
            RectangleConstraint c2 = constraint.toFixedWidth(w);
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "e2b1f7e8-72bb-4f4d-83f3-bbc101a85c42");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "4b1d4ccd-9b5f-4576-a9fa-281c96402265");
        RectangleConstraint c1 = constraint.toUnconstrainedWidth();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "61cc03a9-5590-4dc7-af7f-ab101f10b094");
        Size2D size1 = arrange(container, g2, c1);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "f8a72856-1050-4b7c-ba19-cb07422a1647");
        if (constraint.getWidthRange().contains(size1.getWidth())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "d069335d-19b2-4fdc-8617-4527e26ddb1a");
            return size1;
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "32a7dccb-5245-4b48-86eb-815f2dac3861");
            double w = constraint.getWidthRange().constrain(size1.getWidth());
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "22e9f466-458d-459b-80f0-8c45f66a4e64");
            RectangleConstraint c2 = constraint.toFixedWidth(w);
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "595da80a-9503-463e-b7fb-d6641c6d0674");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "f042f48f-cfba-44c8-aae2-1f8d33827e1e");
        RectangleConstraint c1 = constraint.toUnconstrainedHeight();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "9bd7ba16-e9a8-4f0f-9bd4-e23018d33ff7");
        Size2D size1 = arrange(container, g2, c1);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "88f4b67c-2e97-42d1-8bb0-c9aacd6eb044");
        if (constraint.getHeightRange().contains(size1.getHeight())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "6eb9ae1c-baf3-4fe4-9696-4f4cd7f37925");
            return size1;
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "7a5be10c-d8e9-4f42-af6b-e90ccd498c59");
            double h = constraint.getHeightRange().constrain(size1.getHeight());
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "4be82f71-f3c8-492e-94a4-12a959bbb30a");
            RectangleConstraint c2 = constraint.toFixedHeight(h);
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "0784c82a-0efa-46c3-9427-4f409aa434f0");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "8009ec69-1633-4336-b0e8-4b5fb823d3ea");
        Size2D size1 = arrange(container, g2, RectangleConstraint.NONE);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "99190323-5d39-412a-aaa2-f9e25e983ac1");
        if (constraint.getWidthRange().contains(size1.getWidth())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "14ed1668-9ac6-44f0-881a-e51c727f3d3c");
            if (constraint.getHeightRange().contains(size1.getHeight())) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "b7851748-4823-4fa7-9701-79d808647fd9");
                return size1;
            } else {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "ada1b26b-c515-48fa-adbf-f7ba046e3da2");
                double h = constraint.getHeightRange().constrain(size1.getHeight());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "4884d560-37dc-4f72-8d49-afb2bef9fa94");
                RectangleConstraint cc = new RectangleConstraint(size1.getWidth(), h);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "d65e331c-f374-4fde-b0ea-c12ff33bbccf");
                return arrangeFF(container, g2, cc);
            }
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "51264c6d-0d83-4ef7-bd0f-676f0ef5b346");
            if (constraint.getHeightRange().contains(size1.getHeight())) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "e98ca9ec-284d-4b91-a323-082744d7cd25");
                double w = constraint.getWidthRange().constrain(size1.getWidth());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "fedd649b-0867-4228-afb2-d3fd79f0299a");
                RectangleConstraint cc = new RectangleConstraint(w, size1.getHeight());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "d7eb3dbf-39ec-41be-a963-4036c4d77f5f");
                return arrangeFF(container, g2, cc);
            } else {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "2e8114d5-6da2-4966-92f3-c0341372fa09");
                double w = constraint.getWidthRange().constrain(size1.getWidth());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "7c56632d-43a6-433a-aa3b-3f6d43781fb9");
                double h = constraint.getHeightRange().constrain(size1.getHeight());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "666e4bae-d80b-4a52-bb93-298e585c4bd4");
                RectangleConstraint cc = new RectangleConstraint(w, h);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "25dd2e02-193c-4d1a-bc69-98f0ecb5500c");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "c0937797-7385-4c2a-9413-759b0457594e");
        double width = constraint.getWidth() / this.columns;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "be6b28c6-a5eb-4774-8b1b-57e3c78497cb");
        RectangleConstraint bc = constraint.toFixedWidth(width);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "9c2a15d9-f0f5-41c5-8d93-d469af7686e1");
        List blocks = container.getBlocks();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "f50f8742-0dad-43cc-b310-38a86dbe6dba");
        double maxH = 0.0;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "f0b83137-3a39-4d77-9574-ad691e78a6a7");
        for (int r = 0; r < this.rows; r++) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "2be403ce-6ee0-4cd0-8487-0f4f9c618e7e");
            for (int c = 0; c < this.columns; c++) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "81394e9c-fc64-4bf7-b62f-42997717de98");
                int index = r * this.columns + c;
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "52c74e29-28a0-4a44-91a0-7a55d76da4f7");
                if (index >= blocks.size()) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "703588b7-235c-4c8b-8c82-b9353c027943");
                    break;
                }
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "9920caca-439e-4c94-b707-cfb8876224c2");
                Block b = (Block) blocks.get(index);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "24fec49c-1d1b-4bce-9c4e-269fe74f09bc");
                if (b != null) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "3c1ae4ae-c9c9-45f7-9070-e82d731fe441");
                    Size2D s = b.arrange(g2, bc);
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "d67cf2af-ee1f-47b0-87fd-b864b8955a58");
                    maxH = Math.max(maxH, s.getHeight());
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "ce38e0dd-1e67-4d73-8462-0f8ca0c82f1a");
        RectangleConstraint cc = constraint.toFixedHeight(maxH * this.rows);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "e5476873-37cf-4d4a-bedd-bb8f00cdf92e");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "17f2caba-ea8b-4362-ace7-bff27352e7c8");
        double height = constraint.getHeight() / this.rows;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "7490ac84-8e21-4436-8834-c7c1a4330c11");
        RectangleConstraint bc = constraint.toFixedHeight(height);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "9c8d158b-9564-44a4-b37c-cf28ccfbd3fc");
        List blocks = container.getBlocks();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "338bcbbe-49a3-4024-af24-72a79cc4e499");
        double maxW = 0.0;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "66320ac0-67ee-4242-a7dd-981721ca9de6");
        for (int r = 0; r < this.rows; r++) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "e7011816-54c4-4655-9d87-53abd7d85af3");
            for (int c = 0; c < this.columns; c++) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "8e17c3a0-aa96-42e9-992a-269d71bc57c3");
                int index = r * this.columns + c;
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "975b1133-7c06-4952-850d-0aad8d4f0d16");
                if (index >= blocks.size()) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "d8c7d0c2-0111-4d6c-9b91-be2ca47a31ba");
                    break;
                }
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "2b84df05-a2df-4b03-a721-28e8a29ce301");
                Block b = (Block) blocks.get(index);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "0e51db29-a222-4c54-b973-77b4feebc917");
                if (b != null) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "de981629-7818-436a-9b53-621eab23e200");
                    Size2D s = b.arrange(g2, bc);
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "898b96a2-4b5c-474d-a8b7-3dbe1cc35468");
                    maxW = Math.max(maxW, s.getWidth());
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "08df77c1-7719-4357-91d5-d5b94f96d6c4");
        RectangleConstraint cc = constraint.toFixedWidth(maxW * this.columns);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "2548e0cf-7636-4d5f-8e4b-a2128e8cab7a");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "fd7f05f4-ec0a-40f0-b409-76f85674f33e");
        if (obj == this) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "99082ca0-9f41-4249-9e39-20aeb0a1a4e5");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "acdf61b1-9361-4504-9c0a-f18329b3e1cd");
        if (!(obj instanceof GridArrangement)) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "884c6fcf-5d07-4182-896f-e5910b86229e");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "5c34d8f9-719a-4570-9795-e6ba94996158");
        GridArrangement that = (GridArrangement) obj;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "181cbd8e-7edd-4d0a-a80f-6c98ae56bca4");
        if (this.columns != that.columns) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "cde5e457-7e46-4faa-9a2c-69e9f6e9e1da");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "4f5fb18e-4f39-4003-a2e9-d31ab8884e45");
        if (this.rows != that.rows) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "1fdba960-e19e-4513-8bb2-a6fff029512e");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_4_10.coverage", "167e15be-248f-454e-ba28-e8961a0894ff");
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
