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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "f72adf6f-fb93-46a6-a04a-0358fdfdd454");
        LengthConstraintType w = constraint.getWidthConstraintType();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "2806e05e-ddbc-4a85-a2eb-e4dfa907fba9");
        LengthConstraintType h = constraint.getHeightConstraintType();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "9d5dd1ef-eac4-4abf-b6b2-5de5bcd8396f");
        if (w == LengthConstraintType.NONE) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "7f6b1356-8201-4bbb-ab30-53289d9f3706");
            if (h == LengthConstraintType.NONE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "d72187ec-19b2-4935-9a05-8c5a9718a4ca");
                return arrangeNN(container, g2);
            } else if (h == LengthConstraintType.FIXED) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "88627f93-65da-4638-b19b-6caede9e6443");
                return arrangeNF(container, g2, constraint);
            } else if (h == LengthConstraintType.RANGE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "51aedaf6-be27-4c6d-90d5-fc5e02877283");
                return arrangeNR(container, g2, constraint);
            }
        } else if (w == LengthConstraintType.FIXED) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "5bea8aba-5542-4a62-871c-a7123d93cc4a");
            if (h == LengthConstraintType.NONE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "25a0238f-5aa5-4748-bc95-a6ebf0e684b3");
                return arrangeFN(container, g2, constraint);
            } else if (h == LengthConstraintType.FIXED) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "0406ee9f-0b13-4fd4-be3d-1182d758b44d");
                return arrangeFF(container, g2, constraint);
            } else if (h == LengthConstraintType.RANGE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "30fdfae6-6251-4ce8-84ec-d2a5310465c3");
                return arrangeFR(container, g2, constraint);
            }
        } else if (w == LengthConstraintType.RANGE) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "c44bd1a0-db95-4398-b409-bf1785fcf585");
            if (h == LengthConstraintType.NONE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "bfe77bbc-d28c-4ddf-b835-1c2a68bc0892");
                return arrangeRN(container, g2, constraint);
            } else if (h == LengthConstraintType.FIXED) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "7274348e-5bae-451f-931a-4436c1c64f78");
                return arrangeRF(container, g2, constraint);
            } else if (h == LengthConstraintType.RANGE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "8b36867d-c28e-47a4-8862-9c5a24ca529e");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "31845bd1-a1bf-4d39-851a-e4f2a52eec45");
        double maxW = 0.0;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "a4bfbe69-8e77-44fc-9979-1471d0f6632c");
        double maxH = 0.0;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "5ef8e51b-5fe4-4ae8-8bbf-6810a7dd0d5e");
        List blocks = container.getBlocks();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "af2e9a2b-da4e-481c-86cf-9b5c6312d68e");
        Iterator iterator = blocks.iterator();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "4662e791-b5d4-483b-b656-c1d09c82cc28");
        while (iterator.hasNext()) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "960c6d19-bb9d-4aa3-9a34-07437dbc8f60");
            Block b = (Block) iterator.next();
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "4c298879-5107-4b72-a9e3-8a22ae4f1d8b");
            if (b != null) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "54104582-d688-4dbc-8be9-61dd820dad19");
                Size2D s = b.arrange(g2, RectangleConstraint.NONE);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "9c0a9bbd-80c8-497b-9ab8-ee7640acf3a7");
                maxW = Math.max(maxW, s.width);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "9c28019c-fe52-4201-8dbc-0239874a52ae");
                maxH = Math.max(maxH, s.height);
            }
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "ff4fde84-8d69-416e-b7f4-d4184ba92076");
        double width = this.columns * maxW;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "cdf9ddf8-fa70-4f7f-9aa3-c2adacf8d630");
        double height = this.rows * maxH;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "8b834106-9fbc-4f24-b9e3-cb6091d189ca");
        RectangleConstraint c = new RectangleConstraint(width, height);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "0641f0e1-0f4c-4f85-b60d-cc11523e0954");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "2b71d0ae-33b0-42cb-b9c9-1a37ad52411f");
        double width = constraint.getWidth() / this.columns;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "86f5487c-aad8-4612-b70f-9e682acf6add");
        double height = constraint.getHeight() / this.rows;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "061a1a4c-43df-4438-881a-2cbcd7bfcff0");
        List blocks = container.getBlocks();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "4a45442c-3573-417e-9a68-1df7f1ee8a57");
        for (int c = 0; c < this.columns; c++) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "693941ab-f0dd-4ce2-b8ab-84638d042e96");
            for (int r = 0; r < this.rows; r++) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "d6864208-875b-4d30-95f5-6e67e1744e42");
                int index = r * this.columns + c;
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "07ff4c9a-bcfc-4fdc-86df-db25532c277b");
                if (index >= blocks.size()) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "9451c036-91e7-4227-8d56-cdf7d50ec688");
                    break;
                }
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "83bef7ca-4049-4e34-96f2-e823e5ad0ec9");
                Block b = (Block) blocks.get(index);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "7647e12a-71fe-4968-92c1-10e0dbab14f9");
                if (b != null) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "00d6cd8a-a543-46eb-8014-15401f9018a9");
                    b.setBounds(new Rectangle2D.Double(c * width, r * height, width, height));
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "35912cf8-57e6-458d-9136-723331d804cb");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "ff195a5d-2c63-4a2c-9efb-e8ca7959e558");
        RectangleConstraint c1 = constraint.toUnconstrainedHeight();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "acbbe745-30cc-4bce-9d0a-369ac47b0909");
        Size2D size1 = arrange(container, g2, c1);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "2d1ccc19-366e-4766-90ff-bf8ad91ec1b7");
        if (constraint.getHeightRange().contains(size1.getHeight())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "512ce3bd-0566-4fab-b416-1fa7eb0ffa1f");
            return size1;
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "b5633ce1-2cc4-44a3-b649-d088416af10b");
            double h = constraint.getHeightRange().constrain(size1.getHeight());
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "abdb943b-0fc4-40b5-aa1b-c0225e1450a8");
            RectangleConstraint c2 = constraint.toFixedHeight(h);
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "a1b139e4-212e-4fe5-8567-81935e11ac47");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "e27434b5-9e18-4b34-87f0-2b7b623bd885");
        RectangleConstraint c1 = constraint.toUnconstrainedWidth();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "f318d3c1-401c-422a-92c0-57451710f8bc");
        Size2D size1 = arrange(container, g2, c1);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "9e93601e-ce40-4344-8865-ea7d1ced206e");
        if (constraint.getWidthRange().contains(size1.getWidth())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "952c6800-c09a-4e22-bb26-d9780879d981");
            return size1;
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "4cbbcddc-7532-4393-8cb0-5d06500687aa");
            double w = constraint.getWidthRange().constrain(size1.getWidth());
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "4e4e11b0-cf0e-4a85-a796-7ce318f5cd5d");
            RectangleConstraint c2 = constraint.toFixedWidth(w);
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "eb6ff936-d8ba-42a9-b66a-5c6ff7578058");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "af1064ea-711f-4972-b535-9ad4fc578ba1");
        RectangleConstraint c1 = constraint.toUnconstrainedWidth();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "06669739-4cf0-4940-80d9-cb9c4f4dbc84");
        Size2D size1 = arrange(container, g2, c1);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "0279f53a-3309-4f43-a61b-d33293d7c29a");
        if (constraint.getWidthRange().contains(size1.getWidth())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "5e110b89-0a73-408d-84d0-51adce07a984");
            return size1;
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "1b9a4387-c2f7-4aec-87fa-030a828b4237");
            double w = constraint.getWidthRange().constrain(size1.getWidth());
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "346e4d8c-cfec-49a4-a304-65962b0133f1");
            RectangleConstraint c2 = constraint.toFixedWidth(w);
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "5532cd70-3e27-43d1-bb1f-1b683a1f0245");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "5ffab29c-74ae-4c70-8eec-31224a15c756");
        RectangleConstraint c1 = constraint.toUnconstrainedHeight();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "7f5d0d09-ca1b-4c0f-bded-4067c58a326c");
        Size2D size1 = arrange(container, g2, c1);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "ef9a3014-e71a-4fc3-8002-a6f2614ad02a");
        if (constraint.getHeightRange().contains(size1.getHeight())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "dfc32888-87ab-40e9-b013-9d1f122cec0f");
            return size1;
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "ab6e0a96-d02d-4d64-a01f-c289cb50e740");
            double h = constraint.getHeightRange().constrain(size1.getHeight());
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "075a324f-8382-4518-8cb5-364da0af6519");
            RectangleConstraint c2 = constraint.toFixedHeight(h);
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "eb6b96bb-8f88-4c3b-be6a-a7293023fddb");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "ffa0d21c-d3a5-42e3-b9df-197274f40579");
        Size2D size1 = arrange(container, g2, RectangleConstraint.NONE);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "5f3a99a0-a5f5-41a0-b2f0-743c50df9ac3");
        if (constraint.getWidthRange().contains(size1.getWidth())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "a2915586-93d8-4e8a-a66e-839dbc8a6025");
            if (constraint.getHeightRange().contains(size1.getHeight())) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "8fb9268f-8449-4f67-a86a-4296b53eedd5");
                return size1;
            } else {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "2834d1bb-00f2-481d-a3c4-a3266177636b");
                double h = constraint.getHeightRange().constrain(size1.getHeight());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "48f85926-aec3-4844-992f-79402c47f5e2");
                RectangleConstraint cc = new RectangleConstraint(size1.getWidth(), h);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "325af7bf-b9c1-4183-b5cd-45d939c4d9e4");
                return arrangeFF(container, g2, cc);
            }
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "cc98d193-c3fe-484b-bf05-dc7871d3ad2b");
            if (constraint.getHeightRange().contains(size1.getHeight())) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "dd332500-8b10-4209-9409-9220a8ddb036");
                double w = constraint.getWidthRange().constrain(size1.getWidth());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "e268caf7-ce1e-44e5-99c5-7f0747d156c5");
                RectangleConstraint cc = new RectangleConstraint(w, size1.getHeight());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "ccc77856-346d-42ab-b101-a3c1846dfbf1");
                return arrangeFF(container, g2, cc);
            } else {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "69531d87-a4f8-454f-aa11-546be23622dc");
                double w = constraint.getWidthRange().constrain(size1.getWidth());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "95efaa3e-a8e7-467d-b7a7-057b5af473ed");
                double h = constraint.getHeightRange().constrain(size1.getHeight());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "66b89be5-522b-4d87-b510-a02cda79e330");
                RectangleConstraint cc = new RectangleConstraint(w, h);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "e990de47-5371-4f40-96f4-4cd1ce38378d");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "0a56c5f1-8c8d-460f-9635-d216cf356e6b");
        double width = constraint.getWidth() / this.columns;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "f6a0409e-0fc0-4f0a-a571-30185a66131c");
        RectangleConstraint bc = constraint.toFixedWidth(width);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "c0fcb620-c6a7-41a1-8bea-bb3c18c38837");
        List blocks = container.getBlocks();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "98d1627a-5f72-4db1-bd1d-2b4053af7d87");
        double maxH = 0.0;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "d99e871c-9194-464c-8f67-46c34a8c0458");
        for (int r = 0; r < this.rows; r++) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "d6ad5def-6c73-484d-a484-b6f583391942");
            for (int c = 0; c < this.columns; c++) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "bbab1c88-8f86-4e52-988b-2de5458b6d03");
                int index = r * this.columns + c;
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "abd53a6e-7916-47bb-b445-9dafbdd21503");
                if (index >= blocks.size()) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "7f14d526-03ce-4e54-a374-8e2768774147");
                    break;
                }
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "f3cc0a6a-3ba9-45c3-8bca-2d93886fdf11");
                Block b = (Block) blocks.get(index);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "eb6bff73-28d7-4ee9-9a14-98523763935a");
                if (b != null) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "1210fdf3-069f-457a-83d2-0d1871103d1c");
                    Size2D s = b.arrange(g2, bc);
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "50bafae7-72e7-4134-a72d-2967ccb5b01e");
                    maxH = Math.max(maxH, s.getHeight());
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "3b29ee26-407f-448c-9150-5b3324c977bd");
        RectangleConstraint cc = constraint.toFixedHeight(maxH * this.rows);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "03384119-e3d8-4c58-845c-7bc06f8912b4");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "3caa209b-b306-4e16-88b5-0931ac658284");
        double height = constraint.getHeight() / this.rows;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "0f03ab7c-8f44-4aea-b7ee-05dc019da9ee");
        RectangleConstraint bc = constraint.toFixedHeight(height);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "80607962-a8fa-4179-be15-c04409f065b8");
        List blocks = container.getBlocks();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "1b0a77fe-5acb-459c-a2c2-028454d1df23");
        double maxW = 0.0;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "6a9ea59e-6403-4d9e-8602-59ff7a73cc25");
        for (int r = 0; r < this.rows; r++) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "e28e9b45-9305-4972-a6a8-32ad36ba6b48");
            for (int c = 0; c < this.columns; c++) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "76cdd9c4-9c77-4b64-890e-defe786b690b");
                int index = r * this.columns + c;
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "5f50c7b7-ee96-4ebf-bcc9-0dc975a70003");
                if (index >= blocks.size()) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "7c39103b-ecee-4a10-8a9c-8bc3a6148f59");
                    break;
                }
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "6642be8b-d639-45d8-adc5-dae01c038950");
                Block b = (Block) blocks.get(index);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "3c920b25-dc1f-4a97-b1d7-a02c5958fe9e");
                if (b != null) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "49e75cf2-e813-49da-8db0-3edc5b33b2ca");
                    Size2D s = b.arrange(g2, bc);
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "dd1f8339-8679-4b13-9b59-e5fcbc76284a");
                    maxW = Math.max(maxW, s.getWidth());
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "66e33862-53fa-407a-922b-17a196f8cff2");
        RectangleConstraint cc = constraint.toFixedWidth(maxW * this.columns);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "09624861-51e7-485c-9748-40461596408e");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "dd422d3b-ea5d-4282-948e-c2a3b18ec270");
        if (obj == this) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "b3d68c5d-20e3-4e65-b74f-a705aff6056e");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "334c6737-9ec1-42b9-8d4e-5046329cb51f");
        if (!(obj instanceof GridArrangement)) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "279c53ea-52cc-45fa-9fdf-09039eae4a67");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "6ba204ec-5b24-4cdf-a111-58cfb026e312");
        GridArrangement that = (GridArrangement) obj;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "deae810e-2bcb-4b01-aa27-bed0c9ce09df");
        if (this.columns != that.columns) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "dee0130d-24e4-4afd-ad67-1e65f59cbdbc");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "206fa019-32d0-4f0e-a201-5f4b7fe8d9a6");
        if (this.rows != that.rows) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "b7a1bb7f-8963-43e1-bf63-eac214ca461d");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_8_10.coverage", "ed218fc8-3f97-4cb9-aae7-0fe1035d1d46");
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
