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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "21c91aad-39d6-4f30-a98f-26d62fc3d093");
        LengthConstraintType w = constraint.getWidthConstraintType();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "0f61ca77-4a17-48bf-b930-35440f23c607");
        LengthConstraintType h = constraint.getHeightConstraintType();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "2f3b98d3-373c-4aa9-8e49-de71b873c151");
        if (w == LengthConstraintType.NONE) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "b64ffee3-f95f-4b76-bf0a-baa13e675f0e");
            if (h == LengthConstraintType.NONE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "d0fa00fe-b203-45f9-ac1b-94a648a3ce37");
                return arrangeNN(container, g2);
            } else if (h == LengthConstraintType.FIXED) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "c5173c56-f22b-435f-9c08-d17e8c0f1e97");
                return arrangeNF(container, g2, constraint);
            } else if (h == LengthConstraintType.RANGE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "9ed9c264-56e7-4d13-a0da-04e289152f08");
                return arrangeNR(container, g2, constraint);
            }
        } else if (w == LengthConstraintType.FIXED) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "812b00b7-968e-419b-bea7-f028f2e58f1d");
            if (h == LengthConstraintType.NONE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "25d8a34a-7a15-4d21-9384-6a36a780491e");
                return arrangeFN(container, g2, constraint);
            } else if (h == LengthConstraintType.FIXED) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "0999d089-b582-4792-a2b8-0e306b5df29c");
                return arrangeFF(container, g2, constraint);
            } else if (h == LengthConstraintType.RANGE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "d2bf9ee8-36e3-46b8-88c4-497c5c57912a");
                return arrangeFR(container, g2, constraint);
            }
        } else if (w == LengthConstraintType.RANGE) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "366ef141-c8b4-459c-9cf8-890f6d384249");
            if (h == LengthConstraintType.NONE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "eecd9fb6-9e5b-4c7a-9449-65a8d681002a");
                return arrangeRN(container, g2, constraint);
            } else if (h == LengthConstraintType.FIXED) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "e6f1464b-c6d3-43af-bf7c-acb88c5a70b8");
                return arrangeRF(container, g2, constraint);
            } else if (h == LengthConstraintType.RANGE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "5e8a7e3f-290d-4d41-ab46-849897542fea");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "abbe564a-8841-4963-80f2-5cfe90402a82");
        double maxW = 0.0;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "def9f206-39cf-4168-9210-e45e70a96fca");
        double maxH = 0.0;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "702cae8b-9041-478e-b1ca-778d8b492b7f");
        List blocks = container.getBlocks();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "2e6b025e-a708-4b3f-affe-849fae7cfb8d");
        Iterator iterator = blocks.iterator();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "a9812772-7c33-400f-aabe-988947d81142");
        while (iterator.hasNext()) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "9b0f2891-da78-415c-a101-0d0eaf5c3bc6");
            Block b = (Block) iterator.next();
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "62039b6e-1933-4d7a-805a-c11806e5666b");
            if (b != null) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "db6d0591-2b25-489d-a3be-2c2a69aa88f4");
                Size2D s = b.arrange(g2, RectangleConstraint.NONE);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "8e04b2c1-338f-4b6a-a4d8-9beaf495736b");
                maxW = Math.max(maxW, s.width);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "56b239e8-3666-4104-970e-95b174445b37");
                maxH = Math.max(maxH, s.height);
            }
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "24ba45af-19af-4886-a86e-ce60fcea0e5e");
        double width = this.columns * maxW;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "1f60af87-edfb-4fce-98b4-945d86c2a9db");
        double height = this.rows * maxH;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "5d414461-d1bd-4729-bbf0-645572c2fd98");
        RectangleConstraint c = new RectangleConstraint(width, height);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "467493e3-2527-4412-85a3-fd1ef7969933");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "e8c20e61-9ff9-4ca5-9c86-37539afc56da");
        double width = constraint.getWidth() / this.columns;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "2a8f5a7b-07dd-4129-a2bc-a6fdb3638f71");
        double height = constraint.getHeight() / this.rows;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "a9acdd35-ba61-4b2a-be0c-6cfd0c7efd32");
        List blocks = container.getBlocks();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "55ee63aa-5fb3-4196-93f8-ff369ce5ce38");
        for (int c = 0; c < this.columns; c++) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "503fa884-d938-4504-ad5a-5ff3507b89da");
            for (int r = 0; r < this.rows; r++) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "070e86c1-35b6-4ff3-a59d-0c3a67c9af93");
                int index = r * this.columns + c;
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "bdc7e0de-d4a8-494f-91ec-595139dcfc27");
                if (index >= blocks.size()) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "06ffa783-ce26-4891-882b-f1bd2c6586d5");
                    break;
                }
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "d0d9b052-c9c5-4a6a-aea7-f49392e5160f");
                Block b = (Block) blocks.get(index);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "1350af05-2024-4b47-956c-715019134ac6");
                if (b != null) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "f50c7dae-b456-47c4-b3cb-52461e9b5fea");
                    b.setBounds(new Rectangle2D.Double(c * width, r * height, width, height));
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "eeb8264c-21dd-4dc1-b352-efdc5f9de973");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "e8037131-df99-4f43-9a47-f26df69831ac");
        RectangleConstraint c1 = constraint.toUnconstrainedHeight();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "00412f4a-e5ba-44d4-9f7e-d3af5833ba7b");
        Size2D size1 = arrange(container, g2, c1);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "7939e545-e51a-424f-b76d-069e15d14432");
        if (constraint.getHeightRange().contains(size1.getHeight())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "3a7f7081-34a2-4c43-b84d-64cb48502729");
            return size1;
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "7ad561b0-6ecc-46d0-99bf-91dab4ca7c77");
            double h = constraint.getHeightRange().constrain(size1.getHeight());
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "df5e65de-93ba-4f18-8107-702a552a7490");
            RectangleConstraint c2 = constraint.toFixedHeight(h);
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "70eca408-3125-401f-ac1a-dac19e885e85");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "8f0653ee-ea26-4e9c-ac11-e7594957f624");
        RectangleConstraint c1 = constraint.toUnconstrainedWidth();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "fb6d6da4-9fa8-4f21-907e-345d748ebfce");
        Size2D size1 = arrange(container, g2, c1);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "7982fd5f-fb3b-468a-b375-1b3ff1c325ae");
        if (constraint.getWidthRange().contains(size1.getWidth())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "a753d21f-ad99-44c5-a2e0-7e1ab9ebc93e");
            return size1;
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "fc4271bf-2e41-4ea9-bb93-036a443566d1");
            double w = constraint.getWidthRange().constrain(size1.getWidth());
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "55336e94-bd74-4399-8ae2-0418d77e67c1");
            RectangleConstraint c2 = constraint.toFixedWidth(w);
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "ed172c44-19de-4556-8d88-b52d418b3711");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "bba1d246-4503-4616-b5fc-186771650c54");
        RectangleConstraint c1 = constraint.toUnconstrainedWidth();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "73cd4887-fc19-4660-8c66-d818dbf9cfc9");
        Size2D size1 = arrange(container, g2, c1);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "fa4d3874-c954-4981-8b5b-7f47a61a07f0");
        if (constraint.getWidthRange().contains(size1.getWidth())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "3139e9ec-6b06-4b6c-b2ff-756ab4c3b0c6");
            return size1;
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "21daab15-17f5-405a-936d-d0350237f5f5");
            double w = constraint.getWidthRange().constrain(size1.getWidth());
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "3ad17f0f-5ad0-4590-ab37-6e1e59100ecb");
            RectangleConstraint c2 = constraint.toFixedWidth(w);
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "2c9732b0-4113-437e-88d9-74c98f91093e");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "d84fb18f-57fd-4aa4-a393-ef98809c363a");
        RectangleConstraint c1 = constraint.toUnconstrainedHeight();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "15524edd-bc18-4532-b158-020707ffca2e");
        Size2D size1 = arrange(container, g2, c1);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "1473eeb1-364d-4a37-bcc7-1301f58f0fe6");
        if (constraint.getHeightRange().contains(size1.getHeight())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "bd575684-ac96-47f5-bf0d-ddd82765071d");
            return size1;
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "2a8b31e5-f5cd-4217-9a43-6c246781d8ac");
            double h = constraint.getHeightRange().constrain(size1.getHeight());
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "1b067ee2-2f77-42ae-b2ca-a8f4220c351b");
            RectangleConstraint c2 = constraint.toFixedHeight(h);
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "f71d5c65-79c7-4910-91c9-1e20b22244ee");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "8c5466d7-6336-456b-afd2-0c689faadf93");
        Size2D size1 = arrange(container, g2, RectangleConstraint.NONE);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "47244982-1cec-421b-b4e3-c404d3a95c7b");
        if (constraint.getWidthRange().contains(size1.getWidth())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "4b1e61af-d481-465a-86c2-63e1461d4214");
            if (constraint.getHeightRange().contains(size1.getHeight())) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "a5a03b77-4171-4540-99e0-eb0c8d3af562");
                return size1;
            } else {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "19462777-001e-4b31-9195-7ae69d1bce2f");
                double h = constraint.getHeightRange().constrain(size1.getHeight());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "4cc32d9a-3f31-4411-8ac6-ac19ae5cacf3");
                RectangleConstraint cc = new RectangleConstraint(size1.getWidth(), h);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "fd76c485-11e8-472e-a3b4-ffa909a433ec");
                return arrangeFF(container, g2, cc);
            }
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "de72ef66-f85e-4c4c-a96e-f4106988d9eb");
            if (constraint.getHeightRange().contains(size1.getHeight())) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "f85119f2-623a-4235-9d6b-5bb0060a72f1");
                double w = constraint.getWidthRange().constrain(size1.getWidth());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "6d3cdc79-814e-4a43-ac37-d4d31f4c0ce4");
                RectangleConstraint cc = new RectangleConstraint(w, size1.getHeight());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "8c99765c-c3b2-41b7-bb0a-d059ed9bdb8c");
                return arrangeFF(container, g2, cc);
            } else {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "181d7c92-7e87-4b4b-ad9d-c8684a9210b8");
                double w = constraint.getWidthRange().constrain(size1.getWidth());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "5dd352a8-ff43-4419-bf42-4c02d656989e");
                double h = constraint.getHeightRange().constrain(size1.getHeight());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "5e5f8145-59d9-41dd-bd6a-3a0bad44655e");
                RectangleConstraint cc = new RectangleConstraint(w, h);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "368ad28f-3dfb-445f-9965-64f529991ac1");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "72d59ab5-be4a-4863-b748-1ecf24b3b8ab");
        double width = constraint.getWidth() / this.columns;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "2580e995-4a98-472e-8df0-f14753e789cd");
        RectangleConstraint bc = constraint.toFixedWidth(width);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "6d988d55-738e-420e-80ab-ff6458737881");
        List blocks = container.getBlocks();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "260a2097-f951-4ef9-b826-324fe8efa407");
        double maxH = 0.0;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "f5b828ec-a503-4149-bc72-3b45b0ff9dde");
        for (int r = 0; r < this.rows; r++) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "7c20fe84-55ea-430c-8c06-5bf73e1ff4c6");
            for (int c = 0; c < this.columns; c++) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "c647dba7-c9aa-4a37-9f88-55462714771f");
                int index = r * this.columns + c;
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "adb7d2c1-018d-4f8e-a506-3d972ff45d61");
                if (index >= blocks.size()) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "63ae3bdf-b596-4995-830e-fc40953870e7");
                    break;
                }
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "555f095b-6903-47ea-83a6-60387f1280c4");
                Block b = (Block) blocks.get(index);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "0cea55fc-5ef3-4fd9-8f39-63fe1c4e00f4");
                if (b != null) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "ec5b21c8-c980-48e8-bcc6-8c4ac3c65778");
                    Size2D s = b.arrange(g2, bc);
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "ae007d87-0404-49be-aa48-1fb69c980583");
                    maxH = Math.max(maxH, s.getHeight());
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "ddd62eb2-0813-4f0c-a027-9dd3cf293600");
        RectangleConstraint cc = constraint.toFixedHeight(maxH * this.rows);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "0c6b6c4c-ccdc-4362-b9e4-d6ee8af2c7b8");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "e630e786-0a99-45c3-90ac-1534a9683827");
        double height = constraint.getHeight() / this.rows;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "e1a8c28e-066f-4608-8e2d-bb649e872759");
        RectangleConstraint bc = constraint.toFixedHeight(height);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "0eeca99b-64b1-4a34-8b9e-3aa9d2ce4f1b");
        List blocks = container.getBlocks();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "5b18fdbe-45b8-48a9-b4f3-abc18a2bf6be");
        double maxW = 0.0;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "697b4f5f-8e5a-4735-bef4-a2cef9407be1");
        for (int r = 0; r < this.rows; r++) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "cf085c08-81d0-4506-8faa-632814992c33");
            for (int c = 0; c < this.columns; c++) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "44f8c6fd-c3ab-4963-8821-2aaf38355487");
                int index = r * this.columns + c;
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "790d0959-f1b0-4b73-a5bb-d1ccae5cbd54");
                if (index >= blocks.size()) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "b9194090-bd72-42ee-ab48-8abd7436e37e");
                    break;
                }
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "aac0c096-3b35-4d2d-b618-bb9814d0dc8a");
                Block b = (Block) blocks.get(index);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "35dcedbd-9725-494b-84ef-7f8f2a1c814f");
                if (b != null) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "8f31e3f4-76f8-4fe8-bd7c-e9e0e9d1a72e");
                    Size2D s = b.arrange(g2, bc);
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "2f28753e-bf11-4e19-af59-9918253d7a96");
                    maxW = Math.max(maxW, s.getWidth());
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "0be20aea-fea7-492c-b97f-04967e52ca2e");
        RectangleConstraint cc = constraint.toFixedWidth(maxW * this.columns);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "07c34f4e-c334-497e-bdb9-59b77c1ba835");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "42eb18a5-e6e0-4935-953e-ae4354bb935e");
        if (obj == this) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "9d0cdc76-8fdb-4afe-bd78-5b85908f774f");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "8ab012f2-3dad-45b9-87e7-31de3ea01372");
        if (!(obj instanceof GridArrangement)) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "40b10a5e-6350-4fb0-b1da-90cec3441cf8");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "067de0a9-de25-4d45-8022-8f26aa900ec9");
        GridArrangement that = (GridArrangement) obj;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "ad96d42b-65e0-4f73-bb2a-067086f82a49");
        if (this.columns != that.columns) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "8207c7b3-ed3e-4262-8513-7998c3af6c09");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "eafa3cab-b186-4113-a0cb-d1f493185f56");
        if (this.rows != that.rows) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "821768c7-0fec-4e6e-b6d3-ad6793b66b3a");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_10_10.coverage", "076d43eb-9a3b-44ff-8d3d-4d0a06c0eb58");
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
