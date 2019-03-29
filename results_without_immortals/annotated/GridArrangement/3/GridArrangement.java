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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "7301f1c7-1f32-4d85-9357-bcb4cba588b9");
        LengthConstraintType w = constraint.getWidthConstraintType();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "3c0692cb-352d-4079-b948-53356e90ced5");
        LengthConstraintType h = constraint.getHeightConstraintType();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "1783dcb4-358e-4999-9439-49c09c902bb1");
        if (w == LengthConstraintType.NONE) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "ca83adb6-1f7e-4622-a955-ff31648c2f73");
            if (h == LengthConstraintType.NONE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "74888795-0751-4613-bff4-d5d13227dad3");
                return arrangeNN(container, g2);
            } else if (h == LengthConstraintType.FIXED) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "2897a789-f848-4309-9210-02aca300aec8");
                return arrangeNF(container, g2, constraint);
            } else if (h == LengthConstraintType.RANGE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "e8bb6577-07b9-4bfb-82c6-3309b8644e8d");
                return arrangeNR(container, g2, constraint);
            }
        } else if (w == LengthConstraintType.FIXED) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "a7f84ca4-c78e-403d-b01c-0b014d3d2506");
            if (h == LengthConstraintType.NONE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "7533ecfd-5dd6-4e7c-8acb-82eadb5fb4bf");
                return arrangeFN(container, g2, constraint);
            } else if (h == LengthConstraintType.FIXED) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "a2989a01-e370-4445-8b73-4422c139b1e2");
                return arrangeFF(container, g2, constraint);
            } else if (h == LengthConstraintType.RANGE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "aae41754-fe38-46f4-b992-77c70d1d0888");
                return arrangeFR(container, g2, constraint);
            }
        } else if (w == LengthConstraintType.RANGE) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "aca6241b-8441-4e68-b96f-818b1980fcfa");
            if (h == LengthConstraintType.NONE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "fd16a32a-97bc-402e-b1a0-cd5007a5f4d1");
                return arrangeRN(container, g2, constraint);
            } else if (h == LengthConstraintType.FIXED) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "9425b16f-e611-435d-a97d-3596301e80f5");
                return arrangeRF(container, g2, constraint);
            } else if (h == LengthConstraintType.RANGE) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "c02837bc-76c3-49cf-a538-3efacddadfae");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "ec2152b7-3a59-4b61-9306-e48ef99c3c79");
        double maxW = 0.0;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "76559d04-52a5-45cb-a74f-6e4e65c03efe");
        double maxH = 0.0;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "8792714c-bb9d-4d29-9695-9dcac3f3f6b4");
        List blocks = container.getBlocks();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "319b4bc1-3e30-415b-b35c-fe62700bc375");
        Iterator iterator = blocks.iterator();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "4023b718-5b05-4b70-8163-4f07714f1bf8");
        while (iterator.hasNext()) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "e89e7f25-5d22-40ea-bd03-1c2656942c04");
            Block b = (Block) iterator.next();
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "e7a407fd-1d3e-4eb9-a57b-f3f9374143d8");
            if (b != null) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "c412b442-ae97-4b94-82ce-422dd220b372");
                Size2D s = b.arrange(g2, RectangleConstraint.NONE);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "5a805397-f5b6-4f16-9c04-0b7ec56d681d");
                maxW = Math.max(maxW, s.width);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "a585b01b-bfb7-4f29-830b-4fc8fabf7af3");
                maxH = Math.max(maxH, s.height);
            }
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "be0fff46-5d9c-4ac6-9057-c20d2b641948");
        double width = this.columns * maxW;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "5854fccf-1a72-49e3-b426-eaa8e7fc6d8c");
        double height = this.rows * maxH;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "af361352-0999-4a53-b37f-e207da6792ed");
        RectangleConstraint c = new RectangleConstraint(width, height);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "fa31e8a9-6ae3-418d-88a9-d39b2cecabd2");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "7f784860-a882-4e44-b8e1-4da9601a6989");
        double width = constraint.getWidth() / this.columns;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "8a0bc514-8a3e-4d02-8783-b1aa00eb361b");
        double height = constraint.getHeight() / this.rows;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "239f8528-2620-4e27-beaf-cbcbd1e09cf4");
        List blocks = container.getBlocks();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "42538582-551f-441e-89ed-fdc6519bac47");
        for (int c = 0; c < this.columns; c++) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "9ece7f8a-c0dd-442b-9f2b-c5b8c81c039b");
            for (int r = 0; r < this.rows; r++) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "71b15b88-86fe-4da6-98db-937143054bfd");
                int index = r * this.columns + c;
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "99952624-ce3f-4db4-b24c-99570c6ae5d9");
                if (index >= blocks.size()) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "9e54896c-c966-4eb9-be1d-542b99545d9c");
                    break;
                }
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "2448669f-0fd1-4c73-b5a9-d15a270319df");
                Block b = (Block) blocks.get(index);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "b9060641-fd63-49f8-8f53-6c117db95a79");
                if (b != null) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "ca66fc26-16d4-4166-b677-5064ba910501");
                    b.setBounds(new Rectangle2D.Double(c * width, r * height, width, height));
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "08e9c7d9-14a5-4a11-ac23-24c2840ce5d1");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "080dc4ed-f192-4230-aa3d-05759c289030");
        RectangleConstraint c1 = constraint.toUnconstrainedHeight();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "dcf4d3f8-b7cb-4f2b-b9da-2fb97922ea84");
        Size2D size1 = arrange(container, g2, c1);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "da947f6b-bada-45aa-857c-572d27b8b339");
        if (constraint.getHeightRange().contains(size1.getHeight())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "89d3b5ee-2d59-4427-bb1c-8f209bd0045f");
            return size1;
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "d9f56132-0a6b-48c8-a1db-e886c858300c");
            double h = constraint.getHeightRange().constrain(size1.getHeight());
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "b323d8bc-b073-4406-aa98-fe4fd28aff7e");
            RectangleConstraint c2 = constraint.toFixedHeight(h);
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "149e959f-b40f-42a1-ba41-23e913d80727");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "ffee6fd0-d3d3-4d0c-88f2-588312ef27e6");
        RectangleConstraint c1 = constraint.toUnconstrainedWidth();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "081e9b6f-7f44-4066-85af-8327186332b9");
        Size2D size1 = arrange(container, g2, c1);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "1cf85e9c-13b8-47d4-92f0-20db5150bcdf");
        if (constraint.getWidthRange().contains(size1.getWidth())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "3f66beba-5373-4baa-9ddb-fba7135ad343");
            return size1;
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "57ef101b-1dc6-45a2-9dfa-2b71b18040d5");
            double w = constraint.getWidthRange().constrain(size1.getWidth());
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "6b412b0f-c0a2-474b-b905-2d49c9b53612");
            RectangleConstraint c2 = constraint.toFixedWidth(w);
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "80215af4-13dc-4290-8c05-5fdf76073832");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "ce7f7b0e-5a33-493b-ada4-a87468750cf4");
        RectangleConstraint c1 = constraint.toUnconstrainedWidth();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "f201eb80-0ac7-487b-9b66-e85a48a60fa0");
        Size2D size1 = arrange(container, g2, c1);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "e94c765d-31bc-4f97-b3f0-49887539f4c8");
        if (constraint.getWidthRange().contains(size1.getWidth())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "94945e71-45ed-42b7-b0c5-079859f8eae0");
            return size1;
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "085d691a-985c-42e5-b94e-b10bc16a97a4");
            double w = constraint.getWidthRange().constrain(size1.getWidth());
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "92726a64-03cb-4f58-94be-0bae5c8f79d5");
            RectangleConstraint c2 = constraint.toFixedWidth(w);
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "da40f893-6994-4f2a-858c-a04098e26d48");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "155e1a3c-7e32-47db-91c6-d4f2f1758b56");
        RectangleConstraint c1 = constraint.toUnconstrainedHeight();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "0bc12504-29e6-448c-b7ad-4b471d9c6c9d");
        Size2D size1 = arrange(container, g2, c1);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "c8b3916b-a2d8-4a1c-ae80-2ded837f1b8f");
        if (constraint.getHeightRange().contains(size1.getHeight())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "1a92af92-26b9-4774-8648-5d7a66b25d13");
            return size1;
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "fa239026-7a8b-4b14-8356-8c066f1233e6");
            double h = constraint.getHeightRange().constrain(size1.getHeight());
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "93f57430-21f8-4490-8ab4-3eb348bff136");
            RectangleConstraint c2 = constraint.toFixedHeight(h);
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "d7134f9a-6ede-47f4-8017-f11c618a4d52");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "ae563c7e-9b50-4dab-8588-9d950bc1178c");
        Size2D size1 = arrange(container, g2, RectangleConstraint.NONE);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "3adae316-b59e-4f47-99e7-c082cdf58197");
        if (constraint.getWidthRange().contains(size1.getWidth())) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "d75072b7-ca37-4b03-a3d5-19baee0bcc76");
            if (constraint.getHeightRange().contains(size1.getHeight())) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "f6ccde10-5a6d-4de9-a60d-92fb53472429");
                return size1;
            } else {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "6094af8f-c237-496b-b029-c31311c2b351");
                double h = constraint.getHeightRange().constrain(size1.getHeight());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "f60fd8ea-f732-4ef5-9948-6fd906e760bf");
                RectangleConstraint cc = new RectangleConstraint(size1.getWidth(), h);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "5c630548-c3a1-475a-b654-31efab017fb6");
                return arrangeFF(container, g2, cc);
            }
        } else {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "7c02ceac-811b-4d05-95ea-c5d5a0a8eb94");
            if (constraint.getHeightRange().contains(size1.getHeight())) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "6978f3e2-0db7-49ef-9b3e-9eb2442d6094");
                double w = constraint.getWidthRange().constrain(size1.getWidth());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "17184d00-d695-4127-8ec7-a07c1aeb0e54");
                RectangleConstraint cc = new RectangleConstraint(w, size1.getHeight());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "e08f25d1-2ee8-4dc7-ba3a-e566f0475251");
                return arrangeFF(container, g2, cc);
            } else {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "59c785bd-2e47-4bc1-a171-9370d6098f5d");
                double w = constraint.getWidthRange().constrain(size1.getWidth());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "45a3784d-8445-430a-818a-8c6b744e5b68");
                double h = constraint.getHeightRange().constrain(size1.getHeight());
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "7c8a1778-9450-4dad-8b20-4c56374d0013");
                RectangleConstraint cc = new RectangleConstraint(w, h);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "f1be51fb-09c2-4e80-95ec-8a747d06ffa2");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "6ebbc0c6-73a5-4e00-8690-f131584ff633");
        double width = constraint.getWidth() / this.columns;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "958cdebe-c3b0-47d7-9426-289e9c071515");
        RectangleConstraint bc = constraint.toFixedWidth(width);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "4904280d-5c76-4bae-953a-b473e84ee3b5");
        List blocks = container.getBlocks();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "a6db0ae1-5227-4be1-9f70-74b4ed319739");
        double maxH = 0.0;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "a5a872a7-79ee-4177-b5cc-adf8bd256364");
        for (int r = 0; r < this.rows; r++) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "e41b9258-8284-421a-bc0a-7bce2a2516dd");
            for (int c = 0; c < this.columns; c++) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "ce0d435f-2316-4523-bbbc-a8d941c7453b");
                int index = r * this.columns + c;
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "2b19ca18-ae74-402f-9bb7-4623eb64050e");
                if (index >= blocks.size()) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "ca8fdb78-60fa-4675-a1fe-af786f58606c");
                    break;
                }
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "6df08edd-2342-4d7e-921c-0589946bffe2");
                Block b = (Block) blocks.get(index);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "133c75b5-3af7-4180-b527-00302e68a575");
                if (b != null) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "e622d0e3-5fc2-4ee1-b7c5-eaa734b5ee5d");
                    Size2D s = b.arrange(g2, bc);
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "b718ded6-252f-4a39-8599-723837257504");
                    maxH = Math.max(maxH, s.getHeight());
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "139f67b8-0107-4d6a-ab7a-a00986c36c49");
        RectangleConstraint cc = constraint.toFixedHeight(maxH * this.rows);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "3ef57aa2-5ab2-4543-8cb4-5c2cd7a620a2");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "2fa0d814-eb36-41ed-85de-7b8b915c2887");
        double height = constraint.getHeight() / this.rows;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "aa1d0137-f30d-4261-8301-d58658b3dfd4");
        RectangleConstraint bc = constraint.toFixedHeight(height);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "b713317d-1c34-484a-9244-78c3bbe2c730");
        List blocks = container.getBlocks();
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "f4e361d6-68b0-49d5-a0d7-5c4ae9404ab5");
        double maxW = 0.0;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "d1b52136-94fc-4fc0-9770-5f4106769dab");
        for (int r = 0; r < this.rows; r++) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "8e144591-2d3f-4662-8cd2-6a8b34323a51");
            for (int c = 0; c < this.columns; c++) {
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "8da9078b-d3e8-4d11-82e2-ff79f5f49edb");
                int index = r * this.columns + c;
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "dca5afe9-3aae-4051-b1fe-74125053c484");
                if (index >= blocks.size()) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "8e6d8305-fc67-4144-be15-158255f84abf");
                    break;
                }
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "efe7d370-09f1-42e4-8b3a-edf33b302082");
                Block b = (Block) blocks.get(index);
                writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "06c45146-a57b-4014-979b-343ba6399748");
                if (b != null) {
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "6ee82efe-a3ab-468e-92d8-7ffb7fe5c60d");
                    Size2D s = b.arrange(g2, bc);
                    writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "6e9842b1-d88d-4e65-a554-8907829bb778");
                    maxW = Math.max(maxW, s.getWidth());
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "cf88d43f-2129-46bf-b401-c2ecff439501");
        RectangleConstraint cc = constraint.toFixedWidth(maxW * this.columns);
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "d10f1305-3137-4a1d-865a-472760b8e7cb");
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
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "a6a8d573-f15f-4fc3-aa04-7a53849002a4");
        if (obj == this) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "f861c69c-5bb5-4971-8d45-7c191091c455");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "6bd5151e-e27d-4380-ab2e-b98d3c5665de");
        if (!(obj instanceof GridArrangement)) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "a0c22a57-69d1-4529-9fc1-189fd17eae51");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "0ee0d205-3781-448a-a367-999f94440c86");
        GridArrangement that = (GridArrangement) obj;
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "da5f7bda-367c-41a6-b2d4-2eec9267bbaa");
        if (this.columns != that.columns) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "c4e1fa67-e5b8-4793-89b2-cea3b209ec46");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "4584df80-3051-41ee-9bf5-07303b4ca0eb");
        if (this.rows != that.rows) {
            writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "f17433b8-c7f1-49ef-bc70-9f9ec00d7fdf");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/GridArrangement/GridArrangement_3_10.coverage", "331c5a2c-d538-4b63-9d47-307034eb0d71");
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
