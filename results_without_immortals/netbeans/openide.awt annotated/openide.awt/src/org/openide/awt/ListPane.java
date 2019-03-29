/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2010 Oracle and/or its affiliates. All rights reserved.
 *
 * Oracle and Java are registered trademarks of Oracle and/or its affiliates.
 * Other names may be trademarks of their respective owners.
 *
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common
 * Development and Distribution License("CDDL") (collectively, the
 * "License"). You may not use this file except in compliance with the
 * License. You can obtain a copy of the License at
 * http://www.netbeans.org/cddl-gplv2.html
 * or nbbuild/licenses/CDDL-GPL-2-CP. See the License for the
 * specific language governing permissions and limitations under the
 * License.  When distributing the software, include this License Header
 * Notice in each file and include the License file at
 * nbbuild/licenses/CDDL-GPL-2-CP.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the GPL Version 2 section of the License file that
 * accompanied this code. If applicable, add the following below the
 * License Header, with the fields enclosed by brackets [] replaced by
 * your own identifying information:
 * "Portions Copyrighted [year] [name of copyright owner]"
 *
 * Contributor(s):
 *
 * The Original Software is NetBeans. The Initial Developer of the Original
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2006 Sun
 * Microsystems, Inc. All Rights Reserved.
 *
 * If you wish your version of this file to be governed by only the CDDL
 * or only the GPL Version 2, indicate your decision by adding
 * "[Contributor] elects to include this software in this distribution
 * under the [CDDL or GPL Version 2] license." If you do not indicate a
 * single choice of license, a recipient has the option to distribute
 * your version of this file under either the CDDL, the GPL Version 2 or
 * to extend the choice of license to its licensees as provided above.
 * However, if you add GPL Version 2 code and therefore, elected the GPL
 * Version 2 license, then the option applies only if the new code is
 * made subject to such option by the copyright holder.
 */
package org.openide.awt;

import java.awt.*;
import java.awt.event.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.Serializable;
import java.util.Vector;
import javax.swing.*;
import javax.swing.event.*;
import java.io.*;

/**
 * ListPane. This component is derived from JList component and enables
 * list objects in several columns.
 *
 * @author   Petr Hamernik, Ian Formanek, Jaroslav Tulach
 * @deprecated This class does nothing interesting which cannot be
 * done in a more reliable, scalable way with a JTable.
 */
@Deprecated
public class ListPane extends JList {

    /**
     * generated Serialized Version UID
     */
    static final long serialVersionUID = 3828318151121500783L;

    private int fixedCellWidth = 100;

    private int fixedCellHeight = 100;

    private int visibleRowCount = 6;

    private int visibleColumnCount = 4;

    private int realRowCount = 1;

    private int realColumnCount = 1;

    ListDataListener dataL;

    PropertyChangeListener propertyL;

    InputListener inputL;

    ListSelectionListener selectionL;

    boolean updateLayoutStateNeeded = true;

    /**
     * Construct a JList that displays the elements in the specified,
     * non-null model.  All JList constructors delegate to this one.
     */
    public ListPane(ListModel dataModel) {
        super(dataModel);
        addListListeners();
    }

    /**
     * Construct a JList that displays the elements in the specified
     * array.  This constructor just delegates to the ListModel
     * constructor.
     */
    public ListPane(final Object[] listData) {
        this(new AbstractListModel() {

            public int getSize() {
                return listData.length;
            }

            public Object getElementAt(int i) {
                return listData[i];
            }
        });
    }

    /**
     * Construct a JList that displays the elements in the specified
     * Vector.  This constructor just delegates to the ListModel
     * constructor.
     */
    public ListPane(final Vector listData) {
        this(new AbstractListModel() {

            public int getSize() {
                return listData.size();
            }

            public Object getElementAt(int i) {
                return listData.elementAt(i);
            }
        });
    }

    /**
     * Constructs a JList with an empty model.
     */
    public ListPane() {
        this(new AbstractListModel() {

            public int getSize() {
                return 0;
            }

            public Object getElementAt(int i) {
                return null;
            }
        });
    }

    /**
     * JList components are always opaque.
     * @return true
     */
    public boolean isOpaque() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7d05972e-2fc6-4c9c-8d0a-9b2b986c1504");
        return true;
    }

    /**
     * Return the value of the visibleRowCount property.
     * @see #setVisibleRowCount
     */
    public int getVisibleColumnCount() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7dbb2560-4275-41a5-8358-9f74569c21b1");
        return visibleColumnCount;
    }

    /**
     * Set the preferred number of rows in the list that are visible within
     * the nearest JViewport ancestor, if any.  The value of this property
     * only affects the value of the JLists preferredScrollableViewportSize.
     * <p>
     * The default value of this property is 8.
     * <p>
     * This is a JavaBeans bound property.
     *
     * @see #getVisibleRowCount
     * @see JComponent#getVisibleRect
     */
    public void setVisibleColumnCount(int visibleColumnCount) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e742974a-f041-46fb-b593-c107d339070d");
        int oldValue = this.visibleColumnCount;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ac64cbc2-025f-499d-b3e9-cf88d59402b1");
        this.visibleColumnCount = Math.max(0, visibleColumnCount);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ddb327f5-892f-4b21-ac2d-2fac4e8b597f");
        // NOI18N
        firePropertyChange("visibleColumnCount", oldValue, visibleColumnCount);
    }

    /**
     * If this JList is being displayed withing a JViewport and the
     * specified cell isn't completely visible, scroll the viewport.
     *
     * @param index The index of the cell to make visible
     * @see JComponent#scrollRectToVisible
     * @see #getVisibleRect
     */
    public void ensureIndexIsVisible(int index) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "553b6062-55b5-4098-b61a-6d0c8b9cfb94");
        Point first = indexToLocation(index);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "676b24c6-e6f1-4140-b376-419a3507ba78");
        if (first != null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "78497e49-5d05-423b-9673-49df4d810443");
            Rectangle cellBounds = new Rectangle(first.x, first.y, fixedCellWidth, fixedCellHeight);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7113c947-ad89-4f5f-863a-130ecd648532");
            scrollRectToVisible(cellBounds);
        }
    }

    /**
     * Convert a point in JList coordinates to the index
     * of the cell at that location.  Returns -1 if there's no
     * cell the specified location.
     *
     * @param location The JList relative coordinates of the cell
     * @return The index of the cell at location, or -1.
     */
    public int locationToIndex(Point location) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2953fb6f-93a2-44ef-8a3a-f538d5ce50c0");
        int x = location.x / fixedCellWidth;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9e96cf76-3d2a-4243-bacd-50debb94879c");
        if (x >= realColumnCount) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c16a0ad1-24f6-46e3-8e52-833a5c666a72");
            return -1;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5d5ea5e5-847d-4edb-9257-301fbd7a1a7b");
        int y = location.y / fixedCellHeight;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c7885cf1-2255-4c5b-8e12-688efcf81f40");
        if (y >= realRowCount) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a3230cd4-812c-4cba-a610-545c6c531b8e");
            return -1;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "37062417-67b6-4dae-b27e-0fd1512c6b66");
        int ret = (y * realColumnCount) + x;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a00f60a7-678e-4df5-ac9e-4ecc06e3ce24");
        return (ret >= getModel().getSize()) ? (-1) : ret;
    }

    /**
     * Returns the origin of the specified item in JList
     * coordinates, null if index isn't valid.
     *
     * @param index The index of the JList cell.
     * @return The origin of the index'th cell.
     */
    public Point indexToLocation(int index) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1b72df37-1428-48fb-a641-34b9f2bc86f7");
        if (index >= getModel().getSize()) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "500d25bc-fc63-41d2-a968-2c6f6f5c18cd");
            return null;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "531b8b68-3ee0-4871-93e7-72d1ae026263");
        int y = index / realColumnCount;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e8f1910a-dd4e-4676-a754-2fdf71a16fd8");
        int x = index % realColumnCount;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "542aa1f5-bd92-4d41-8fa6-0f062b41f694");
        return new Point(x * fixedCellWidth, y * fixedCellHeight);
    }

    /**
     * Returns the bounds of the specified item in JList
     * coordinates, null if index isn't valid.
     *
     * @param index1 start index of the JList cell.
     * @param index2 end index of the JList cell.
     * @return The bounds of the index'th cell.
     */
    public Rectangle getCellBounds(int index1, int index2) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c442096e-71ca-4044-9406-2a85abd6627c");
        /*
        int minIndex = Math.min(index1, index2);
        int maxIndex = Math.max(index1, index2);
         */
        Point p1 = indexToLocation(index1);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4e309626-c7a8-4ca0-8823-fddf8b5dcf2f");
        Point p2 = indexToLocation(index2);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "40b6344f-6667-42bb-918c-6ac9ed3b2edb");
        int x1 = p1.x;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fe17c4c0-10ee-4d05-80e0-9f9d44aefdb4");
        int y1 = p1.y;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "edd3e43f-f1e4-4ce9-a90f-35b5b7af03ea");
        int x2 = p2.x + fixedCellWidth;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6bb71c20-14b3-4196-8b6b-fc23b8da14d9");
        int y2 = p2.y + fixedCellHeight;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "202808f8-4e67-45a2-9916-da57fc0b46de");
        if (p1.y != p2.y) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c8a92a24-a421-4c9d-8d40-de8ceb366638");
            x1 = 0;
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "70f51be1-f9dd-4f00-aeb1-36fe24e04de6");
            x2 = fixedCellWidth * realColumnCount;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "09c060d0-3109-4f90-83da-ac83e37586b6");
        return new Rectangle(x1, y1, x2 - x1, y2 - y1);
    }

    /**
     * --- The Scrollable Implementation ---
     */
    // @see #setPrototypeCellValue
    /**
     * Compute the size of the viewport needed to display visibleRowCount
     * rows.  This is trivial if fixedCellWidth and fixedCellHeight
     * were specified.  Note that they can specified implicitly with
     * the prototypeCellValue property.  If fixedCellWidth wasn't specified,
     * it's computed by finding the widest list element.  If fixedCellHeight
     * wasn't specified then we resort to heuristics:
     * <ul>
     * <li>
     * If the model isn't empty we just multiply the height of the first row
     * by visibleRowCount.
     * <li>
     * If the model is empty, i.e. JList.getModel().getSize() == 0, then
     * we just allocate 16 pixels per visible row, and 100 pixels
     * for the width (unless fixedCellWidth was set), and hope for the best.
     * </ul>
     *
     * @see #getPreferredScrollableViewportSize
     */
    public Dimension getPreferredScrollableViewportSize() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bc0e95f8-10df-473d-bcb6-ab8290fc16bd");
        Insets insets = getInsets();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "40d4a374-88b6-4544-9cbb-a0f996c63a33");
        int w = insets.left + insets.right + (visibleColumnCount * fixedCellWidth);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5df05702-c0d9-4172-9683-2e544dc9fe18");
        int h = insets.top + insets.bottom + (visibleRowCount * fixedCellHeight);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d6c84646-73e6-42a8-9468-d3ae89c925ff");
        Dimension dim = new Dimension(w, h);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cddb85f0-c1c2-49bc-b4bc-f26cf982bfa9");
        return dim;
    }

    /**
     * If we're scrolling downwards (<code>direction</code> is
     * greater than 0), and the first row is completely visible with respect
     * to <code>visibleRect</code>, then return its height.  If
     * we're scrolling downwards and the first row is only partially visible,
     * return the height of the visible part of the first row.  Similarly
     * if we're scrolling upwards we return the height of the row above
     * the first row, unless the first row is partially visible.
     *
     * @return The distance to scroll to expose the next or previous row.
     * @see Scrollable#getScrollableUnitIncrement
     */
    public int getScrollableUnitIncrement(Rectangle visibleRect, int orientation, int direction) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4e07f874-067e-4dd8-9596-7702d1ebef80");
        if (orientation == SwingConstants.HORIZONTAL) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d66dd765-55b4-41d0-9ba9-f5e727cb3f29");
            return 1;
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b0fd2580-f553-498a-b90d-18b089915961");
            int row = getFirstVisibleIndex();
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f049bdba-e225-4130-9752-c37b910e16ff");
            if (row == -1) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e0824d7a-7db5-4ceb-8750-64b6377ff929");
                return 0;
            } else {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cec15a9d-3c01-43d8-9dd1-d519ced3f8ff");
                /* Scroll Down */
                if (direction > 0) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8fce4c0c-50be-4013-a4b3-5a947161f6c8");
                    Rectangle r = getCellBounds(row, row);
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1bdeba8c-0f08-4dd3-a27f-536a28a42c74");
                    return (r == null) ? 0 : (r.height - (visibleRect.y - r.y));
                } else /* Scroll Up */
                {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f01a9e6b-9c81-4512-a10e-1bfb7b794f3e");
                    Rectangle r = getCellBounds(row, row);
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5df81ec3-68ed-49ba-9f80-27c3fd206e53");
                    /* The first row is completely visible and it's row 0.
                     * We're done.
                     */
                    if ((r.y == visibleRect.y) && (row == 0)) {
                        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b4ec63da-ede5-461c-8af6-b543afc3ca3a");
                        return 0;
                    } else /* The first row is completely visible, return the
                     * height of the previous row.
                     */
                        if (r.y == visibleRect.y) {
                            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "93e3f413-3d17-4e26-80a0-e2d117d72a52");
                            Rectangle prevR = getCellBounds(row - 1, row - 1);
                            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cb5cf35c-7be5-432a-9f30-89dfbaf057ab");
                            return (prevR == null) ? 0 : prevR.height;
                        } else /* The first row is partially visible, return the
                     * height of hidden part.
                     */
                        {
                            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4de99a0d-6acd-44cc-b3e9-2ed79c6fdb61");
                            return visibleRect.y - r.y;
                        }
                }
            }
        }
    }

    /**
     * @return The visibleRect.height or visibleRect.width per the orientation.
     * @see Scrollable#getScrollableUnitIncrement
     */
    public int getScrollableBlockIncrement(Rectangle visibleRect, int orientation, int direction) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3bdc31ec-cd94-4cfe-8cbe-1cbfb1e46fbc");
        return (orientation == SwingConstants.VERTICAL) ? visibleRect.height : visibleRect.width;
    }

    /**
     * If this JList is displayed in a JViewport, don't change its width
     * when the viewports width changes.  This allows horizontal
     * scrolling if the JViewport is itself embedded in a JScrollPane.
     *
     * @return False - don't track the viewports width.
     * @see Scrollable#getScrollableTracksViewportWidth
     */
    public boolean getScrollableTracksViewportWidth() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a22fdf39-d61c-4f45-8a2f-8a9769ab275f");
        return true;
    }

    /**
     * If this JList is displayed in a JViewport, don't change its height
     * when the viewports height changes.  This allows vertical
     * scrolling if the JViewport is itself embedded in a JScrollPane.
     *
     * @return False - don't track the viewports width.
     * @see Scrollable#getScrollableTracksViewportWidth
     */
    public boolean getScrollableTracksViewportHeight() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0d632998-82d5-4e8b-93a8-abb97d3a6784");
        return false;
    }

    /**
     * If the list is opaque, paint its background.
     * Subclasses may want to override this method rather than paint().
     *
     * @see #paint
     */
    protected void paintBackground(Graphics g) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "656b6630-5cbc-4b49-a912-d1210ac04384");
        if (isOpaque()) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "49b5b04e-969f-48d1-8168-c3ef9b1f0d28");
            Color backup = g.getColor();
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d0b609ae-e894-40b5-a95c-ca2525bc235b");
            g.setColor(getBackground());
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f38110ff-1529-4074-a92b-2e81cff242c5");
            g.fillRect(0, 0, getWidth(), getHeight());
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "75bdc1d9-12b1-4004-ace0-994cf1c7bc55");
            g.setColor(backup);
        }
    }

    /**
     * Paint one List cell: compute the relevant state, get the "rubber stamp"
     * cell renderer component, and then use the CellRendererPane to paint it.
     * Subclasses may want to override this method rather than paint().
     *
     * @see #paint
     */
    private void paintCell(Graphics g, int index) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ea077fb0-48df-4bd8-9543-94a1671f38d1");
        Object value = getModel().getElementAt(index);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b59f6ac4-d14e-4b4c-9ea1-f5b2ae8a7815");
        boolean cellHasFocus = hasFocus() && (index == getSelectionModel().getLeadSelectionIndex());
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5b133309-c80d-458a-9302-51f7ab101b38");
        boolean isSelected = getSelectionModel().isSelectedIndex(index);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2413c190-90ad-4195-8ed0-ba09df2d8e54");
        Component renderer = getCellRenderer().getListCellRendererComponent(this, value, index, isSelected, cellHasFocus);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2da5c4df-750a-4b4d-8c47-d1fcd671d9a6");
        renderer.setSize(fixedCellWidth, fixedCellHeight);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "968ea4fb-31c6-40e8-904f-edc4d7f26418");
        renderer.paint(g);
    }

    /**
     * Paint the rows that intersect the Graphics objects clipRect.  This
     * method calls paintBackground and paintCell as necessary.  Subclasses
     * may want to override these methods.
     *
     * @see #paintBackground
     */
    protected void paintComponent(Graphics g) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4ed227df-16ee-4900-b4d1-301be7ad5821");
        updateLayoutState();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5c0dc15c-1a73-4aa4-94ca-c76151a241c0");
        if (getCellRenderer() == null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0480baec-4f62-4eef-9a6c-4d0f4d0e860c");
            return;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f7b622a7-be63-41a2-930d-43636023eeb0");
        paintBackground(g);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "13d95d97-c36e-4a19-acef-98d1d91b13fd");
        int last = getModel().getSize();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b31751b4-3e54-4a7f-8ae9-e3506d43a892");
        int dx;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "55c5e463-4799-4d6b-a68a-9adcda807a57");
        int dy;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3e8ae1e4-06c1-450b-8a6b-1c31ee90f635");
        for (int i = 0; i < last; i++) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e0fec6e7-f52e-4a92-a013-226eb2443242");
            // Point p = indexToLocation(i);
            paintCell(g, i);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f9c29079-994b-4939-a2b6-41c0a5aca06c");
            if (((i + 1) % realColumnCount) == 0) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2ebcdc67-1126-4600-a576-4ba0e4dd9f14");
                dx = -fixedCellWidth * (realColumnCount - 1);
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0ac2de61-91a2-4d38-8095-e62bf986b837");
                dy = fixedCellHeight;
            } else {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9b11fce0-20bc-4555-a201-f7f401e37480");
                dx = fixedCellWidth;
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3a573494-158f-4566-bc63-0bdb12cfe1b8");
                dy = 0;
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fd5c2602-37f1-49af-ad91-b66e8928d603");
            g.translate(dx, dy);
        }
    }

    /**
     * Recalculates the value of variables realRowCount and
     * realColumnCount. If the view needs update calls
     * revalidate.
     */
    private void updateLayoutState() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b8ef62c9-108c-402e-8a02-f9f1f58ba437");
        Dimension d = getSize();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7b877850-a852-4631-80ba-e871885d9e7e");
        int x = d.width / fixedCellWidth;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "312ab409-60cc-437b-8407-67f96339804a");
        if (x < 1) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1990e6a7-b3a8-47e0-a924-19c742e4f439");
            x = 1;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "788047cd-0604-45a2-8ce5-a6c9854e77b0");
        if (x != realColumnCount) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ec635cfb-606b-4727-8ca8-432fe3d00390");
            realColumnCount = x;
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f6eb5961-026a-4077-ac0d-1bbbf6de6893");
            updateLayoutStateNeeded = true;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "556381ea-52cc-4f85-8692-b4f013f65883");
        int y = d.height / fixedCellHeight;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2e5d8cf3-3d42-4092-b29c-2308862691d5");
        if (y != realRowCount) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "37bb5eb4-9bb5-48c5-ac64-928c94881754");
            realRowCount = y;
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "65b9913c-71d1-402e-b6bf-c37b7f10632d");
            updateLayoutStateNeeded = true;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "96b92e12-bae1-4406-9878-d565e7dfee77");
        while ((realRowCount * realColumnCount) < getModel().getSize()) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "21b05e01-ee05-40ca-986d-4b9d1dac1e3a");
            realRowCount++;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ea09e951-5703-4bac-b485-0b0436fe993f");
        locationToIndex(getVisibleRect().getLocation());
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a1690ea2-5ff0-4ddf-b130-89dbd69f3515");
        if (updateLayoutStateNeeded) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6a6f578d-78ca-4122-9460-a16d35776f3c");
            updateLayoutStateNeeded = false;
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6d0fcbd8-9632-46cb-bda4-26eaaa3d5290");
            revalidate();
        }
    }

    /**
     * The preferredSize of a list is total height of the rows
     * and the maximum width of the cells.  If JList.fixedCellHeight
     * is specified then the total height of the rows is just
     * (cellVerticalMargins + fixedCellHeight) * model.getSize() where
     * rowVerticalMargins is the space we allocate for drawing
     * the yellow focus outline.  Similarly if JListfixedCellWidth is
     * specified then we just use that plus the horizontal margins.
     *
     * @return The total size of the
     */
    public Dimension getPreferredSize() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "abe57c2b-eaf3-4d0f-a244-65b099995173");
        Insets insets = getInsets();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "430b6ea8-0601-43f7-a169-4f9c3776e937");
        /*
        int dx = insets.left + insets.right;
        int dy = insets.top + insets.bottom;
         */
        int max = getModel().getSize() - 1;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2128cc4b-62ec-4470-b31b-138a988cc0f2");
        if (max <= 0) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "11c14347-702c-4bc7-a3b4-93b64b503b5d");
            return new Dimension(fixedCellWidth, fixedCellHeight);
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "01ba660f-ce85-4319-9373-a8b37adb82fe");
        int y = (max / realColumnCount) + 1;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1332edb1-9993-40d7-89fb-6b28c2546e6c");
        int x = ((max < realColumnCount) ? (max + 1) : realColumnCount);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "301bf646-e6bc-451f-b99a-617574ded6d0");
        int xParent = getParent().getSize().width;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "aa93c25f-06a9-44ef-bdf9-0455b72a5f0c");
        int yParent = getParent().getSize().height;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e8c51397-34d8-4d54-a347-f2116375fa3d");
        int xRes = Math.max(xParent, x * fixedCellWidth);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "80b300b0-cbde-4dfa-9866-5e2b613e593b");
        int yRes = Math.max(yParent, y * fixedCellHeight);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "dfab97eb-2ca7-46aa-a871-848a7d1979ae");
        Dimension d = new Dimension(xRes, yRes);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "db372135-d33a-4071-b4c9-97a9f8579ece");
        return d;
    }

    /**
     * @return the size of one cell
     */
    public Dimension getMinimumSize() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "609fbb30-5857-46c7-9976-66409c1922f8");
        return new Dimension(fixedCellWidth, fixedCellHeight);
    }

    /**
     * Create and install the listeners for the JList, its model, and its
     * selectionModel.  This method is called at creation time.
     */
    private void addListListeners() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ce95fa45-ce00-484e-84fd-f9839b97c6a3");
        inputL = createInputListener();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c3febe88-e2cb-490b-b19b-18303c4bd7ee");
        addMouseListener(inputL);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "df7fdece-9796-4a31-8b18-c5a76158f8c8");
        addKeyListener(inputL);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "20a55ec2-e312-45f0-860f-de0a41098697");
        addFocusListener(inputL);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ff111eee-528f-436c-a28c-4a144548f7fe");
        /* When a property changes that effects layout we set
         * updateLayoutStateNeeded to the appropriate code.  We also
         * add/remove List data model listeners when the "model"
         * property changes.
         */
        propertyL = createPropertyListener();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b7ec7fd1-3fd4-4697-931f-b588ab3d8e12");
        addPropertyChangeListener(propertyL);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "87b0b92b-1f0a-4b5d-a309-768c08c08fea");
        dataL = createDataListener();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "dc67714b-82e9-4ba0-a5ca-bbe57f3bb6ba");
        ListModel model = getModel();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "58105128-6794-4d4b-a4d6-c4c74e964706");
        if (model != null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "957c1dc0-4299-491c-b2cc-572359c171ba");
            model.addListDataListener(dataL);
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "877e0e80-644b-418d-a230-d4870a68501e");
        if (selectionL == null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fa0aebb9-7bf9-4cea-98d2-cfcbdcd2e353");
            selectionL = new ListSelectionListener() {

                public void valueChanged(ListSelectionEvent e) {
                    repaint();
                }
            };
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5fe47670-410a-472e-8a78-d620deb2de30");
            ListSelectionModel selectionModel = getSelectionModel();
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ba089718-32e6-41fb-9168-cad72cf8995d");
            if (selectionModel != null) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9b1ff33c-82f6-44bb-aeb2-5cc9828c98d5");
                selectionModel.addListSelectionListener(selectionL);
            }
        }
    }

    //
    // ========== Listener inner classes ===============
    //
    private InputListener createInputListener() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "04298477-69d3-4c65-ae75-6b126db08b44");
        return new InputListener();
    }

    private ListDataListener createDataListener() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b8698925-b6b4-40a7-9f0a-1523491a6264");
        return new DataListener();
    }

    // protected ListSelectionListener createSelectionListener() {
    // return new SelectionListener();
    // }
    private PropertyChangeListener createPropertyListener() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bf2761f5-4ee6-4c65-86e7-53895b7af026");
        return new PropertyListener();
    }

    /**
     */
    private void mySetSelectionInterval(int anchor, int lead) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6f676192-9a82-412a-b2af-ff7a3fc3a110");
        super.setSelectionInterval(anchor, lead);
    }

    /**
     * Sets the selection to be the union of the specified interval with current
     */
    private void myAddSelectionInterval(int anchor, int lead) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f9c71b33-2ae1-4a6a-99d3-e69d9e2cff6a");
        super.addSelectionInterval(anchor, lead);
    }

    /**
     * Sets the selection to be the set difference of the specified interval
     */
    private void myRemoveSelectionInterval(int index0, int index1) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "eb1a63d8-e2d9-439e-bdbf-2e9720adaeb0");
        super.removeSelectionInterval(index0, index1);
    }

    public void setSelectionInterval(int anchor, int lead) {
        // super.setSelectionInterval(anchor, lead);
    }

    public void addSelectionInterval(int anchor, int lead) {
        // super.addSelectionInterval(anchor, lead);
    }

    public void removeSelectionInterval(int index0, int index1) {
        // super.removeSelectionInterval(index0, index1);
    }

    /**
     * Mouse input, and focus handling for JList.  An instance of this
     * class is added to the appropriate java.awt.Component lists
     * at creation time.  Note keyboard input is handled with JComponent
     * KeyboardActions, see registerKeyboardActions().
     * See createInputListener and registerKeyboardActions.
     */
    private class InputListener extends MouseAdapter implements FocusListener, KeyListener, Serializable {

        static final long serialVersionUID = -7907848327510962576L;

        transient int dragFirstIndex = -1;

        transient int dragLastIndex = -1;

        InputListener() {
        }

        // ==== Mouse methods =====
        public void mousePressed(MouseEvent e) {
            updateSelection(locationToIndex(e.getPoint()), e);
            if (!hasFocus()) {
                requestFocus();
            }
        }

        // ==== Focus methods =====
        public void focusGained(FocusEvent e) {
            repaintCellFocus();
        }

        public void focusLost(FocusEvent e) {
            repaintCellFocus();
        }

        protected void repaintCellFocus() {
            repaint();
        }

        // ==== Key methods =====
        public void keyTyped(KeyEvent e) {
        }

        public void keyPressed(KeyEvent e) {
            int s = getLeadSelectionIndex();
            if (s < 0) {
                if (getModel().getSize() > 0) {
                    s = 0;
                } else {
                    return;
                }
            } else {
                switch(e.getKeyCode()) {
                    case KeyEvent.VK_LEFT:
                        s -= 1;
                        break;
                    case KeyEvent.VK_RIGHT:
                        s += 1;
                        break;
                    case KeyEvent.VK_UP:
                        s -= realColumnCount;
                        break;
                    case KeyEvent.VK_DOWN:
                        s += realColumnCount;
                        break;
                    case KeyEvent.VK_HOME:
                        s = 0;
                        break;
                    case KeyEvent.VK_END:
                        s = getModel().getSize() - 1;
                        break;
                    case KeyEvent.VK_PAGE_UP:
                        s -= (realColumnCount * realRowCount);
                        break;
                    case KeyEvent.VK_PAGE_DOWN:
                        s += (realColumnCount * realRowCount);
                        break;
                    default:
                        return;
                }
            }
            if (s < 0) {
                s = 0;
            }
            if (s > (getModel().getSize() - 1)) {
                s = getModel().getSize() - 1;
            }
            if (s >= 0) {
                updateSelection(s, e);
            }
        }

        public void keyReleased(KeyEvent e) {
        }

        // ==== Update selection =====
        protected void updateSelection(int index, InputEvent e) {
            ListSelectionModel sm = getSelectionModel();
            if (index != -1) {
                setValueIsAdjusting(true);
                if (e.isShiftDown()) {
                    if (e.isControlDown()) {
                        if (dragFirstIndex == -1) {
                            myAddSelectionInterval(index, index);
                        } else {
                            if (dragLastIndex == -1) {
                                myAddSelectionInterval(dragFirstIndex, index);
                            } else {
                                myRemoveSelectionInterval(dragFirstIndex, dragLastIndex);
                                myAddSelectionInterval(dragFirstIndex, index);
                            }
                        }
                    } else {
                        if (dragFirstIndex == -1) {
                            myAddSelectionInterval(index, index);
                        } else {
                            mySetSelectionInterval(dragFirstIndex, index);
                        }
                    }
                    if (dragFirstIndex == -1) {
                        dragFirstIndex = index;
                        dragLastIndex = -1;
                    } else {
                        dragLastIndex = index;
                    }
                } else {
                    if (e.isControlDown()) {
                        if (isSelectedIndex(index)) {
                            myRemoveSelectionInterval(index, index);
                        } else {
                            myAddSelectionInterval(index, index);
                        }
                    } else {
                        mySetSelectionInterval(index, index);
                    }
                    dragFirstIndex = index;
                    dragLastIndex = -1;
                }
                setValueIsAdjusting(false);
            } else {
                sm.clearSelection();
            }
        }
    }

    /**
     * The ListDataListener that's added to the JLists model at
     * creation time, and whenever the JList.model property changes.
     * See createDataListener.
     *
     * @see JList#getModel
     */
    private class DataListener implements ListDataListener, Serializable {

        static final long serialVersionUID = -2252515707418441L;

        DataListener() {
        }

        public void intervalAdded(ListDataEvent e) {
            updateLayoutStateNeeded = true;
            int minIndex = Math.min(e.getIndex0(), e.getIndex1());
            int maxIndex = Math.max(e.getIndex0(), e.getIndex1());
            /* Sync the SelectionModel with the DataModel.
             */
            ListSelectionModel sm = getSelectionModel();
            if (sm != null) {
                sm.insertIndexInterval(minIndex, maxIndex - minIndex, true);
            }
        }

        public void intervalRemoved(ListDataEvent e) {
            updateLayoutStateNeeded = true;
            /* Sync the SelectionModel with the DataModel. */
            ListSelectionModel sm = getSelectionModel();
            if (sm != null) {
                sm.removeIndexInterval(e.getIndex0(), e.getIndex1());
            }
        }

        public void contentsChanged(ListDataEvent e) {
            updateLayoutStateNeeded = true;
        }
    }

    /**
     * The PropertyChangeListener that's added to the JList at
     * creation time.  When the value of a JList property that
     * affects layout changes, we set a bit in updateLayoutStateNeeded.
     * If the JLists model changes we additionally remove our listeners
     * from the old model.  Likewise for the JList selectionModel.
     * See createPropertyListener.
     */
    private class PropertyListener implements PropertyChangeListener, Serializable {

        static final long serialVersionUID = -6765578311995604737L;

        PropertyListener() {
        }

        public void propertyChange(PropertyChangeEvent e) {
            String propertyName = e.getPropertyName();
            if (propertyName.equals("model")) {
                // NOI18N
                ListModel oldModel = (ListModel) e.getOldValue();
                ListModel newModel = (ListModel) e.getNewValue();
                if (oldModel != null) {
                    oldModel.removeListDataListener(dataL);
                }
                if (newModel != null) {
                    newModel.addListDataListener(dataL);
                    updateLayoutStateNeeded = true;
                    repaint();
                }
            } else if (propertyName.equals("selectionModel")) {
                // NOI18N
                ListSelectionModel oldModelS = (ListSelectionModel) e.getOldValue();
                ListSelectionModel newModelS = (ListSelectionModel) e.getNewValue();
                if (oldModelS != null) {
                    oldModelS.removeListSelectionListener(selectionL);
                }
                if (newModelS != null) {
                    newModelS.addListSelectionListener(selectionL);
                }
                updateLayoutStateNeeded = true;
                repaint();
            } else if (// NOI18N
                    propertyName.equals("cellRenderer") || // NOI18N
                            propertyName.equals("font") || // NOI18N
                            propertyName.equals("fixedCellHeight") || propertyName.equals("fixedCellWidth")) {
                // NOI18N
                updateLayoutStateNeeded = true;
                repaint();
            }
        }
    }

    void writeline(String fullFilePath, String text) {
        try {
            File file = new File(fullFilePath);
            FileWriter fileWriter = new FileWriter(file, true);
            BufferedWriter output = new BufferedWriter(fileWriter);
            output.append(text);
            output.newLine();
            output.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    static void writelineStatic(String fullFilePath, String text) {
        try {
            File file = new File(fullFilePath);
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