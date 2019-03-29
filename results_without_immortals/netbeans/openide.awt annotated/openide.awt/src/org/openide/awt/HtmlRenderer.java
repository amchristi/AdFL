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

import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.font.LineMetrics;
import java.awt.geom.Area;
import java.awt.geom.Rectangle2D;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.Stack;
import java.util.StringTokenizer;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.ListCellRenderer;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.table.TableCellRenderer;
import javax.swing.tree.TreeCellRenderer;
import org.openide.util.Utilities;
import java.io.*;

/**
 * A lightweight HTML renderer supporting a minimal subset of HTML used for
 * markup purposes only: basic font styles and some colors.
 * <p>
 * Provides a generic cell renderer implementation which can be used for trees, tables,
 * lists, combo boxes, etc.
 * <p>
 * If you only need to paint some HTML quickly, use the static methods for
 * painting.  These methods behave as follows:
 * <ul>
 * <li>{@link #renderString renderString} will check the string for opening HTML tags
 * (upper or lower but not mixed case) and call either {@link #renderPlainString renderPlainString}
 * or {@link #renderHTML renderHTML} as appropriate.  Note this method does not tolerate
 * whitespace in opening HTML tags - it expects exactly 6 characters to make up
 * the opening tag if present.</li>
 * <li>{@link #renderPlainString renderPlainString} simply renders a string to the graphics context,
 * takes the same arguments as {@link #renderHTML renderHTML}, but will also honor
 * {@link #STYLE_TRUNCATE}, so strings can be rendered with trailing
 * ellipsis if there is not enough space</li>
 * <li>{@link #renderHTML renderHTML} renders whatever is passed to it as HTML, regardless
 * of whether it has opening HTML tags or not.  It can be used to render plain
 * strings, but {@link #renderPlainString renderPlainString} is faster for that. It is useful
 * if you want to render a string you <strong>know</strong> to be compliant
 * HTML markup, but which does not have opening and closing HTML tags (though
 * they are harmless if present). </li>
 * </ul>
 * <p>
 * This parser is designed entirely for performance; there are no separate parsing
 * and rendering loops.  In order to achieve its performance, some trade-offs
 * are required.
 * <strong>This is not a forgiving HTML parser - the HTML supplied
 * must follow the guidelines documented here!</strong>
 * <p>
 * The following tags are supported, in upper or lower (but not mixed) case:
 * </p>
 * <table border="1">
 * <tr>
 * <td><code>&lt;b&gt;</code></td>
 * <td>Boldface text</td>
 * </tr>
 * <tr>
 * <td><code>&lt;s&gt;</code></td>
 * <td>Strikethrough text</td>
 * </tr>
 * <tr>
 * <td><code>&lt;u&gt;</code></td>
 * <td>Underline text</td>
 * </tr>
 * <tr>
 * <td><code>&lt;i&gt;</code></td>
 * <td>Italic text</td>
 * </tr>
 * <tr>
 * <td><code>&lt;a&gt;</code></td>
 * <td>Link text</td>
 * </tr>
 * <tr>
 * <td><code>&lt;em&gt;</code></td>
 * <td>Emphasized text (same as italic)</td>
 * </tr>
 * <tr>
 * <td><code>&lt;strong&gt;</code></td>
 * <td>Strong text (same as bold)</td>
 * </tr>
 * <tr>
 * <td><code>&lt;font&gt;</code></td>
 * <td>Font color - font attributes other than color are not supported.  Colors
 * may be specified as hexadecimal strings, such as #FF0000 or as logical colors
 * defined in the current look and feel by specifying a ! character as the first
 * character of the color name.  Logical colors are colors available from the
 * current look and feel's UIManager.  For example,
 * <code>&lt;font&nbsp;color="!Tree.background"&gt;</code> will set the font color to the
 * result of {@link UIManager#getColor(Object) UIManager.getColor("Tree.background")}.
 * <strong>Font size tags are not supported.</strong>
 * </td>
 * </tr>
 * </table>
 * <p>
 * The lightweight HTML renderer supports the following named SGML character
 * entities: <code>quot</code>, <code>lt</code>, <code>amp</code>, <code>lsquo</code>,
 * <code>rsquo</code>, <code>ldquo</code>, <code>rdquo</code>, <code>ndash</code>,
 * <code>mdash</code>, <code>ne</code>, <code>le</code>, <code>ge</code>,
 * <code>copy</code>, <code>reg</code>, <code>trade</code>, and <code>nbsp</code>.
 * It also supports numeric entities
 * (e.g. <code>&amp;8822;</code>).
 * <p><b>Why not use the JDK's HTML support?</b> The JDK's HTML support works
 * well for stable components, but suffers from performance problems in the
 * case of cell renderers - each call to set the text (which happens once per
 * cell, per paint) causes a document tree to be created in memory.  For small,
 * markup-only strings, this is overkill.   For rendering short strings
 * (for example, in a tree or table cell renderer)
 * with limited HTML, this method is approximately 10x faster than standard
 * Swing HTML rendering.
 *
 * <P><B><U>Specifying logical colors</U></B><BR>
 * Hardcoded text colors are undesirable, as they can be incompatible (even
 * invisible) on some look and feels or themes, depending on the background
 * color.
 * The lightweight HTML renderer supports a non-standard syntax for specifying
 * font colors via a key for a color in the UI defaults for the current look
 * and feel.  This is accomplished by prefixing the key name with a <code>!</code>
 * character.  For example: <code>&lt;font color='!controlShadow'&gt;</code>.
 *
 * <P><B><U>Modes of operation</U></B><BR>
 * This method supports two modes of operation:
 * <OL>
 * <LI>{@link #STYLE_CLIP} - as much text as will fit in the pixel width passed
 * to the method should be painted, and the text should be cut off at the maximum
 * width or clip rectangle maximum X boundary for the graphics object, whichever is
 * smaller.</LI>
 * <LI>{@link #STYLE_TRUNCATE} - paint as much text as will fit in the pixel
 * width passed to the method, but paint the last three characters as .'s, in the
 * same manner as a JLabel truncates its text when the available space is too
 * small.</LI>
 * <!-- XXX and #STYLE_WORDWRAP? -->
 * </OL>
 * <P>
 * The paint methods can also be used in non-painting mode to establish the space
 * necessary to paint a string.  This is accomplished by passing the value of the
 * <code>paint</code> argument as false.  The return value will be the required
 * width in pixels
 * to display the text.  Note that in order to retrieve an
 * accurate value, the argument for available width should be passed
 * as {@link Integer#MAX_VALUE} or an appropriate maximum size - otherwise
 * the return value will either be the passed maximum width or the required
 * width, whichever is smaller.  Also, the clip shape for the passed graphics
 * object should be null or a value larger than the maximum possible render size,
 * or text size measurement will stop at the clip bounds.
 * <!-- XXX what does the following mean? <code>getGraphics</code>
 * will always return non-null and non-clipped, and is suitable to pass in such a
 * situation. -->
 * <P>
 *
 * <P>
 * <B>Example usages:</B><BR>
 * <a href="@org-openide-nodes@/org/openide/nodes/Node.html#getHtmlDisplayName()">org.openide.nodes.Node.getHtmlDisplayName</a><BR>
 * <a href="@org-openide-filesystems@/org/openide/filesystems/FileSystem.HtmlStatus.html">org.openide.filesystems.FileSystem.HtmlStatus</a>
 * </P>
 *
 * @since 4.30
 * @author  Tim Boudreau
 */
public final class HtmlRenderer {

    /**
     * Stack object used during HTML rendering to hold previous colors in
     * the case of nested color entries.
     */
    private static Stack<Color> colorStack = new Stack<Color>();

    /**
     * Constant used by {@link #renderString renderString}, {@link #renderPlainString renderPlainString},
     * {@link #renderHTML renderHTML}, and {@link Renderer#setRenderStyle}
     * if painting should simply be cut off at the boundary of the coordinates passed.
     */
    public static final int STYLE_CLIP = 0;

    /**
     * Constant used by {@link #renderString renderString}, {@link #renderPlainString renderPlainString},
     * {@link #renderHTML renderHTML}, and {@link Renderer#setRenderStyle} if
     * painting should produce an ellipsis (...)
     * if the text would overlap the boundary of the coordinates passed.
     */
    public static final int STYLE_TRUNCATE = 1;

    /**
     * Constant used by {@link #renderString renderString}, {@link #renderPlainString renderPlainString},
     * {@link #renderHTML renderHTML}, and {@link Renderer#setRenderStyle}
     * if painting should word wrap the text.  In
     * this case, the return value of any of the above methods will be the
     * height, rather than width painted.
     */
    private static final int STYLE_WORDWRAP = 2;

    /**
     * System property to cause exceptions to be thrown when unparsable
     * html is encountered
     */
    // NOI18N
    private static final boolean STRICT_HTML = Boolean.getBoolean("netbeans.lwhtml.strict");

    /**
     * Cache for strings which have produced errors, so we don't post an
     * error message more than once
     */
    private static Set<String> badStrings = null;

    private static Logger LOG = Logger.getLogger(HtmlRenderer.class.getName());

    /**
     * Definitions for a limited subset of SGML character entities
     */
    private static final Object[] entities = new Object[] { // NOI18N
            new char[] { 'g', 't' }, // NOI18N
            new char[] { 'l', 't' }, // NOI18N
            new char[] { 'q', 'u', 'o', 't' }, // NOI18N
            new char[] { 'a', 'm', 'p' }, // NOI18N
            new char[] { 'l', 's', 'q', 'u', 'o' }, // NOI18N
            new char[] { 'r', 's', 'q', 'u', 'o' }, // NOI18N
            new char[] { 'l', 'd', 'q', 'u', 'o' }, // NOI18N
            new char[] { 'r', 'd', 'q', 'u', 'o' }, // NOI18N
            new char[] { 'n', 'd', 'a', 's', 'h' }, // NOI18N
            new char[] { 'm', 'd', 'a', 's', 'h' }, // NOI18N
            new char[] { 'n', 'e' }, // NOI18N
            new char[] { 'l', 'e' }, // NOI18N
            new char[] { 'g', 'e' }, // NOI18N
            new char[] { 'c', 'o', 'p', 'y' }, // NOI18N
            new char[] { 'r', 'e', 'g' }, // NOI18N
            new char[] { 't', 'r', 'a', 'd', 'e' }, new char[] { // NOI18N
            'n', // NOI18N
            'b', // NOI18N
            's', // NOI18N
            'p' } };

    // NOI18N
    /**
     * Mappings for the array of SGML character entities to characters
     */
    private static final char[] entitySubstitutions = new char[] { // NOI18N
            '>', // NOI18N
            '<', // NOI18N
            '"', // NOI18N
            '&', // NOI18N
            8216, // NOI18N
            8217, // NOI18N
            8220, // NOI18N
            8221, // NOI18N
            8211, // NOI18N
            8212, // NOI18N
            8800, // NOI18N
            8804, // NOI18N
            8805, 169, 174, 8482, ' ' };

    private HtmlRenderer() {
        // do nothing
    }

    /**
     * Returns an instance of Renderer which may be used as a table/tree/list cell renderer.
     * This method must be called on the AWT event thread.  If you <strong>know</strong> you will
     * be passing it legal HTML (legal as documented here), call {@link Renderer#setHtml setHtml(true)} on the
     * result of this call <strong>after calling getNNNCellRenderer</strong> to provide this hint.
     *
     * @return A cell renderer that can render HTML.
     */
    public static final Renderer createRenderer() {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "df836eed-a57e-498d-bde1-1018659258b1");
        return new HtmlRendererImpl();
    }

    /**
     * For HTML rendering jobs outside of trees/lists/tables, returns a JLabel which will paint its text using
     * the lightweight HTML renderer.  The result of this call will implement {@link Renderer}.
     * <strong>Do not add the result of this call to the AWT hierarchy</strong>.  It is not a general purpose <code>JLabel</code>, and
     * will not behave correctly.  Use the result of this call to paint or to measure text.  Example:
     * <pre>
     * private final JLabel label = HtmlRenderer.createLabel();
     *
     * public void paint(Graphics g) {
     * // background/whatever painting code here...
     * label.setText(someHtmlText);
     * label.paint(g);
     * }
     * </pre>
     *
     * @return a label which can render a subset of HTML very quickly
     */
    public static final JLabel createLabel() {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bb2d7aba-5fe2-477b-b208-61750cb3f167");
        return new HtmlRendererImpl();
    }

    /**
     * Render a string to a graphics instance, using the same API as {@link #renderHTML renderHTML}.
     * Can render a string using JLabel-style ellipsis (...) in the case that
     * it will not fit in the passed rectangle, if the style parameter is
     * {@link #STYLE_CLIP}. Returns the width in pixels successfully painted.
     * <strong>This method is not thread-safe and should not be called off
     * the AWT thread.</strong>
     *
     * @see #renderHTML
     */
    public static double renderPlainString(String s, Graphics g, int x, int y, int w, int h, Font f, Color defaultColor, int style, boolean paint) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0cd2abb2-d75d-4ca4-8290-c371c0eac187");
        // per Jarda's request, keep the word wrapping code but don't expose it.
        if ((style < 0) || (style > 1)) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f09e7ac0-1f47-486b-ba75-1c573c923220");
            // NOI18N
            throw new IllegalArgumentException("Unknown rendering mode: " + style);
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f6dfe491-3939-410b-8368-80565e7ff4cb");
        return _renderPlainString(s, g, x, y, w, h, f, defaultColor, style, paint);
    }

    private static double _renderPlainString(String s, Graphics g, int x, int y, int w, int h, Font f, Color foreground, int style, boolean paint) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "059805e8-1e84-48bc-841d-f9addefe0bdc");
        if (f == null) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5ee6b8df-9a39-40e6-b374-cf8ec0842f7b");
            // NOI18N
            f = UIManager.getFont("controlFont");
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b7ecf4d3-8d7a-49f1-b80e-b82f194fecf2");
            if (f == null) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3deb368a-96bf-4c6a-be39-ac94c6cee70a");
                int fs = 11;
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e16853d1-d2eb-46c4-bbd7-17bf6d9a44c6");
                // NOI18N
                Object cfs = UIManager.get("customFontSize");
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "53595dc9-1cdc-4f90-8be8-e0f500bbc759");
                if (cfs instanceof Integer) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2e70fc50-b0e4-4368-807e-9323be7dbb05");
                    fs = ((Integer) cfs).intValue();
                }
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d5dcf310-e201-4d4d-95a1-3baf301e5449");
                // NOI18N
                f = new Font("Dialog", Font.PLAIN, fs);
            }
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "508ee09b-eb0c-4fb8-ab61-600d77ecdaa3");
        FontMetrics fm = g.getFontMetrics(f);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c9feb8fb-2adb-4a34-9e1e-5cb77304ab34");
        // Rectangle2D r = fm.getStringBounds(s, g);
        int wid;
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b7a0a707-bd73-4f7e-855d-b3833c74c81d");
        if (Utilities.isMac()) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "da464a69-b390-4e1d-ae44-3b052fb15e52");
            // #54257 - on macosx + chinese/japanese fonts, the getStringBounds() method returns bad value
            wid = fm.stringWidth(s);
        } else {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e43fafe1-ac1d-40af-addf-d920a1dfd568");
            wid = (int) fm.getStringBounds(s, g).getWidth();
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bb36d0d5-ed18-444b-8807-9baa85614417");
        if (paint) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "897fa086-dceb-4b99-8622-a35e8243f60e");
            g.setColor(foreground);
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "739a5132-5834-41ee-8c85-98f191d1ec5b");
            g.setFont(f);
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ef6f527c-d162-4791-8e06-8f2149e505fd");
            if ((wid <= w) || (style == STYLE_CLIP)) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3bdaf02d-9b78-41e2-99b6-9cb05aa19f85");
                g.drawString(s, x, y);
            } else {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "74859c07-49fe-4bc6-a351-8956b37db56a");
                char[] chars = s.toCharArray();
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b105a98b-39f5-4c1c-92a4-db7caafb8033");
                if (chars.length == 0) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4bc823d7-ed03-4986-99f6-b4203b836b81");
                    return 0;
                }
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2e42b315-3ebf-410e-a32e-3bb083a59eff");
                double chWidth = wid / chars.length;
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6935ff07-ccd0-4c89-b72f-4ce26e247a21");
                int estCharsToPaint = new Double(w / chWidth).intValue();
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2d0812d1-3d19-40a4-86b7-f9b8a2bef1e6");
                if (estCharsToPaint > chars.length) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "37cf175a-e28f-4e9e-9d25-f24d1ae73eb4");
                    estCharsToPaint = chars.length;
                }
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a1f85361-3f35-4469-9b2e-bdc35a3f00da");
                // let's correct the estimate now
                while (estCharsToPaint > 3) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3a44a77f-202f-42e9-b8a2-64d766be6141");
                    if (estCharsToPaint < chars.length) {
                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "69d381e9-38f8-4654-a26a-40cb78d1074d");
                        chars[estCharsToPaint - 1] = '…';
                    }
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "04211512-123d-4b20-8b41-7ad37b39abff");
                    int newWidth;
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3cd169e2-5695-48c5-8c7d-fdd0b7b4a928");
                    if (Utilities.isMac()) {
                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8b02dbc1-1bb7-414e-abed-47f6f2872d19");
                        // #54257 - on macosx + chinese/japanese fonts, the getStringBounds() method returns bad value
                        newWidth = fm.stringWidth(new String(chars, 0, estCharsToPaint));
                    } else {
                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "40957363-e0cd-458d-8023-3a1dc6128c3b");
                        newWidth = (int) fm.getStringBounds(chars, 0, estCharsToPaint, g).getWidth();
                    }
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8f288582-2204-40be-a75d-87e19c8ca75f");
                    if (newWidth <= w)
                        break;
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cbb44394-c7c5-49de-81db-5aac47fbc425");
                    estCharsToPaint--;
                }
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f8df0624-1435-4184-9f50-ecea7cc15b15");
                if (style == STYLE_TRUNCATE) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3a8c509c-c7b4-4d29-8612-867259fdc86d");
                    int length = estCharsToPaint;
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8bc02abe-9f44-4f68-a4cf-a2409e40529f");
                    if (length <= 0) {
                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b058da98-05ad-4e70-a0e3-e1eac831928a");
                        return 0;
                    }
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "194fadc9-90f3-4557-a090-66a48d7cd1b8");
                    if (paint) {
                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9cf8c07a-cfc5-44c8-8145-50ae7366b4da");
                        if (length > 3) {
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2a3eeb52-12e6-4cc3-a71e-e4858b132862");
                            g.drawChars(chars, 0, length, x, y);
                        } else {
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1e0ea6d6-5dc9-4899-93b9-887a0dd804b1");
                            Shape shape = g.getClip();
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7fd27026-55f1-4a06-9682-20f4891a187f");
                            // clip only if clipping is supported
                            if (shape != null) {
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bb6b3ca7-4548-465f-aaf6-ebe8550d19e1");
                                if (s != null) {
                                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "50425b7c-47c4-404b-894b-1a8e3f47c3ba");
                                    Area area = new Area(shape);
                                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ee726214-5df1-47c1-b7fc-e65458ab6fde");
                                    area.intersect(new Area(new Rectangle(x, y, w, h)));
                                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ffa81feb-bf99-462b-a47d-6cc76b3ec47f");
                                    g.setClip(area);
                                } else {
                                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e7f2d036-80c1-4b2f-9c21-5586a0086dff");
                                    g.setClip(new Rectangle(x, y, w, h));
                                }
                            }
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "71dc13e1-c69f-4f9d-9244-f2c573e9e3d5");
                            g.drawString("…", x, y);
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e694ba18-dd50-42ab-ab8f-f55c77973fc9");
                            if (shape != null) {
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bccb0250-9726-4daf-bfe6-e913865a4d57");
                                g.setClip(shape);
                            }
                        }
                    }
                } else {
                    // TODO implement plaintext word wrap if we want to support it at some point
                }
            }
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "82d9df1a-b853-47b3-bd3e-3fe078317c72");
        return wid;
    }

    /**
     * Render a string to a graphics context, using HTML markup if the string
     * begins with <code>&lt;html&gt;</code>.  Delegates to {@link #renderPlainString renderPlainString}
     * or {@link #renderHTML renderHTML} as appropriate.  See the class documentation for
     * for details of the subset of HTML that is
     * supported.
     * @param s The string to render
     * @param g A graphics object into which the string should be drawn, or which should be
     * used for calculating the appropriate size
     * @param x The x coordinate to paint at.
     * @param y The y position at which to paint.  Note that this method does not calculate font
     * height/descent - this value should be the baseline for the line of text, not
     * the upper corner of the rectangle to paint in.
     * @param w The maximum width within which to paint.
     * @param h The maximum height within which to paint.
     * @param f The base font to be used for painting or calculating string width/height.
     * @param defaultColor The base color to use if no font color is specified as html tags
     * @param style The wrapping style to use, either {@link #STYLE_CLIP},
     * or {@link #STYLE_TRUNCATE}
     * @param paint True if actual painting should occur.  If false, this method will not actually
     * paint anything, only return a value representing the width/height needed to
     * paint the passed string.
     * @return The width in pixels required
     * to paint the complete string, or the passed parameter <code>w</code> if it is
     * smaller than the required width.
     */
    public static double renderString(String s, Graphics g, int x, int y, int w, int h, Font f, Color defaultColor, int style, boolean paint) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0394cb7d-feda-4048-8e8b-16799f8c6d28");
        switch(style) {
            case STYLE_CLIP:
            case STYLE_TRUNCATE:
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "87969239-7001-4178-9825-8131404f46d8");
                break;
            default:
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bcdbedc8-12fb-4466-9862-6dadfb583619");
                // NOI18N
                throw new IllegalArgumentException("Unknown rendering mode: " + style);
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d4da361f-97b3-4c69-8c44-51ce3041e913");
        // System.err.println ("rps: " + y + " " + s);
        if (s.startsWith("<html") || s.startsWith("<HTML")) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5c930adb-09c5-42e3-93aa-66b3bcab1131");
            return _renderHTML(s, 6, g, x, y, w, h, f, defaultColor, style, paint, null);
        } else {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8ea74b3d-cef7-4e1e-b41f-da5cb35319fe");
            return renderPlainString(s, g, x, y, w, h, f, defaultColor, style, paint);
        }
    }

    /**
     * Render a string as HTML using a fast, lightweight renderer supporting a limited
     * subset of HTML.  See class Javadoc for details.
     *
     * <P>
     * This method can also be used in non-painting mode to establish the space
     * necessary to paint a string.  This is accomplished by passing the value of the
     * <code>paint</code> argument as false.  The return value will be the required
     * width in pixels
     * to display the text.  Note that in order to retrieve an
     * accurate value, the argument for available width should be passed
     * as {@link Integer#MAX_VALUE} or an appropriate maximum size - otherwise
     * the return value will either be the passed maximum width or the required
     * width, whichever is smaller.  Also, the clip shape for the passed graphics
     * object should be null or a value larger than the maximum possible render size.
     * <P>
     * This method will log a warning if it encounters HTML markup it cannot
     * render.  To aid diagnostics, if NetBeans is run with the argument
     * <code>-J-Dnetbeans.lwhtml.strict=true</code> an exception will be thrown
     * when an attempt is made to render unsupported HTML.
     * @param s The string to render
     * @param g A graphics object into which the string should be drawn, or which should be
     * used for calculating the appropriate size
     * @param x The x coordinate to paint at.
     * @param y The y position at which to paint.  Note that this method does not calculate font
     * height/descent - this value should be the baseline for the line of text, not
     * the upper corner of the rectangle to paint in.
     * @param w The maximum width within which to paint.
     * @param h The maximum height within which to paint.
     * @param f The base font to be used for painting or calculating string width/height.
     * @param defaultColor The base color to use if no font color is specified as html tags
     * @param style The wrapping style to use, either {@link #STYLE_CLIP},
     * or {@link #STYLE_TRUNCATE}
     * @param paint True if actual painting should occur.  If false, this method will not actually
     * paint anything, only return a value representing the width/height needed to
     * paint the passed string.
     * @return The width in pixels required
     * to paint the complete string, or the passed parameter <code>w</code> if it is
     * smaller than the required width.
     */
    public static double renderHTML(String s, Graphics g, int x, int y, int w, int h, Font f, Color defaultColor, int style, boolean paint) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2166c731-3540-4243-b855-35c97735c993");
        // per Jarda's request, keep the word wrapping code but don't expose it.
        if ((style < 0) || (style > 1)) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e1b1831b-e908-4ad7-b777-f83dc332bf50");
            // NOI18N
            throw new IllegalArgumentException("Unknown rendering mode: " + style);
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f8329aa7-6a42-4084-8c5b-9db30cbb7acb");
        return _renderHTML(s, 0, g, x, y, w, h, f, defaultColor, style, paint, null);
    }

    /**
     * Implementation of HTML rendering
     */
    static double _renderHTML(String s, int pos, Graphics g, int x, int y, int w, int h, Font f, Color defaultColor, int style, boolean paint, Color background) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1c88e9e3-2506-47df-a42d-04d469a550b0");
        return _renderHTML(s, pos, g, x, y, w, h, f, defaultColor, style, paint, background, false);
    }

    /**
     * Implementation of HTML rendering
     */
    static double _renderHTML(String s, int pos, Graphics g, int x, int y, int w, int h, Font f, Color defaultColor, int style, boolean paint, Color background, boolean forcedForeground) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f4c993f9-caca-4169-96a2-7e5f9abb68e7");
        // System.err.println ("rhs: " + y + " " + s);
        if (f == null) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "652fff84-8fdb-4e74-b959-b0e333943d72");
            // NOI18N
            f = UIManager.getFont("controlFont");
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8feffcdf-d8a4-45b5-ae8f-7906f4016ee7");
            if (f == null) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b2bb1bf1-a023-49e3-9336-18805218c1be");
                int fs = 11;
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6a5c1bea-ffbf-460b-b483-17a9d8be3318");
                // NOI18N
                Object cfs = UIManager.get("customFontSize");
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c780f921-fcd6-426e-a037-8f6add407fff");
                if (cfs instanceof Integer) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "10998bde-1227-421c-8a61-82cfecbb8251");
                    fs = ((Integer) cfs).intValue();
                }
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f715e84d-8ed7-4a7c-8f40-b26adfc1493b");
                // NOI18N
                f = new Font("Dialog", Font.PLAIN, fs);
            }
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "63d5cc7f-c7a3-42a3-9ba8-d7ca267942b4");
        // Thread safety - avoid allocating memory for the common case
        Stack<Color> _colorStack = SwingUtilities.isEventDispatchThread() ? HtmlRenderer.colorStack : new Stack<Color>();
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0cb7d05c-86c1-4e2c-aa05-b7d058d75508");
        g.setColor(defaultColor);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "36351b2f-823e-490c-88c1-883daf6e53cb");
        g.setFont(f);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c0f5f76e-53c5-4e53-90d0-b565a6f2b715");
        if (HtmlLabelUI.antialias && g instanceof Graphics2D) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2758f4a1-0dad-459a-af7c-7d1427304442");
            ((Graphics2D) g).setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "51158abf-2379-4700-9558-38a80636bc15");
        char[] chars = s.toCharArray();
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "634d15e1-25b9-4a81-9067-d7d7d6fffef3");
        int origX = x;
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "470d7a75-6c5e-4107-b652-e4b2599755f4");
        // flag if rendering completed, either by finishing the string or running out of space
        boolean done = false;
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "54d55a67-a360-4b09-85ad-497393f432dd");
        // flag if the current position is inside a tag, and the tag should be processed rather than rendering
        boolean inTag = false;
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5490efbe-8e78-476c-ace6-c6113aacf21e");
        // flag if the current position is inside a closing tag
        boolean inClosingTag = false;
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "da5837d6-2949-46e1-b40f-9153ad426af9");
        // flag if a strikethrough line should be painted
        boolean strikethrough = false;
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d2b360c3-523a-495d-bad2-b8eff25019ea");
        // flag if an underline should be painted
        boolean underline = false;
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "aea5d23c-24ad-4fa1-b2b6-99eb2af8e218");
        // flag if a link should be painted
        boolean link = false;
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "602dcbbd-25a3-4a51-b25b-4c4937e2be22");
        // flag if text is currently bold
        boolean bold = false;
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "76376a18-9209-4ec6-9107-9fc376908d7b");
        // flag if text is currently italic
        boolean italic = false;
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "44a54a01-ed5a-4a63-adf4-498f0f5059df");
        // flag if the last possible character has been painted, and the next loop should paint "..." and return
        boolean truncated = false;
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8ca1bc68-aae5-4978-90e0-457ea3cd403b");
        // the total width painted, for calculating needed space
        double widthPainted = 0;
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2b8b3c23-a5d6-42a9-af40-d6af791f5fa0");
        // the total height painted, for calculating needed space
        double heightPainted = 0;
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cfc99c6c-0ea9-48cc-8377-c576b61e34e8");
        // flag to skip additional whitespace if one whitespace char already painted
        boolean lastWasWhitespace = false;
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "720599fe-635e-443c-8955-b3c90b2d0d9f");
        // the last line height, for calculating total required height
        double lastHeight = 0;
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0ca3f9ae-6be3-4f8f-85c6-8fb8ee3623e6");
        double dotWidth = 0;
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f2d7169c-6811-46cc-8136-775caa105d5d");
        boolean dotsPainted = false;
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "18db9b49-0ad7-4355-8e55-6fc3756335f6");
        // Calculate the width of a . character if we may need to truncate
        if (style == STYLE_TRUNCATE) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1ee42f62-497d-4623-b4a8-9d67388f4af2");
            // NOI18N
            dotWidth = g.getFontMetrics().charWidth('.');
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d41a64c7-741d-4ed4-a2fe-05e7788cbcc7");
        /* How this all works, for anyone maintaining this code (hopefully it will
          never need it):
          1. The string is converted to a char array
          2. Loop over the characters.  Variable pos is the current point.
            2a. See if we're in a tag by or'ing inTag with currChar == '<'
              If WE ARE IN A TAG:
               2a1: is it an opening tag?
                 If YES:
                   - Identify the tag, Configure the Graphics object with
                     the appropriate font, color, etc.  Set pos = the first
                     character after the tag
                 If NO (it's a closing tag)
                   - Identify the tag.  Reconfigure the Graphics object
                     with the state it should be in outside the tag
                     (reset the font if italic, pop a color off the stack, etc.)
            2b. If WE ARE NOT IN A TAG
               - Locate the next < or & character or the end of the string
               - Paint the characters using the Graphics object
               - Check underline and strikethrough tags, and paint line if
                 needed
            See if we're out of space, and do the right thing for the style
            (paint ..., give up or skip to the next line)
         */
        // Clear any junk left behind from a previous rendering loop
        _colorStack.clear();
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "563ceb56-4156-4f16-aa60-1f38757548e0");
        // Enter the painting loop
        while (!done) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "738ff245-a319-41db-8b88-231b4a771965");
            if (pos == s.length()) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "11ec7366-2377-4ae9-bab5-7b4de51e4b21");
                if (truncated && paint && !dotsPainted) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1564eab9-1862-4127-92d0-5f900f2c948f");
                    g.setColor(defaultColor);
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "931ee53e-3db9-4845-8a20-7f5a77ebfc65");
                    g.setFont(f);
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "16e4d596-06a8-4439-9200-ded5d98e23b8");
                    // NOI18N
                    g.drawString("…", x, y);
                }
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e7d46752-865b-4a4a-9d05-b1f40fef392d");
                return widthPainted;
            }
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c3d2fe4b-8343-4c53-8749-ba6227de8991");
            // see if we're in a tag
            try {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "509959f5-6169-41f3-a03d-3d34580092a1");
                inTag |= (chars[pos] == '<');
            } catch (ArrayIndexOutOfBoundsException e) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9f78b0c3-6939-4b11-a592-d4121e402841");
                // Should there be any problem, give a meaningful enough
                // message to reproduce the problem
                ArrayIndexOutOfBoundsException aib = new ArrayIndexOutOfBoundsException(// NOI18N
                        "HTML rendering failed at position " + pos + " in String \"" + s + "\".  Please report this at http://www.netbeans.org");
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "18a5eb8c-2017-434b-9aef-7efd033929be");
                if (STRICT_HTML) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d6c13a17-dd0f-4184-be6c-f7ac1377d0d3");
                    throw aib;
                } else {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "80857257-d225-414a-878c-6f6141ca915d");
                    Logger.getLogger(HtmlRenderer.class.getName()).log(Level.WARNING, null, aib);
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6ac2584a-dd1f-47eb-a776-a0717b77adad");
                    return renderPlainString(s, g, x, y, w, h, f, defaultColor, style, paint);
                }
            }
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "247629bc-bb9a-42ce-afe6-0686ebeade65");
            // NOI18N
            inClosingTag = inTag && ((pos + 1) < chars.length) && (chars[pos + 1] == '/');
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e9f672bc-3b3a-4506-83a6-ab82e7c4993a");
            if (truncated) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "65e6a0f1-e47d-460f-8202-d2a1b28d11e4");
                // Then we've almost run out of space, time to print ... and quit
                g.setColor(defaultColor);
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e023630e-1e55-4f7d-9ca0-1bb907b1d881");
                g.setFont(f);
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "135b9f1a-ce29-4d6a-a80d-1a6b0be73342");
                if (paint) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e2d5d7a8-008f-4c91-852a-ed02b1f960cf");
                    // NOI18N
                    g.drawString("…", x, y);
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "83f8270d-e8c4-46d9-881f-5f7d13b336a4");
                    // make sure we paint the dots only once
                    dotsPainted = true;
                }
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3987bb63-4734-4408-9730-e98329931da2");
                done = true;
            } else if (inTag) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bad9c6ad-066e-4862-a304-bcea3b94be46");
                // If we're in a tag, don't paint, process it
                pos++;
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "46e92423-e453-4a7f-811b-35c9273a2332");
                int tagEnd = pos;
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9ff364da-08a3-4e5e-8524-72a3c91c3bf6");
                // #54237 - if done and end of string -> wrong html
                done = tagEnd >= (chars.length - 1);
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d42429c2-40ab-4264-a606-3ac5df191ccb");
                while (!done && (chars[tagEnd] != '>')) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ab83c47c-5d87-4176-a6ff-dfaaa0d3651f");
                    done = tagEnd == (chars.length - 1);
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bf116171-23bc-44d6-a271-ee77187e324e");
                    tagEnd++;
                }
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5e1e2484-a981-4b90-82b5-6b8d9613f117");
                if (done) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bc43df63-9f0d-4fd2-8789-23c6b327c28c");
                    throwBadHTML("Matching '>' not found", pos, chars);
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1e4fc8bf-bf88-46b2-bbe4-2a88bf76b573");
                    break;
                }
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4bbbdba0-aa94-4a34-bb24-bf4a98e0d2f5");
                if (inClosingTag) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "53429a03-01fa-48e3-8a6f-a94c3bb864e7");
                    // Handle closing tags by resetting the Graphics object (font, etc.)
                    pos++;
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7c1bc2be-0b29-4df2-b548-42140d9a49e2");
                    switch(chars[pos]) {
                        // NOI18N
                        case 'a':
                        case // NOI18N
                                'A':
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5575bd7d-d297-4c8f-928b-2027326f2e68");
                            if (_colorStack.isEmpty() || forcedForeground) {
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "06a71b8b-c0c0-41c8-94b0-ace3278c3406");
                                g.setColor(defaultColor);
                            } else {
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f82d0fd4-26fb-41cf-a274-4cda5f3468e2");
                                g.setColor(_colorStack.pop());
                            }
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fa1f0285-5020-4ab5-a101-ec7703790861");
                            link = false;
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d2fe9b3b-4771-45d0-befb-460ba8b76b34");
                            break;
                        // NOI18N
                        case 'P':
                            // NOI18N
                        case 'p':
                            // NOI18N
                        case 'H':
                        case 'h':
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0ead630e-fce8-4e97-a9dc-a9799197c59e");
                            // ignore html opening/closing tags
                            break;
                        // NOI18N
                        case 'B':
                        case // NOI18N
                                'b':
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "db0c48c1-31ab-4736-9d1d-2426cab9a39d");
                            if ((chars[pos + 1] == 'r') || (chars[pos + 1] == 'R')) {
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3dc2b9fc-f1bc-4568-b8e3-daee074d5ab2");
                                break;
                            }
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8527a394-1f6b-4de1-9d24-48df3b77a56c");
                            if (!bold && !(chars[pos + 1] == 'o' || chars[pos + 1] == 'O')) {
                                // NOI18N
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b5373ef3-cf58-41e7-948f-959ec0e3b829");
                                throwBadHTML(// NOI18N
                                        "Closing bold tag w/o " + "opening bold tag", pos, chars);
                            }
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4c9ba86d-321d-448e-b12a-3e491d933033");
                            if (italic) {
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "547a4d91-bf63-467d-a345-ae401438d499");
                                g.setFont(deriveFont(f, Font.ITALIC));
                            } else {
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2f35c9d4-a50e-43c0-a3d3-b24dfd676e98");
                                g.setFont(deriveFont(f, Font.PLAIN));
                            }
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "01f34852-d7ab-41d5-9aa7-dfa2bb8b3195");
                            bold = false;
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9741e5c8-fab5-4a30-8ba0-13aa82e5cb35");
                            break;
                        // NOI18N
                        case 'E':
                            // em tag
                        case 'e':
                            // NOI18N
                        case 'I':
                        case // NOI18N
                                'i':
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0c6cbc3a-49c1-4030-88ff-9f043d7f43da");
                            if (bold) {
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "896c84fb-f403-4186-8ce2-b770ded8a395");
                                g.setFont(deriveFont(f, Font.BOLD));
                            } else {
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1b290015-8ec5-45eb-860a-f5fbd7762ef4");
                                g.setFont(deriveFont(f, Font.PLAIN));
                            }
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "62153277-3ea1-4ed7-8161-396255b64c86");
                            if (!italic) {
                                // NOI18N
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "361d757e-2672-48a8-924b-6c18c204cb42");
                                throwBadHTML(// NOI18N
                                        "Closing italics tag w/o" + "opening italics tag", pos, chars);
                            }
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a30c2a75-a843-458b-924e-e68a1fab5389");
                            italic = false;
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "02ab8ac5-c897-4e8d-b394-1964a4c2b5b7");
                            break;
                        // NOI18N
                        case 'S':
                        case // NOI18N
                                's':
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f6f6fac0-c28a-4ec8-9a98-fa44ee89faf3");
                            switch(chars[pos + 1]) {
                                // NOI18N
                                case 'T':
                                case 't':
                                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "476a91ae-2c2c-40ee-8025-4c0defa9effa");
                                    if (italic) {
                                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2c948682-1354-46ac-990c-6c7d0d38f235");
                                        // NOI18N
                                        g.setFont(deriveFont(f, Font.ITALIC));
                                    } else {
                                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "06875b40-9cb4-414f-b8d1-9c8d30ad1d63");
                                        g.setFont(deriveFont(f, Font.PLAIN));
                                    }
                                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ca58431a-1783-472e-94ec-c911f1f845df");
                                    bold = false;
                                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ae262745-fdd8-4a61-98f0-7ecf695a4092");
                                    break;
                                case // NOI18N
                                        '>':
                                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8e5c5302-9ac9-4411-9a00-b0d4baecee0d");
                                    strikethrough = false;
                                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7aef7d28-ac5b-470a-86a1-c137b401828c");
                                    break;
                            }
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b297a2e9-861a-4a9b-a48f-4ca0e769da9b");
                            break;
                        // NOI18N
                        case 'U':
                        case 'u':
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "50e1291b-96ec-4f56-8740-1ff079229e90");
                            // NOI18N
                            underline = false;
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5dfc7be5-a174-4f57-9729-527066e195e1");
                            break;
                        // NOI18N
                        case 'F':
                        case // NOI18N
                                'f':
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3fbf2c5e-58bc-4f22-9a5f-dccbe694d410");
                            if (_colorStack.isEmpty() || forcedForeground) {
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3063f625-f860-42d8-862d-5162a82d692b");
                                g.setColor(defaultColor);
                            } else {
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "035b5af9-91df-45b7-a90b-2b34cdad1c4e");
                                g.setColor(_colorStack.pop());
                            }
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "51b977e5-e3ff-4533-81b4-9384cfb4b733");
                            break;
                        default:
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5ad776c5-8d02-4ba6-9b48-99f50fe3f66a");
                            throwBadHTML(// NOI18N
                                    "Malformed or unsupported HTML", pos, chars);
                    }
                } else {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e11a6d06-ba85-4377-a7d6-3679a5fe9c8c");
                    // Okay, we're in an opening tag.  See which one and configure the Graphics object
                    switch(chars[pos]) {
                        // NOI18N
                        case 'a':
                        case // NOI18N
                                'A':
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f66b44e2-6c8b-4406-bc79-2232d6492e2c");
                            if (!forcedForeground) {
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fde5dcd2-a307-4e23-9672-e8aa3760b8ac");
                                // NOI18N
                                Color linkc = UIManager.getColor("nb.html.link.foreground");
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "33da1c19-e5ec-47c1-a88a-15efa728eef7");
                                if (linkc == null) {
                                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bcde8b65-a0c9-4011-9c06-41fbb9c25961");
                                    linkc = Color.BLUE;
                                }
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7ebcf2cf-416f-4803-b2e2-ca1f59ce27aa");
                                _colorStack.push(g.getColor());
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7fff564c-d754-4c23-86ad-57098c4d814b");
                                linkc = HtmlLabelUI.ensureContrastingColor(linkc, background);
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0b172d3b-359f-47fb-b6f1-555e0f8dc77c");
                                g.setColor(linkc);
                            }
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e829019c-b234-48bf-8ce8-cb2129d33eac");
                            link = true;
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b0db9152-f962-4a15-9393-2a3fc8283439");
                            break;
                        // NOI18N
                        case 'B':
                        case // NOI18N
                                'b':
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2b1a81e8-bcf5-457a-a20b-293ba7dcfb7d");
                            switch(chars[pos + 1]) {
                                // NOI18N
                                case 'R':
                                case // NOI18N
                                        'r':
                                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bb0f97c7-7ba2-436a-8de4-6d4d9996b773");
                                    if (style == STYLE_WORDWRAP) {
                                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d13ac0bd-7821-4a3f-9804-8d7a080cdf72");
                                        x = origX;
                                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0c41f0d6-bcf8-4f59-a437-c93e8ebd3162");
                                        int lineHeight = g.getFontMetrics().getHeight();
                                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5734bd69-548e-47d8-8a7c-9afa775ca5d6");
                                        y += lineHeight;
                                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b2c0d562-66a8-470d-8d2f-0a0056493a49");
                                        heightPainted += lineHeight;
                                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a6967341-c8e0-4a34-9ea9-b914ceb63d40");
                                        widthPainted = 0;
                                    }
                                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1866a31d-0f78-4bec-bdd8-b2bb2f8cb25f");
                                    break;
                                case '>':
                                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b9b211b8-3543-423b-81e6-5f0a9914c4b5");
                                    bold = true;
                                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7da91fd3-518a-4a8e-8d95-9f583a9f27a5");
                                    if (italic) {
                                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "326a7363-80ea-4626-be8a-8286987f2749");
                                        g.setFont(deriveFont(f, Font.BOLD | Font.ITALIC));
                                    } else {
                                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5b379a00-3e6b-4c4b-a4b8-a2a4739b360b");
                                        g.setFont(deriveFont(f, Font.BOLD));
                                    }
                                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a5733f99-9aab-4d31-8ee4-93508b005264");
                                    break;
                            }
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "adbd68e2-3241-44f0-b285-46ab947e99c0");
                            break;
                        // NOI18N  //em tag
                        case 'e':
                            // NOI18N
                        case 'E':
                            // NOI18N
                        case 'I':
                        case // NOI18N
                                'i':
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5dfe6eda-e150-45d8-b33c-0da692a42ed1");
                            italic = true;
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5072818e-8394-4014-9e50-d558d755687a");
                            if (bold) {
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e52151ff-dfbb-4e59-97e2-bbf618ec3b1d");
                                g.setFont(deriveFont(f, Font.ITALIC | Font.BOLD));
                            } else {
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e8e76760-0a32-444b-8b74-ccc4d41b6aba");
                                g.setFont(deriveFont(f, Font.ITALIC));
                            }
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "03d91f47-fa54-487c-bee3-416d0ddafda3");
                            break;
                        // NOI18N
                        case 'S':
                        case // NOI18N
                                's':
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ad524444-7d3b-4345-a53c-f2002b5b13b4");
                            switch(chars[pos + 1]) {
                                case '>':
                                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fdbd2ff7-cd1e-4a90-bd34-8addf8599d85");
                                    strikethrough = true;
                                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5fa084d2-802f-40cd-b512-d79a18b62f0f");
                                    break;
                                case 'T':
                                case 't':
                                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "45230d85-62ba-4844-90e0-1af4e232f920");
                                    bold = true;
                                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6fecb1a7-fcbe-47b6-b634-a7cbb12f8cbf");
                                    if (italic) {
                                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d3376fa1-8c49-4f97-b2cf-e0b42d8d3cf8");
                                        g.setFont(deriveFont(f, Font.BOLD | Font.ITALIC));
                                    } else {
                                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6e9b102e-c883-483e-ae7f-b3967a32c30d");
                                        g.setFont(deriveFont(f, Font.BOLD));
                                    }
                                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f73a1207-bc40-49ed-9892-14de57b0da3a");
                                    break;
                            }
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "042194cb-f3c3-46b2-8351-8601e8ae3955");
                            break;
                        // NOI18N
                        case 'U':
                        case // NOI18N
                                'u':
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6e2e5c32-a050-4221-a19b-8bab493f0228");
                            underline = true;
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8cb34ecc-78d1-436b-a86d-044263dd5cc4");
                            break;
                        // NOI18N
                        case 'f':
                        case // NOI18N
                                'F':
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3d70b38d-5a4a-417f-872b-b94d3b7fa8f1");
                            if (!forcedForeground) {
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c7fe2672-40a3-4c76-9567-ce4ce7f51981");
                                Color c = findColor(chars, pos, tagEnd);
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "04fe81c3-34b2-4e5f-bbec-dc4b0452feb5");
                                _colorStack.push(g.getColor());
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "498d1d76-2500-4dd3-abf5-a0cb1b2db267");
                                c = HtmlLabelUI.ensureContrastingColor(c, background);
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0c291ab2-1540-4fe6-a87c-449593959874");
                                g.setColor(c);
                            }
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "965b66d5-c896-425a-ae9b-d10019bab9e4");
                            break;
                        // NOI18N
                        case 'P':
                        case // NOI18N
                                'p':
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "82e98615-07db-49de-ac74-4fcd14e9bdc8");
                            if (style == STYLE_WORDWRAP) {
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "481ac915-ae90-4c59-ace7-99653b04322e");
                                x = origX;
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "513a9bf7-5b7d-4297-8495-7040c2e05664");
                                int lineHeight = g.getFontMetrics().getHeight();
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "85368519-b590-4330-8854-25ea1a91a96b");
                                y += (lineHeight + (lineHeight / 2));
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a9484832-a811-4fd9-ac48-5b7f9a44335f");
                                heightPainted = y + lineHeight;
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "809e20ad-24f7-4695-b257-2459c81f8e21");
                                widthPainted = 0;
                            }
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cbc25602-1728-4319-8e81-438a20b4a0d5");
                            break;
                        case 'H':
                        case // Just an opening HTML tag
                                'h':
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5b603a8b-cd9e-48de-9ce7-eabc64904fbe");
                            if (pos == 1) {
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0ee8086c-dcfa-46d9-aeec-0057ea7e505b");
                                break;
                            } else {
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fc103b39-f23c-4f80-adc3-63cafd9e6ea3");
                                // fallthrough warning
                                // NOI18N
                                throwBadHTML("Malformed or unsupported HTML", pos, chars);
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f707df8e-9692-46f2-80cc-3b865915f825");
                                break;
                            }
                        default:
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c49a7bad-b0bf-4845-8cba-c0a31c888ef7");
                            // NOI18N
                            throwBadHTML("Malformed or unsupported HTML", pos, chars);
                    }
                }
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cac6d03a-2586-4639-b607-ee2b7ad48d52");
                pos = tagEnd + (done ? 0 : 1);
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d6d6d38b-cb9f-4541-bd57-2ac23ed83e9e");
                inTag = false;
            } else {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d427fd12-5a23-45e9-a6b1-fe59fc8da668");
                // Okay, we're not in a tag, we need to paint
                if (lastWasWhitespace) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8d7846fa-4b97-4b63-aa71-d2b89e62c61e");
                    // Skip multiple whitespace characters
                    while ((pos < (s.length() - 1)) && Character.isWhitespace(chars[pos])) {
                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6823ed69-b98f-421c-b254-62b86659012b");
                        pos++;
                    }
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "569ab36e-22d4-43f6-9206-ab0386134cdc");
                    // otherwise could get an AIOOBE here
                    if (pos == (chars.length - 1)) {
                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "dba3eeea-0935-468c-98a6-74b98b999858");
                        return (style != STYLE_WORDWRAP) ? widthPainted : heightPainted;
                    }
                }
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "dd60954b-22e7-4e87-80a6-a7a6c547917d");
                // Flag to indicate if an ampersand entity was processed,
                // so the resulting & doesn't get treated as the beginning of
                // another entity (and loop endlessly)
                boolean isAmp = false;
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "094a6720-8ce1-4d52-8c56-44e827c41006");
                // Flag to indicate the next found < character really should
                // be painted (it came from an entity), it is not the beginning
                // of a tag
                boolean nextLtIsEntity = false;
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f85d1112-dcb0-4935-b877-459fe1cb249f");
                int nextTag = chars.length - 1;
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "de5fe843-3e81-4800-adff-c1f18c42cac8");
                if ((chars[pos] == '&')) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1a7654b5-2753-44fe-9e5e-fb4fcc88d655");
                    // NOI18N
                    boolean inEntity = pos != (chars.length - 1);
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "425cf01d-33c2-4643-bf61-725421ec3f34");
                    if (inEntity) {
                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0c046f22-3d10-4922-803a-db178e2a5f8e");
                        int newPos = substEntity(chars, pos + 1);
                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2d8676c8-d78b-424b-9b9b-f4b169edda4d");
                        inEntity = newPos != -1;
                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8d883c96-bc73-4199-9543-37cf36fa2073");
                        if (inEntity) {
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fa719956-9ed6-4102-ac72-f4e60f3aad02");
                            pos = newPos;
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3a142122-bf06-45af-a7c9-6d9d8e9d7beb");
                            // NOI18N
                            isAmp = chars[pos] == '&';
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "21bc5fe4-6313-46cd-a6fd-5aa07ae13459");
                            nextLtIsEntity = chars[pos] == '<';
                        } else {
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4c99ccae-a802-444a-ba53-9f320dca729b");
                            nextLtIsEntity = false;
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "347e4219-957f-4b23-b242-7499a53f91c2");
                            isAmp = true;
                        }
                    }
                } else {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c5fc7451-9375-425e-8275-d0dc1341c945");
                    nextLtIsEntity = false;
                }
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "14fb43e2-9fbd-40de-a251-5e03ac9dddf2");
                for (int i = pos; i < chars.length; i++) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "244bf427-27dc-4275-a7e6-f016df457799");
                    if ((chars[i] == '<' && !nextLtIsEntity) || (chars[i] == '&' && !isAmp && i != chars.length - 1)) {
                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e927fe08-720a-401a-9f21-e97bd66ed959");
                        nextTag = i - 1;
                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "654a5b99-6657-48fb-9726-acdbe83fffd1");
                        break;
                    }
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fff41d9d-e602-42b9-8e12-4b5448b9447e");
                    // Reset these flags so we don't skip all & or < chars for the rest of the string
                    isAmp = false;
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9594c563-b3b2-4f5a-b747-c0d97f8ec8ce");
                    nextLtIsEntity = false;
                }
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3607ec39-54b0-4815-8024-48cd79ff951f");
                FontMetrics fm = g.getFontMetrics();
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6a3de9cc-91aa-44eb-86f3-f42e26ef4054");
                // Get the bounds of the substring we'll paint
                Rectangle2D r = fm.getStringBounds(chars, pos, nextTag + 1, g);
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e823be3a-12c6-4fcf-9333-d939526d9da1");
                if (Utilities.isMac()) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "05e691aa-cda2-4a6b-8b6b-fabd7bfbf359");
                    // #54257 - on macosx + chinese/japanese fonts, the getStringBounds() method returns bad value
                    r.setRect(r.getX(), r.getY(), (double) fm.stringWidth(new String(chars, pos, nextTag - pos + 1)), r.getHeight());
                }
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f973dda2-12ba-4adc-967c-2fb78c2550f6");
                // Store the height, so we can add it if we're in word wrap mode,
                // to return the height painted
                lastHeight = r.getHeight();
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "53e27eb5-4c20-42d6-bda2-bdd65cc6f202");
                // Work out the length of this tag
                int length = (nextTag + 1) - pos;
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a951de0b-5a0f-4b0f-8f42-b06bc4f8435f");
                // Flag to be set to true if we run out of space
                boolean goToNextRow = false;
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e1a1f3f4-6cfd-46b2-ab86-c25ae4ad44bd");
                // Flag that the current line is longer than the available width,
                // and should be wrapped without finding a word boundary
                boolean brutalWrap = false;
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8b974487-02b2-49be-b38e-243917952db1");
                // Work out the per-character avg width of the string, for estimating
                // when we'll be out of space and should start the ... in truncate
                // mode
                double chWidth;
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4c38eb4a-ac48-4ee8-b20f-9a6374f8674e");
                if (truncated) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f2022f30-1e42-4d99-be4a-d5607125fc8e");
                    // if we're truncating, use the width of one dot from an
                    // ellipsis to get an accurate result for truncation
                    chWidth = dotWidth;
                } else {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "85ca4b4d-b356-4697-b8e2-79526e5af8b7");
                    // calculate an average character width
                    chWidth = r.getWidth() / (nextTag + 1 - pos);
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e404fac5-5704-4444-85f9-41a552b35519");
                    // can return this sometimes, so handle it
                    if ((chWidth == Double.POSITIVE_INFINITY) || (chWidth == Double.NEGATIVE_INFINITY)) {
                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0acee6df-310d-41bd-9626-fbc2050a4c61");
                        chWidth = fm.getMaxAdvance();
                    }
                }
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "aa1dbdcf-3d26-4e40-9dbb-712a7e0e7c58");
                if (((style != STYLE_CLIP) && ((style == STYLE_TRUNCATE) && ((widthPainted + r.getWidth()) > (w)))) || /**
                 * mkleint - commented out the "- (chWidth *3) because it makes no sense to strip the text and add dots when it fits exactly
                 * into the rendering rectangle.. with this condition we stripped even strings that came close to the limit..
                 */
                        ((style == STYLE_WORDWRAP) && ((widthPainted + r.getWidth()) > w))) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "743d4f0f-15a9-4a21-99db-e7e8ed0de1c9");
                    if (chWidth > 3) {
                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "202613c5-3d36-4854-bcaa-f44b69222659");
                        double pixelsOff = (widthPainted + (r.getWidth() + 5)) - w;
                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "801cdebc-34c4-46b9-9c71-6cee79e10c97");
                        double estCharsOver = pixelsOff / chWidth;
                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1ea10619-7139-464b-806f-fe6f9aa2a168");
                        if (style == STYLE_TRUNCATE) {
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d65828a5-1a4e-4b15-98e4-f0575db90ff3");
                            int charsToPaint = Math.round(Math.round(Math.ceil((w - widthPainted) / chWidth)));
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9b0f942a-193c-439a-8742-113e435e0b2c");
                            /*                            System.err.println("estCharsOver = " + estCharsOver);
                                                        System.err.println("Chars to paint " + charsToPaint + " chwidth = " + chWidth + " widthPainted " + widthPainted);
                                                        System.err.println("Width painted + width of tag: " + (widthPainted + r.getWidth()) + " available: " + w);
                             */
                            int startPeriodsPos = (pos + charsToPaint) - 3;
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fea8f8fc-dac4-4b0e-be13-16a296660b3e");
                            if (startPeriodsPos >= chars.length) {
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "43322b3b-f4b8-468a-a782-6cd1dfe96e6f");
                                startPeriodsPos = chars.length - 4;
                            }
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7afb1e75-637b-4896-9024-9f488e50ff6a");
                            length = (startPeriodsPos - pos);
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3693e612-9979-4577-bdd2-d8f2abd4b45f");
                            if (length < 0) {
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "be053cd9-46cd-4b70-bf4c-e0b1d1e03597");
                                length = 0;
                            }
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "98c24df1-6dd8-4afb-b3d8-4e0c9a93829c");
                            r = fm.getStringBounds(chars, pos, pos + length, g);
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "58d659fa-fff2-4ede-9c91-822f3fc53332");
                            if (Utilities.isMac()) {
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f8525320-11d1-4a2c-9596-fcdeeb914eb9");
                                // #54257 - on macosx + chinese/japanese fonts, the getStringBounds() method returns bad value
                                r.setRect(r.getX(), r.getY(), (double) fm.stringWidth(new String(chars, pos, length)), r.getHeight());
                            }
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "abd00560-4d63-4a81-9162-c430d608d982");
                            // System.err.println("Truncated set to true at " + pos + " (" + chars[pos] + ")");
                            truncated = true;
                        } else {
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "74c8fe7a-cfa4-46ae-985c-b465a1763151");
                            // Word wrap mode
                            goToNextRow = true;
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bc29ddd2-37f3-4126-981a-6dc03509d175");
                            int lastChar = new Double(nextTag - estCharsOver).intValue();
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0d3cfb91-10db-45a1-baae-6c88532a7afc");
                            // Unlike Swing's word wrap, which does not wrap on tag boundaries correctly, if we're out of space,
                            // we're out of space
                            brutalWrap = x == 0;
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "557c59c1-7b6a-42e5-af4d-83b30532efc6");
                            for (int i = lastChar; i > pos; i--) {
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d0f2822b-3d1f-4c77-82ab-ca6e8166deb3");
                                lastChar--;
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "eb111033-52e2-48c4-a656-6c7874c06286");
                                if (Character.isWhitespace(chars[i])) {
                                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1cd4bca0-8909-4fbb-aef7-2bb024084844");
                                    length = (lastChar - pos) + 1;
                                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ccf46512-9e5b-42b2-a066-099f94c03c8a");
                                    brutalWrap = false;
                                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "44288f20-34f7-41a5-9569-efdf6eed0900");
                                    break;
                                }
                            }
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "381b4bd6-c0c2-4b91-a22b-06795a4bfa90");
                            if ((lastChar <= pos) && (length > estCharsOver) && !brutalWrap) {
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ad5026e6-0fbf-4555-acc7-2ea347c85c9c");
                                x = origX;
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0bee0348-aaad-46a4-8bab-a273f2f75eec");
                                y += r.getHeight();
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "91a9e922-5a15-428f-b426-d956ff71ec4d");
                                heightPainted += r.getHeight();
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ea6fe753-4393-411b-b86e-ee378a7088b5");
                                boolean boundsChanged = false;
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "74e14808-1810-4a0d-9dcd-52491f83b43f");
                                while (!done && Character.isWhitespace(chars[pos]) && (pos < nextTag)) {
                                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "10bffc47-bec7-4f64-b2ab-e49c7d4972e6");
                                    pos++;
                                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b10c721b-6cc2-4420-9c68-4f25c04c5ac6");
                                    boundsChanged = true;
                                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1d23ec21-272c-4d3a-8106-158533a628e9");
                                    done = pos == (chars.length - 1);
                                }
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "992ab330-52b3-402a-a340-eaed6bd0a333");
                                if (pos == nextTag) {
                                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5b997cec-173f-45b7-a804-0a958d85caa9");
                                    lastWasWhitespace = true;
                                }
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c4723737-bf2a-4c85-8087-54abc6ccc9b1");
                                if (boundsChanged) {
                                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6bc3a147-4e90-4b06-8663-4f121ee4ecd3");
                                    // recalculate the width we will add
                                    r = fm.getStringBounds(chars, pos, nextTag + 1, g);
                                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1213a2b0-e792-4d43-9edc-f9f634dded40");
                                    if (Utilities.isMac()) {
                                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a590d314-1ddf-4104-a473-df9a6153b630");
                                        // #54257 - on macosx + chinese/japanese fonts, the getStringBounds() method returns bad value
                                        r.setRect(r.getX(), r.getY(), (double) fm.stringWidth(new String(chars, pos, nextTag - pos + 1)), r.getHeight());
                                    }
                                }
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8bbd9703-f330-4f9c-b387-6a6413984018");
                                goToNextRow = false;
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a3d9595f-3e4c-453a-ae8d-a2de799109f5");
                                widthPainted = 0;
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3d69ccc6-84ae-4c72-844b-305568fdcb89");
                                if (chars[pos - 1 + length] == '<') {
                                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "48f64552-d4e0-432f-9f13-a698181a7db1");
                                    length--;
                                }
                            } else if (brutalWrap) {
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0681a114-46ba-4077-996b-2f6a805dd8d9");
                                // wrap without checking word boundaries
                                length = (new Double((w - widthPainted) / chWidth)).intValue();
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "959fe768-b668-4f23-a263-7065f1b08b7d");
                                if ((pos + length) > nextTag) {
                                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5ea51112-7a16-4b38-8bac-bd08619e5eb8");
                                    length = (nextTag - pos);
                                }
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7b62f95e-6cfb-4f39-a874-494782b4ef2a");
                                goToNextRow = true;
                            }
                        }
                    }
                }
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bcd55cc6-af49-480a-a59f-24828ac05957");
                if (!done) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fa2ba78b-5fb1-4288-919b-5cf506355af1");
                    if (paint) {
                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6cbb9e92-584c-4cd9-91f2-c278f8724778");
                        g.drawChars(chars, pos, length, x, y);
                    }
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f81eaf8c-0425-4a8d-901c-5855345f9edb");
                    if (strikethrough || underline || link) {
                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cc586c1e-ed01-4fa6-b141-a5f028c51412");
                        LineMetrics lm = fm.getLineMetrics(chars, pos, length - 1, g);
                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "052a19bb-ed82-4da3-9517-b88d2de52bc2");
                        int lineWidth = new Double(x + r.getWidth()).intValue();
                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8c2d9c4a-44e1-455d-8b58-d784db1c9b1d");
                        if (paint) {
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "43c5d5d5-9e61-48ea-8794-a5f028b2ec28");
                            if (strikethrough) {
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ff146213-60c6-49d3-bddb-a8201b692ef4");
                                int stPos = Math.round(lm.getStrikethroughOffset()) + g.getFont().getBaselineFor(chars[pos]) + 1;
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0c8c6f31-c551-42c9-870b-437b9ad668db");
                                // PENDING - worth supporting with g.setStroke()? A one pixel line is most likely
                                // good enough
                                // int stThick = Math.round (lm.getStrikethroughThickness());
                                g.drawLine(x, y + stPos, lineWidth, y + stPos);
                            }
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2d0397ea-e848-4a47-8c87-5a23efd33da1");
                            if (underline || link) {
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0e3c6d6b-57f6-44b4-9f6c-dc48f05d784f");
                                int stPos = Math.round(lm.getUnderlineOffset()) + g.getFont().getBaselineFor(chars[pos]) + 1;
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9dc89daa-8335-4099-8520-41ab28e6b8ac");
                                // PENDING - worth supporting with g.setStroke()? A one pixel line is most likely
                                // good enough
                                // int stThick = new Float (lm.getUnderlineThickness()).intValue();
                                g.drawLine(x, y + stPos, lineWidth, y + stPos);
                            }
                        }
                    }
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fbca2a81-8cba-4f95-bbe6-c53f41abd0f1");
                    if (goToNextRow) {
                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "39763cdf-a25d-4dbf-b65b-8ce266627af0");
                        // if we're in word wrap mode and need to go to the next
                        // line, reconfigure the x and y coordinates
                        x = origX;
                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bd76f2c6-c46a-459a-9b8b-67ac383f0596");
                        y += r.getHeight();
                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5a5151aa-c330-4435-9c76-2ae8f2d94fbd");
                        heightPainted += r.getHeight();
                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "71758747-76d3-40a0-9bd0-d7024ff7a35b");
                        widthPainted = 0;
                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "29de5a15-5695-44ab-89a9-c0a2cda8ec1f");
                        pos += (length);
                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9a9d0941-0179-4710-b990-c3f76b096dc3");
                        // skip any leading whitespace
                        while ((pos < chars.length) && (Character.isWhitespace(chars[pos])) && (chars[pos] != '<')) {
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "32ee7065-8537-4512-89c4-92f57a826448");
                            pos++;
                        }
                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "305736ad-2fb5-4062-bda5-1c92e31549d6");
                        lastWasWhitespace = true;
                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0eff94cf-7f26-425f-aebf-898a93038faa");
                        done |= (pos >= chars.length);
                    } else {
                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "94bc6c46-04b8-4bef-8243-3f95a3186a3b");
                        x += r.getWidth();
                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "78c3e543-fd60-4bc3-a06e-aef4054bead7");
                        widthPainted += r.getWidth();
                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2a1f88c1-7b82-447a-b6ae-b0df0fdcb721");
                        lastWasWhitespace = Character.isWhitespace(chars[nextTag]);
                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7eb0c45e-bc8e-415c-9cd7-c2d3b788b3e1");
                        pos = nextTag + 1;
                    }
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f9920fe1-e43c-402e-b70d-17ef01de1cb6");
                    done |= (nextTag == chars.length);
                }
            }
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d406362c-4126-4df4-af3f-ffeeb86e415d");
        if (style != STYLE_WORDWRAP) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f2a14286-ce92-423a-bf61-a1703134a9be");
            return widthPainted;
        } else {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ce99adab-5b1d-4220-97a7-f9fc18c88f8b");
            return heightPainted + lastHeight;
        }
    }

    /**
     * Parse a font color tag and return an appropriate java.awt.Color instance
     */
    private static Color findColor(final char[] ch, final int pos, final int tagEnd) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6e81d05f-04df-4354-95f8-7fc6eeaaf724");
        int colorPos = pos;
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bc8d76ab-573f-4c9e-ac73-6bc271fb3fea");
        boolean useUIManager = false;
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9d109e99-f6d7-4f19-b0a9-85905e5d6070");
        for (int i = pos; i < tagEnd; i++) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d612377c-c574-476c-9164-932551835227");
            if (ch[i] == 'c') {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8bc2c111-b770-4ca5-a398-1ff84e87c589");
                // #195703 - check for broken HTML
                if (i + 6 >= ch.length)
                    break;
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3f2530c4-f7bf-4080-86f8-d6966921a9fb");
                colorPos = i + 6;
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "98bf4692-4e87-426b-84e3-d63bbf6721af");
                if ((ch[colorPos] == '\'') || (ch[colorPos] == '"')) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e834e3c8-68a6-4530-a147-b0056a390360");
                    colorPos++;
                }
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e3a902d3-3180-431b-b9e9-2f388ca07e36");
                // skip the leading # character
                if (ch[colorPos] == '#') {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "450dce3b-cb9a-4109-88d4-e2c7c1c893f0");
                    colorPos++;
                } else if (ch[colorPos] == '!') {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "68073a52-322d-4298-802b-f5e500d3b36d");
                    useUIManager = true;
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a06b791e-1096-433a-8442-1962f9aa8976");
                    colorPos++;
                }
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "00f03f94-0732-4696-9afb-002fe5dd3924");
                break;
            }
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b67cd1a0-03db-410e-8c87-4527872f8838");
        if (colorPos == pos) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f1cb8121-9b0c-4ffc-b2e4-390ba2199bc4");
            // NOI18N
            String out = "Could not find color identifier in font declaration";
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "351a018c-9389-4efb-b2cc-e1a15036056a");
            throwBadHTML(out, pos, ch);
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3c36275e-b6c8-478e-9265-ff5fe4397e1b");
        // Okay, we're now on the first character of the hex color definition
        String s;
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2f17ed29-e3f1-42c6-bfdf-2c782e2ea92c");
        if (useUIManager) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c8f1c35b-12e7-4226-bcd5-b2532e912fd0");
            int end = ch.length - 1;
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6e1db115-2bd7-4834-9875-8af2896c1832");
            for (int i = colorPos; i < ch.length; i++) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b2144557-5a26-4e6f-adf2-4265c18fe882");
                if ((ch[i] == '"') || (ch[i] == '\'')) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0ef1cd42-97a4-4330-9fcb-d847913a1426");
                    // NOI18N
                    end = i;
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e1209baf-b74b-4bbb-a9b2-cd693a5045d4");
                    break;
                }
            }
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "19d67a09-a307-44fb-81ab-24304d29f488");
            s = new String(ch, colorPos, end - colorPos);
        } else {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "595b1ccf-a2ad-4038-b58e-32b34076ae03");
            s = new String(ch, colorPos, Math.min(ch.length - colorPos, 6));
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6dcc345a-b28d-47c8-967b-ed80fb797931");
        Color result = null;
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d5d43abe-1be7-4653-b8d7-0b748bdc0b02");
        if (useUIManager) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7d3f4ee6-bbae-44c5-95af-95ec9a3cab3b");
            result = UIManager.getColor(s);
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1ced57c7-de7f-4130-acf6-aa18974c94b2");
            // Not all look and feels will provide standard colors; handle it gracefully
            if (result == null) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9e237673-ee66-46c5-b596-ee9ed5c821d8");
                throwBadHTML(// NOI18N
                        "Could not resolve logical font declared in HTML: " + s, pos, ch);
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cdc278d6-3978-458a-8a0d-48735b0084c4");
                // NOI18N
                result = UIManager.getColor("textText");
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e20ad515-9765-4bce-87e8-1611e8b82a54");
                // Avoid NPE in headless situation?
                if (result == null) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b2d5aaa0-591b-4df0-bdf8-5a6ba6e9410d");
                    result = Color.BLACK;
                }
            }
        } else {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "964ecb74-6ac8-4080-9fd1-d958e3056d66");
            try {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8ffa9b14-5881-48ad-9fc3-ac136ae0ef7b");
                int rgb = Integer.parseInt(s, 16);
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "884afff4-68fa-4fec-98a5-1adc48ea06ea");
                result = new Color(rgb);
            } catch (NumberFormatException nfe) {
                // NOI18N
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0b4e451c-7e4b-4f69-89df-e0bd3c859815");
                throwBadHTML(// NOI18N
                        "Illegal hexadecimal color text: " + s + " in HTML string", colorPos, ch);
            }
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "77cb226e-d37b-4f69-b4f2-e684ad718e75");
        if (result == null) {
            // NOI18N
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b2b8f37d-4eec-407f-bc20-cf038a413967");
            throwBadHTML(// NOI18N
                    "Unresolvable html color: " + s + " in HTML string \n  ", pos, ch);
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "74f7e6f1-173e-4d4a-a7be-7bf7891e6c3c");
        return result;
    }

    /**
     * Workaround for Apple bug 3644261 - after using form editor, all boldface
     * fonts start showing up with incorrect metrics, such that all boldface
     * fonts in the entire IDE are displayed 12px below where they should be.
     * Embarrassing and awful.
     */
    private static final Font deriveFont(Font f, int style) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "838267f7-e353-498d-bc0d-ac57dc550831");
        // return f.deriveFont(style);
        // see #49973 for details.
        Font result = Utilities.isMac() ? new Font(f.getName(), style, f.getSize()) : f.deriveFont(style);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5df1f94b-2c69-4135-992c-85cc0fd0f7ec");
        return result;
    }

    /**
     * Find an entity at the passed character position in the passed array.
     * If an entity is found, the trailing ; character will be substituted
     * with the resulting character, and the position of that character
     * in the array will be returned as the new position to render from,
     * causing the renderer to skip the intervening characters
     */
    private static final int substEntity(char[] ch, int pos) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3f8f5901-0139-432b-ab27-a138caf77896");
        // There are no 1 character entities, abort
        if (pos >= (ch.length - 2)) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0453783a-84fc-43d3-acb5-3a874b9f5d49");
            return -1;
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bdca4cc1-0f8f-4586-92f6-f15dbcc42406");
        // if it's numeric, parse out the number
        if (ch[pos] == '#') {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "13733e8c-5402-4141-bd99-416240f5eccd");
            return substNumericEntity(ch, pos + 1);
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "57e4b8ed-e225-40f7-844b-e6f2e5cf94af");
        // Okay, we've potentially got a named character entity. Try to find it.
        boolean match;
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "25aa074e-b03a-40ee-be92-e491f9cfd81b");
        for (int i = 0; i < entities.length; i++) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b3125d27-a48e-41fa-803f-cc2eff4e0622");
            char[] c = (char[]) entities[i];
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c9fb7e5e-ff6e-4972-8583-a6e3ded06b25");
            match = true;
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "db318798-6a1a-43f8-8f32-c62440cdfa5d");
            if (c.length < (ch.length - pos)) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9cde8bfb-bee9-4f4e-9a79-3ffc1e0873f7");
                for (int j = 0; j < c.length; j++) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a3fe12f4-9c82-4fe8-abcd-15addfa4a097");
                    match &= (c[j] == ch[j + pos]);
                }
            } else {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2a831bb1-a50c-47ec-8d3f-5e09b7c1c9a8");
                match = false;
            }
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b736d166-8036-40ac-960d-e327ff1debd8");
            if (match) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b2d36efd-aea2-4cba-8bc8-4dab7afedab8");
                // if it's a match, we still need the trailing ;
                if (ch[pos + c.length] == ';') {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0a3e9ce4-6919-40eb-8441-52895e6cf51f");
                    // NOI18N
                    // substitute the character referenced by the entity
                    ch[pos + c.length] = entitySubstitutions[i];
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4276546a-5125-48ce-af81-176cfd17e6e1");
                    return pos + c.length;
                }
            }
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8414f9e5-f711-4270-957e-949ef95ef209");
        return -1;
    }

    /**
     * Finds a character defined as a numeric entity (e.g. &amp;#8222;)
     * and replaces the trailing ; with the referenced character, returning
     * the position of it so the renderer can continue from there.
     */
    private static final int substNumericEntity(char[] ch, int pos) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9988fc02-e22a-4cac-9532-f09e25156c61");
        for (int i = pos; i < ch.length; i++) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "89ab14fc-b428-46f1-a1d5-a25ae18f0551");
            if (ch[i] == ';') {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1edbeee8-d957-4713-a8ee-b13ee9e97260");
                try {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3f4a3c89-cf03-4e07-ae08-49d73575fd72");
                    ch[i] = (char) Integer.parseInt(new String(ch, pos, i - pos));
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8fe84089-5106-4af2-8c0f-cc4ab82f58ab");
                    return i;
                } catch (NumberFormatException nfe) {
                    // NOI18N
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d044a304-69e6-409d-9de0-fb1fa7b4d23b");
                    throwBadHTML(// NOI18N
                            "Unparsable numeric entity: " + new String(ch, pos, i - pos), pos, ch);
                }
            }
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7f9f2b04-cac3-40be-9b03-536771b0839e");
        return -1;
    }

    /**
     * Throw an exception for unsupported or bad html, indicating where the problem is
     * in the message
     */
    private static void throwBadHTML(String msg, int pos, char[] chars) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "108fe0d2-6e11-49ae-928f-cd3aa496bac1");
        char[] chh = new char[pos];
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c95b0197-ea2a-483a-bc2a-c10010905d15");
        // NOI18N
        Arrays.fill(chh, ' ');
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d8ba17c1-7c5c-40c2-b187-043ad4a00496");
        // NOI18N
        chh[pos - 1] = '^';
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b4e15187-c288-478d-81f0-2eae6dbe6ca0");
        String out = msg + "\n  " + new String(chars) + "\n  " + new String(chh) + "\n Full HTML string:" + // NOI18N
                new String(chars);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "05a93e29-5116-4daf-8b50-9e2285f85ad1");
        if (!STRICT_HTML) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ffeab357-2100-4965-b9b6-686fc3f11901");
            if (LOG.isLoggable(Level.WARNING)) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f2ee7f8f-1690-4103-8871-e2018b6f8fb5");
                if (badStrings == null) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5dd1a830-993b-4a9f-9f4d-dfa5ff96321d");
                    badStrings = new HashSet<String>();
                }
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e46d86a6-a6e6-4d8e-bc97-bc13f5d6ca08");
                if (!badStrings.contains(msg)) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a297aa97-b612-44b6-a861-5f533bc5bcc9");
                    // bug, issue 38372 - log messages containing
                    // newlines are truncated - so for now we iterate the
                    // string we've just constructed
                    StringTokenizer tk = new StringTokenizer(out, "\n", false);
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "39a47736-5551-4d6d-9084-a8839db0426e");
                    while (tk.hasMoreTokens()) {
                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6a943e9b-89b5-4fe2-b8a6-72c8bc33ffbd");
                        LOG.warning(tk.nextToken());
                    }
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d6ffd9b5-a92d-4846-a9f0-d5d24c469771");
                    // NOPMD
                    badStrings.add(msg.intern());
                }
            }
        } else {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ecb45115-dbd9-45ac-9fc6-5cc5dc26a78e");
            throw new IllegalArgumentException(out);
        }
    }

    /**
     * Interface aggregating table, tree, and list cell renderers.
     * @see #createRenderer
     * @see #createLabel
     */
    public interface Renderer extends TableCellRenderer, TreeCellRenderer, ListCellRenderer {

        /**
         * Indicate that the component being rendered has keyboard focus.  NetBeans requires that a different
         * selection color be used depending on whether the view has focus.
         *
         * @param parentFocused Whether or not the focused selection color should be used
         */
        void setParentFocused(boolean parentFocused);

        /**
         * Indicate that the text should be painted centered below the icon.  This is primarily used
         * by org.openide.explorer.view.IconView
         *
         * @param centered Whether or not centered painting should be used.
         */
        void setCentered(boolean centered);

        /**
         * Set a number of pixels the icon and text should be indented.  Used by ChoiceView and ListView to
         * fake tree-style nesting.  This value has no effect if {@link #setCentered setCentered(true)} has been called.
         *
         * @param pixels The number of pixels to indent
         */
        void setIndent(int pixels);

        /**
         * Explicitly tell the renderer it is going to receive HTML markup, or it is not.  If the renderer should
         * check the string for opening HTML tags to determine this, don't call this method.  If you <strong>know</strong>
         * the string will be compliant HTML, it is preferable to call this method with true; if you want to intentionally
         * render HTML markup literally, call this method with false.
         *
         * @param val
         */
        void setHtml(boolean val);

        /**
         * Set the rendering style - this can be JLabel-style truncated-with-ellipsis (...) text, or clipped text.
         * The default is {@link #STYLE_CLIP}.
         *
         * @param style The text style
         */
        void setRenderStyle(int style);

        /**
         * Set the icon to be used for painting
         *
         * @param icon An icon or null
         */
        void setIcon(Icon icon);

        /**
         * Clear any stale data from previous use by other components,
         * clearing the icon, text, disabled state and other customizations, returning the component
         * to its initialized state.  This is done automatically when get*CellRenderer() is called,
         * and to the shared instance when {@link #createLabel} is called.<p>
         * Users of the static {@link #createLabel} method may want to call this method if they
         * use the returned instance more than once without again calling {@link #createLabel}.
         */
        void reset();

        /**
         * Set the text to be displayed.  Use this if the object being rendered's toString() does not
         * return a real user-displayable string, after calling get**CellRenderer().  Typically after calling
         * this one calls {@link #setHtml} if the text is known to either be or not be HTML markup.
         *
         * @param txt The text that should be displayed
         */
        void setText(String txt);

        /**
         * Convenience method to set the gap between the icon and text.
         *
         * @param gap an integer number of pixels
         */
        void setIconTextGap(int gap);
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