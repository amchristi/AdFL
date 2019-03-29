package org.openide.awt;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.image.BufferedImage;
import junit.framework.TestCase;

/**
 * @author mkleint
 */
public class HtmlRendererTest extends TestCase {

    private Graphics graphic;

    public HtmlRendererTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        BufferedImage waitingForPaintDummyImage = new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB);
        graphic = waitingForPaintDummyImage.getGraphics();
    }

    /**
     * Test of renderHTML method, of class org.openide.awt.HtmlRenderer.
     */
    public void testRenderHTML() throws Exception {
        doTestRender("<html>text</html>");
        doTestRender("<html>text</html");
        doTestRender("<html>text</h");
        doTestRender("<html>text</");
        doTestRender("<html>text<");
        doTestRender("<html>text");
        doTestRender("<html>text</html<html/>");
        doTestRender("<html>text</h</html>");
        doTestRender("<html>text</</html>");
        doTestRender("<html>text<</html>");
        doTestRender("<html>text<</html>&");
        doTestRender("<html>text<sometag");
        doTestRender55310();
        doTestRender("<html><body>text</body>");
    }

    private void doTestRender(String text) {
        HtmlRenderer.renderHTML(text, graphic, 0, 0, 1000, 1000, Font.getFont("Dialog"), Color.RED, HtmlRenderer.STYLE_TRUNCATE, true);
    }

    /**
     * Test issue #55310: AIOOBE from HtmlRenderer.
     *
     * @see http://www.netbeans.org/issues/show_bug.cgi?id=55310
     */
    private void doTestRender55310() {
        doTestRender("<html><b>a </b></html> ");
    }
}
