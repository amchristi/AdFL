package org.netbeans.modules.openide.awt;

import java.awt.Component;
import java.awt.GraphicsEnvironment;
import javax.swing.JComponent;
import javax.swing.JMenuItem;
import org.openide.util.Lookup;
import org.openide.util.actions.Presenter;
import java.io.IOException;
import org.openide.util.test.AnnotationProcessorTestUtils;
import java.util.Collections;
import java.util.List;
import org.openide.awt.ActionID;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.ByteArrayOutputStream;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ActionMap;
import javax.swing.JSeparator;
import org.netbeans.junit.NbTestCase;
import org.openide.awt.ActionReference;
import org.openide.awt.ActionReferences;
import org.openide.awt.ActionRegistration;
import org.openide.awt.Actions;
import org.openide.awt.DynamicMenuContent;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.util.ContextAwareAction;
import org.openide.util.lookup.AbstractLookup;
import org.openide.util.lookup.InstanceContent;
import static org.junit.Assert.*;

/**
 * @author Jaroslav Tulach <jtulach@netbeans.org>
 */
public class ActionProcessorTest extends NbTestCase {

    @ActionRegistration(displayName = "#Key", iconBase = "org/openide/awt/TestIcon.png")
    @ActionID(category = "Edit", id = "my.field.action")
    public static final String ACTION_MAP_KEY = "my.action.map.key";

    @ActionRegistration(displayName = "somename", surviveFocusChange = true)
    @ActionID(category = "Windows", id = "my.survival.action")
    public static final String SURVIVE_KEY = "somekey";

    static {
        System.setProperty("java.awt.headless", "true");
    }

    public ActionProcessorTest(String n) {
        super(n);
    }

    @ActionRegistration(displayName = "#AlwaysOn")
    @ActionID(id = "my.test.Always", category = "Tools")
    @ActionReference(path = "My/Folder", position = 333, name = "D-F6")
    public static final class Always implements ActionListener {

        static int created;

        public Always() {
            created++;
        }

        static int cnt;

        @Override
        public void actionPerformed(ActionEvent e) {
            cnt += e.getID();
        }
    }

    public static final class AlwaysByMethod {

        private AlwaysByMethod() {
        }

        static int created, cnt;

        @ActionRegistration(displayName = "#AlwaysOn")
        @ActionID(id = "my.test.AlwaysByMethod", category = "Tools")
        @ActionReferences({ @ActionReference(path = "Kuk/buk", position = 1, separatorAfter = 2), @ActionReference(path = "Muk/luk", position = 11, separatorBefore = 10) })
        public static ActionListener factory() {
            created++;
            return new ActionListener() {

                @Override
                public void actionPerformed(ActionEvent e) {
                    cnt += e.getID();
                }
            };
        }
    }

    @ActionRegistration(displayName = "#Key", key = "klic")
    @ActionID(category = "Tools", id = "my.action")
    public static final class Callback implements ActionListener {

        static int cnt;

        @Override
        public void actionPerformed(ActionEvent e) {
            cnt += e.getID();
        }
    }

    public static final class NumberLike {

        final int x;

        NumberLike(int x) {
            this.x = x;
        }
    }

    @ActionID(category = "Tools", id = "on.int")
    @ActionRegistration(displayName = "#OnInt")
    public static final class Context implements ActionListener {

        private final int context;

        public Context(NumberLike context) {
            this.context = context.x;
        }

        static int cnt;

        @Override
        public void actionPerformed(ActionEvent e) {
            cnt += context;
        }
    }

    @ActionRegistration(displayName = "#OnInt")
    @ActionID(category = "Tools", id = "on.numbers")
    public static final class MultiContext implements ActionListener {

        private final List<NumberLike> context;

        public MultiContext(List<NumberLike> context) {
            this.context = context;
        }

        static int cnt;

        @Override
        public void actionPerformed(ActionEvent e) {
            for (NumberLike n : context) {
                cnt += n.x;
            }
        }
    }

    @ActionID(category = "eager", id = "direct.one")
    @ActionRegistration(displayName = "Direct Action")
    public static class Direct extends AbstractAction implements Presenter.Menu {

        static int cnt;

        public Direct() {
            cnt++;
        }

        @Override
        public void actionPerformed(ActionEvent e) {
        }

        @Override
        public JMenuItem getMenuPresenter() {
            throw new UnsupportedOperationException("Not supported yet.");
        }
    }

    @ActionID(category = "eager", id = "direct.two")
    @ActionRegistration(displayName = "Direct Action")
    @ActionReference(path = "Shortcuts", name = "C-F2 D-A")
    public static class Direct2 extends AbstractAction implements Presenter.Toolbar {

        static int cnt;

        public Direct2() {
            cnt++;
        }

        @Override
        public void actionPerformed(ActionEvent e) {
        }

        @Override
        public Component getToolbarPresenter() {
            throw new UnsupportedOperationException("Not supported yet.");
        }
    }

    @ActionID(category = "eager", id = "direct.three")
    @ActionRegistration(displayName = "Direct Action")
    public static class Direct3 extends AbstractAction implements Presenter.Popup {

        static int cnt;

        public Direct3() {
            cnt++;
        }

        @Override
        public void actionPerformed(ActionEvent e) {
        }

        @Override
        public JMenuItem getPopupPresenter() {
            throw new UnsupportedOperationException("Not supported yet.");
        }
    }

    @ActionID(category = "eager", id = "direct.four")
    @ActionRegistration(displayName = "Direct Action")
    public static class Direct4 extends AbstractAction implements ContextAwareAction {

        static int cnt;

        public Direct4() {
            cnt++;
        }

        @Override
        public void actionPerformed(ActionEvent e) {
        }

        @Override
        public Action createContextAwareInstance(Lookup actionContext) {
            return this;
        }
    }

    private static class Direct5 extends AbstractAction implements ContextAwareAction {

        static int cnt;

        public Direct5() {
            cnt++;
        }

        @Override
        public void actionPerformed(ActionEvent e) {
        }

        @Override
        public Action createContextAwareInstance(Lookup actionContext) {
            return this;
        }
    }

    @ActionID(category = "eager", id = "direct.six")
    @ActionRegistration(displayName = "Direct Action")
    public static class Direct6 extends AbstractAction implements DynamicMenuContent {

        @Override
        public void actionPerformed(ActionEvent e) {
        }

        @Override
        public JComponent[] getMenuPresenters() {
            return null;
        }

        @Override
        public JComponent[] synchMenuPresenters(JComponent[] items) {
            return null;
        }
    }

    @ActionID(category = "eager", id = "direct.seven")
    @ActionRegistration(displayName = "Direct Action", lazy = false)
    public static class Direct7 extends AbstractAction {

        @Override
        public void actionPerformed(ActionEvent e) {
        }
    }

    @ActionID(category = "eager", id = "direct.eight")
    @ActionRegistration(displayName = "Direct Action", lazy = true)
    public static class Direct8 extends AbstractAction implements ContextAwareAction {

        @Override
        public void actionPerformed(ActionEvent e) {
        }

        @Override
        public Action createContextAwareInstance(Lookup actionContext) {
            return this;
        }
    }

    @ActionID(category = "menutext", id = "namedaction")
    @ActionRegistration(displayName = "This is an Action", menuText = "This is a Menu Action", popupText = "This is a Popup Action")
    public static class NamedAction extends AbstractAction {

        public NamedAction() {
        }

        @Override
        public void actionPerformed(ActionEvent e) {
        }
    }

    @Override
    protected boolean runInEQ() {
        return true;
    }

    @ActionID(category = "eager", id = "direct.five")
    @ActionRegistration(displayName = "Direct Action")
    public static ContextAwareAction direct5() {
        return new Direct5();
    }

    public void testDirectInstanceIfRequested() throws Exception {
        assertEquals("Direct class is created", Direct7.class, Actions.forID("eager", "direct.seven").getClass());
    }
}
