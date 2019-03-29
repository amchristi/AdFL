package org.openide.awt;

import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CannotUndoException;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import javax.swing.undo.UndoableEdit;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.UndoableEditEvent;
import org.netbeans.junit.NbTestCase;
import static org.junit.Assert.*;

/**
 * @author Jaroslav Tulach <jtulach@netbeans.org>
 */
public class UndoRedoTest extends NbTestCase implements ChangeListener {

    private int cnt;

    public UndoRedoTest(String n) {
        super(n);
    }

    private static final class MyEdit implements UndoableEdit, PropertyChangeListener {

        private int undo;

        private int redo;

        private int cnt;

        private boolean ignore;

        private boolean undoFails;

        private boolean significant;

        public MyEdit() {
            this(false);
        }

        public MyEdit(boolean ignore) {
            this.ignore = ignore;
            this.significant = true;
        }

        @Override
        public void propertyChange(PropertyChangeEvent evt) {
            if ("enabled".equals(evt.getPropertyName())) {
                cnt++;
            }
        }

        @Override
        public void undo() throws CannotUndoException {
            if (undoFails) {
                throw new CannotUndoException();
            }
            undo++;
        }

        @Override
        public boolean canUndo() {
            return true;
        }

        @Override
        public void redo() throws CannotRedoException {
            redo++;
        }

        @Override
        public boolean canRedo() {
            return true;
        }

        @Override
        public void die() {
        }

        @Override
        public boolean addEdit(UndoableEdit anEdit) {
            if (anEdit instanceof MyEdit && ((MyEdit) anEdit).ignore) {
                return true;
            }
            return false;
        }

        @Override
        public boolean replaceEdit(UndoableEdit anEdit) {
            return false;
        }

        @Override
        public boolean isSignificant() {
            return significant;
        }

        @Override
        public String getPresentationName() {
            return "My Edit";
        }

        @Override
        public String getUndoPresentationName() {
            return "My Undo";
        }

        @Override
        public String getRedoPresentationName() {
            return "My Redo";
        }

        void setUndoFails(boolean undoFails) {
            this.undoFails = undoFails;
        }

        void setSignificant(boolean significant) {
            this.significant = significant;
        }
    }

    public void testUndoDeliversChangesWithTooManyEdits() {
        Manager ur = new Manager() {

            @Override
            public boolean canUndo() {
                if (super.canUndo()) {
                    undoableEditHappened(new UndoableEditEvent(this, new MyEdit(true)));
                }
                return super.canUndo();
            }
        };
        doUndoRedoTest(ur);
    }

    private void doUndoRedoTest(Manager ur) {
        assertFalse("Nothing to undo", ur.canUndo());
        ur.addChangeListener(this);
        MyEdit me = new MyEdit();
        ur.undoableEditHappened(new UndoableEditEvent(this, me));
        assertChange("One change");
        assertTrue("Can undo now", ur.canUndo());
        ur.undo();
        assertFalse("Cannot undo", ur.canUndo());
        assertChange("Snd change");
        assertTrue("But redo", ur.canRedo());
        ur.redo();
        assertChange("Third change");
        assertEquals("One undo", 1, me.undo);
        assertEquals("One redo", 1, me.redo);
    }

    private void assertChange(String msg) {
        if (cnt == 0) {
            fail(msg);
        }
        cnt = 0;
    }

    @Override
    public void stateChanged(ChangeEvent e) {
        cnt++;
    }
}
