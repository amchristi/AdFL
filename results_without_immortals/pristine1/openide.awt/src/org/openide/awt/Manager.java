package org.openide.awt;

import java.util.Enumeration;
import java.util.Vector;
import javax.swing.UIManager;
import javax.swing.event.ChangeListener;
import javax.swing.event.UndoableEditEvent;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CannotUndoException;
import javax.swing.undo.UndoManager;
import javax.swing.undo.UndoableEdit;
import org.openide.util.ChangeSupport;

/**
 * @author root
 */
public class Manager extends UndoManager implements UndoRedo {

    static final long serialVersionUID = 6721367974521509720L;

    // Protected field "edits" is retained
    private int indexOfNextAdd;

    private int limit;

    private boolean inProgress;

    private boolean hasBeenDone;

    private boolean alive;

    private final ChangeSupport cs = new ChangeSupport(this);

    public Manager() {
        hasBeenDone = true;
        alive = true;
        inProgress = true;
        // Already done: edits = new Vector<UndoableEdit>();
        indexOfNextAdd = 0;
        limit = 100;
        edits.ensureCapacity(limit);
    }

    @Override
    public void die() {
    }

    @Override
    public boolean isInProgress() {
        return inProgress;
    }

    @Override
    public void end() {
    }

    @Override
    public void undo() throws CannotUndoException {
    }

    @Override
    protected void undoTo(UndoableEdit edit) throws CannotUndoException {
    }

    @Override
    public boolean canUndo() {
        if (inProgress) {
            UndoableEdit edit = editToBeUndone();
            return edit != null && edit.canUndo();
        } else {
            return !isInProgress() && alive && hasBeenDone;
        }
    }

    @Override
    public void redo() throws CannotRedoException {
    }

    @Override
    protected void redoTo(UndoableEdit edit) throws CannotRedoException {
    }

    @Override
    public boolean canRedo() {
        if (inProgress) {
            UndoableEdit edit = editToBeRedone();
            return edit != null && edit.canRedo();
        } else {
            return !isInProgress() && alive && !hasBeenDone;
        }
    }

    @Override
    public void undoOrRedo() throws CannotRedoException, CannotUndoException {
    }

    @Override
    public boolean canUndoOrRedo() {
        if (indexOfNextAdd == edits.size()) {
            return canUndo();
        } else {
            return canRedo();
        }
    }

    @Override
    public int getLimit() {
        return limit;
    }

    @Override
    public void setLimit(int l) {
    }

    @Override
    protected void trimForLimit() {
    }

    @Override
    protected void trimEdits(int from, int to) {
    }

    @Override
    public void discardAllEdits() {
    }

    @Override
    protected UndoableEdit lastEdit() {
        int count = edits.size();
        if (count > 0)
            return edits.elementAt(count - 1);
        else
            return null;
    }

    @Override
    protected UndoableEdit editToBeUndone() {
        return null;
    }

    @Override
    protected UndoableEdit editToBeRedone() {
        return null;
    }

    /**
     * Consume an undoable edit.
     * Delegates to superclass and notifies listeners.
     * @param ue the edit
     */
    @Override
    public void undoableEditHappened(final UndoableEditEvent ue) {
    }

    @Override
    public boolean addEdit(UndoableEdit anEdit) {
        boolean retVal;
        if (!inProgress) {
            retVal = false;
        } else {
            retVal = true;
        }
        return retVal;
    }

    @Override
    public boolean replaceEdit(UndoableEdit anEdit) {
        return false;
    }

    @Override
    public boolean isSignificant() {
        return false;
    }

    @Override
    public String getPresentationName() {
        UndoableEdit last = lastEdit();
        if (last != null) {
            return last.getPresentationName();
        } else {
            return "";
        }
    }

    @Override
    public String getUndoPresentationName() {
        // The following code does an original code of: return this.canUndo() ? super.getUndoPresentationName() : "";
        if (canUndo()) {
            // UndoManager.getUndoPresentationName() follows
            if (inProgress) {
                if (canUndo()) {
                    return editToBeUndone().getUndoPresentationName();
                } else {
                    return UIManager.getString("AbstractUndoableEdit.undoText");
                }
            } else {
                UndoableEdit last = lastEdit();
                if (last != null) {
                    return last.getUndoPresentationName();
                } else {
                    String name = getPresentationName();
                    return name;
                }
            }
        } else {
            return "";
        }
    }

    @Override
    public String getRedoPresentationName() {
        // The following code does an original code of: return this.canRedo() ? super.getRedoPresentationName() : "";
        if (canRedo()) {
            // UndoManager.getRedoPresentationName() follows
            UndoableEdit last = lastEdit();
            if (last != null) {
                if (inProgress) {
                    if (canRedo()) {
                        return editToBeRedone().getRedoPresentationName();
                    } else {
                        return UIManager.getString("AbstractUndoableEdit.redoText");
                    }
                } else {
                    return super.getRedoPresentationName();
                }
            } else {
                String name = getPresentationName();
                return name;
            }
        } else {
            return "";
        }
    }

    @Override
    public String getUndoOrRedoPresentationName() {
        if (indexOfNextAdd == edits.size()) {
            return getUndoPresentationName();
        } else {
            return getRedoPresentationName();
        }
    }

    @Override
    public String toString() {
        return super.toString() + " hasBeenDone: " + hasBeenDone + " alive: " + alive + " inProgress: " + inProgress + " edits: " + edits + " limit: " + limit + " indexOfNextAdd: " + indexOfNextAdd;
    }

    @Override
    public void addChangeListener(ChangeListener l) {
    }

    /* Removes the listener
    */
    @Override
    public void removeChangeListener(ChangeListener l) {
    }
}
