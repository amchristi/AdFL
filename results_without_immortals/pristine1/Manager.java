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
 *
 * @author root
 */
public class Manager extends UndoManager implements UndoRedo {

    static final long serialVersionUID = 6721367974521509720L;

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
        indexOfNextAdd = 0;
        limit = 10;
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
        inProgress = false;
        this.trimEdits(indexOfNextAdd, edits.size() - 1);
    }

    @Override
    public void undo() throws CannotUndoException {
        if (inProgress) {
            UndoableEdit edit = editToBeUndone();
            if (edit == null) {
                throw new CannotUndoException();
            }
            undoTo(edit);
        } else {
            if (!canUndo()) {
                throw new CannotUndoException();
            }
            int i = edits.size() - 1;
            try {
                for (; i >= 0; i--) {
                    edits.get(i).undo();
                }
                hasBeenDone = false;
            } finally {
                if (i != -1) {
                    int size = edits.size();
                    while (++i < size) {
                        edits.get(i).redo();
                    }
                }
            }
        }
        cs.fireChange();
    }

    @Override
    protected void undoTo(UndoableEdit edit) throws CannotUndoException {
        int i = indexOfNextAdd;
        boolean done = false;
        try {
            while (!done) {
                UndoableEdit next = edits.get(--i);
                next.undo();
                done = next == edit;
            }
            indexOfNextAdd = i;
        } finally {
            if (!done) {
                i++;
                for (; i < indexOfNextAdd; i++) {
                    edits.get(i).redo();
                }
            }
        }
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
        if (inProgress) {
            UndoableEdit edit = editToBeRedone();
            if (edit == null) {
                throw new CannotRedoException();
            }
            redoTo(edit);
        } else {
            if (!canRedo()) {
                throw new CannotRedoException();
            }
            int i = 0;
            int size = edits.size();
            try {
                for (; i < size; i++) {
                    edits.get(i).redo();
                }
                hasBeenDone = true;
            } finally {
                if (i != size) {
                    while (--i >= 0) {
                        edits.get(i).undo();
                    }
                }
            }
        }
        cs.fireChange();
    }

    @Override
    protected void redoTo(UndoableEdit edit) throws CannotRedoException {
        int i = indexOfNextAdd;
        boolean done = false;
        try {
            while (!done) {
                UndoableEdit next = edits.elementAt(i++);
                next.redo();
                done = next == edit;
            }
            indexOfNextAdd = i;
        } finally {
            if (!done) {
                i -= 2;
                for (; i >= indexOfNextAdd; i--) {
                    edits.get(i).undo();
                }
            }
        }
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
        if (indexOfNextAdd == edits.size()) {
            undo();
        } else {
            redo();
        }
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
        if (!inProgress) throw new RuntimeException("Attempt to call UndoManager.setLimit() after UndoManager.end() has been called");
        limit = l;
        trimForLimit();
    }

    @Override
    protected void trimForLimit() {
        if (limit >= 0) {
            int size = edits.size();
            if (size > limit) {
                int halfLimit = limit / 2;
                int keepFrom = indexOfNextAdd - 1 - halfLimit;
                int keepTo = indexOfNextAdd - 1 + halfLimit;
                if (keepTo - keepFrom + 1 > limit) {
                    keepFrom++;
                }
                if (keepFrom < 0) {
                    keepTo -= keepFrom;
                    keepFrom = 0;
                }
                if (keepTo >= size) {
                    int delta = size - keepTo - 1;
                    keepTo += delta;
                    keepFrom += delta;
                }
                trimEdits(keepTo + 1, size - 1);
                trimEdits(0, keepFrom - 1);
            }
        }
    }

    @Override
    protected void trimEdits(int from, int to) {
        if (from <= to) {
            for (int i = to; from <= i; i--) {
                UndoableEdit e = edits.elementAt(i);
                e.die();
                edits.removeElementAt(i);
            }
            if (indexOfNextAdd > to) {
                indexOfNextAdd -= to - from + 1;
            } else if (indexOfNextAdd >= from) {
                indexOfNextAdd = from;
            }
        }
    }

    @Override
    public void discardAllEdits() {
        Enumeration cursor = edits.elements();
        while (cursor.hasMoreElements()) {
            UndoableEdit e = (UndoableEdit) cursor.nextElement();
            e.die();
        }
        edits = new Vector<UndoableEdit>();
        indexOfNextAdd = 0;
        cs.fireChange();
    }

    @Override
    protected UndoableEdit lastEdit() {
        int count = edits.size();
        if (count > 0) return edits.elementAt(count - 1); else return null;
    }

    @Override
    protected UndoableEdit editToBeUndone() {
        int i = indexOfNextAdd;
        while (i > 0) {
            UndoableEdit edit = edits.elementAt(--i);
            if (edit.isSignificant()) {
                return edit;
            }
        }
        return null;
    }

    @Override
    protected UndoableEdit editToBeRedone() {
        int count = edits.size();
        int i = indexOfNextAdd;
        while (i < count) {
            UndoableEdit edit = edits.elementAt(i++);
            if (edit.isSignificant()) {
                return edit;
            }
        }
        return null;
    }

    /** Consume an undoable edit.
         * Delegates to superclass and notifies listeners.
         * @param ue the edit
         */
    @Override
    public void undoableEditHappened(final UndoableEditEvent ue) {
        addEdit(ue.getEdit());
        cs.fireChange();
    }

    @Override
    public boolean addEdit(UndoableEdit anEdit) {
        boolean retVal;
        trimEdits(indexOfNextAdd, edits.size() - 1);
        if (!inProgress) {
            retVal = false;
        } else {
            UndoableEdit last = lastEdit();
            if (last == null) {
                edits.addElement(anEdit);
            } else if (!last.addEdit(anEdit)) {
                if (anEdit.replaceEdit(last)) {
                    edits.removeElementAt(edits.size() - 1);
                }
                edits.addElement(anEdit);
            }
            retVal = true;
        }
        indexOfNextAdd = edits.size();
        trimForLimit();
        return retVal;
    }

    @Override
    public boolean replaceEdit(UndoableEdit anEdit) {
        return false;
    }

    @Override
    public boolean isSignificant() {
        Enumeration cursor = edits.elements();
        while (cursor.hasMoreElements()) {
            if (((UndoableEdit) cursor.nextElement()).isSignificant()) {
                return true;
            }
        }
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
        if (canUndo()) {
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
                    if (!"".equals(name)) {
                        name = UIManager.getString("AbstractUndoableEdit.undoText") + " " + name;
                    } else {
                        name = UIManager.getString("AbstractUndoableEdit.undoText");
                    }
                    return name;
                }
            }
        } else {
            return "";
        }
    }

    @Override
    public String getRedoPresentationName() {
        if (canRedo()) {
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
                if (!"".equals(name)) {
                    name = UIManager.getString("AbstractUndoableEdit.redoText") + " " + name;
                } else {
                    name = UIManager.getString("AbstractUndoableEdit.redoText");
                }
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
        cs.addChangeListener(l);
    }

    @Override
    public void removeChangeListener(ChangeListener l) {
        cs.removeChangeListener(l);
    }
}
