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
import java.io.*;

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
        // Integer.MAX_VALUE / 50;
        limit = 10;
        edits.ensureCapacity(limit);
    }

    @Override
    public void die() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e14b75de-b578-4712-b461-815548e6d778");
        int size = edits.size();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c81f634d-27f3-418e-8f69-5a12b0eeee0d");
        for (int i = size - 1; i >= 0; i--) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "302603c1-2d2f-476a-af09-7d6ae0327f7a");
            UndoableEdit e = edits.elementAt(i);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4562474d-6895-499c-9c05-d4b477d3d75b");
            e.die();
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "84f4424e-ab4d-42bf-82ba-a7fe10233f7d");
        alive = false;
    }

    @Override
    public boolean isInProgress() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6e1c7b80-6f3e-4bb4-a965-4bc03708db64");
        return inProgress;
    }

    @Override
    public void end() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3e0ceadd-6603-4e85-a6f1-16d740ac3997");
        inProgress = false;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e336cc73-a755-4c25-9156-6522782946b9");
        this.trimEdits(indexOfNextAdd, edits.size() - 1);
    }

    @Override
    public void undo() throws CannotUndoException {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "87677da4-3a4d-4db6-a620-6d2af9629781");
        if (inProgress) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "afc30341-2328-4c21-a976-9f7d42916973");
            UndoableEdit edit = editToBeUndone();
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c5eb059e-064e-40cf-b95f-7d8803d2f3b3");
            if (edit == null) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "67675020-e687-442d-ab1c-1b8cd2c7b7f6");
                throw new CannotUndoException();
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "df468280-acd4-4f29-b4db-ecb7f0f74e48");
            undoTo(edit);
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "39be3661-ef2f-40b8-b700-f73595ca22e4");
            if (!canUndo()) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "032bfd2d-f363-4f79-956e-ba2641b13a9f");
                throw new CannotUndoException();
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9532707c-f143-479f-a604-5b3267f7b317");
            int i = edits.size() - 1;
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "94def759-7821-4964-8d64-50c7891f83b8");
            try {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cfffe06d-ee7c-4c04-a6c5-054f0dd8b6a3");
                for (; i >= 0; i--) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3919e857-a0bb-440f-9000-7d90939602a5");
                    // may throw CannotUndoException
                    edits.get(i).undo();
                }
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "88740f9c-9216-4745-97e4-1fa08e78e281");
                hasBeenDone = false;
            } finally {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d40edbac-085c-4e56-9d08-9410e7e65a99");
                if (i != -1) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4c36bed9-a220-4a7b-8526-2445a3c4681a");
                    // i-th edit's undo failed => redo the ones above
                    int size = edits.size();
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2ff9ea72-29cb-434d-9e75-d37546a04840");
                    while (++i < size) {
                        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b3719b4a-456a-4b89-9473-a52c294d2b06");
                        edits.get(i).redo();
                    }
                }
            }
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6b3f1843-578b-40cc-8515-3ea6cf3c3238");
        cs.fireChange();
    }

    @Override
    protected void undoTo(UndoableEdit edit) throws CannotUndoException {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a20f0c85-27e7-41ad-9fa1-18c1d0555bac");
        int i = indexOfNextAdd;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "70c338f3-2cff-4436-b7bf-3e2eff3b5d8f");
        boolean done = false;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "623a23ba-0fa3-401a-b2da-0ec1a905b022");
        try {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "eea3ba02-cbd8-4583-b188-c09e9999b4ed");
            while (!done) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "58654e9b-0566-411a-9b5b-95deb022624d");
                UndoableEdit next = edits.get(--i);
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e1b1bf3f-1198-41de-a856-9e3fb4c7bbf3");
                // may throw CannotUndoException
                next.undo();
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fa193e4c-0701-4b44-aebb-12e60807fb57");
                done = next == edit;
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bc887921-43b2-4257-9505-984556066764");
            indexOfNextAdd = i;
        } finally {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "771b90f3-0b9a-48bd-95cd-2ba9094186c6");
            if (!done) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c241a993-25c4-4a78-9fe9-8ee6b49eaa65");
                // i-th edit's undo failed => redo the ones above
                i++;
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5a38df4e-55e2-4025-bc9b-1886fa0ce019");
                for (; i < indexOfNextAdd; i++) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b54ac98d-642d-4d46-80d0-da6f31b99e18");
                    edits.get(i).redo();
                }
            }
        }
    }

    @Override
    public boolean canUndo() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "29ba7c86-ec44-4954-b321-f4175e83631c");
        if (inProgress) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "775dc245-54a2-485e-965f-faeac3c7f52a");
            UndoableEdit edit = editToBeUndone();
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cc90f1e7-dda8-406e-9e5f-5c28cac77740");
            return edit != null && edit.canUndo();
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a4f18be4-303a-42da-83dd-b2289693f8db");
            return !isInProgress() && alive && hasBeenDone;
        }
    }

    @Override
    public void redo() throws CannotRedoException {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "32effd5d-18c7-416b-9643-d967078091c6");
        if (inProgress) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6cef07d6-28e7-4b99-adb9-3886d518875d");
            UndoableEdit edit = editToBeRedone();
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7ab727d4-f7dd-4f83-95d5-20a17caf4155");
            if (edit == null) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bff2a493-ab37-494f-96b7-80f93f98f2d4");
                throw new CannotRedoException();
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "614f36e5-0cd9-4857-a17d-d59033e3fb87");
            redoTo(edit);
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "67d72774-4a0c-4f63-848e-518f724718f1");
            if (!canRedo()) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e43f707c-8154-4cca-8631-eafdaabf40a6");
                throw new CannotRedoException();
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bef19354-cfb9-4426-9f9d-18624fa0518e");
            int i = 0;
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "66d7466f-2c97-4aaf-9598-eccf791b7d86");
            int size = edits.size();
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a1f5a8b5-48d4-4fd5-9068-58d6fcc07d58");
            try {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "553250cb-24d2-4a37-b3e9-e7940a7b8448");
                for (; i < size; i++) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "603d6e7c-e39e-4f62-aa67-993b33bbf356");
                    // may throw CannotRedoException
                    edits.get(i).redo();
                }
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "31db8d11-fa2b-489d-b0bb-a8c937bed738");
                hasBeenDone = true;
            } finally {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "90c4a6b1-4376-4e43-aaa9-00240d94bf67");
                if (i != size) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "142d7e4d-014b-43ff-b99e-d5807c9cbe53");
                    // i-th edit's redo failed => undo the ones below
                    while (--i >= 0) {
                        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5b5c2eb1-1be1-48a2-880f-c425c7018512");
                        edits.get(i).undo();
                    }
                }
            }
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4eb9939f-18d8-4beb-8be2-c80d1e0a861f");
        cs.fireChange();
    }

    @Override
    protected void redoTo(UndoableEdit edit) throws CannotRedoException {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "75eaa6ee-dae0-477a-8a4f-0ee920668fe5");
        int i = indexOfNextAdd;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "70852f82-2251-4a6a-a0ad-8e3964edce80");
        boolean done = false;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e53531f9-37ce-4c9c-bfbd-ac678ef99cd5");
        try {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "36de0083-b11e-4a67-9860-d9d245acd065");
            while (!done) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bf2d81eb-1e82-4c44-b287-97cf9f5182d1");
                UndoableEdit next = edits.elementAt(i++);
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7f0598b3-3469-4556-8797-abdb8707ca0f");
                // may throw CannotRedoException
                next.redo();
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5a2b884f-497d-4624-9854-533b1a9084f9");
                done = next == edit;
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "826c1c7c-f77d-4a94-8839-a69e3fd44eb6");
            indexOfNextAdd = i;
        } finally {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5970bc34-9c97-4305-aa4b-bd7b6d931a6e");
            if (!done) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f8799b7c-c54c-4fc6-8264-4a01c78a9749");
                // (i-1)-th edit's redo failed => undo the ones below
                i -= 2;
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6e0f240a-e38e-4a81-81fb-18d2e37308f5");
                for (; i >= indexOfNextAdd; i--) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "319b89d3-a6a5-4d33-8ce8-61d014077ffd");
                    edits.get(i).undo();
                }
            }
        }
    }

    @Override
    public boolean canRedo() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f5887900-b01a-4354-9ac3-f6a63ba9e918");
        if (inProgress) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ef707c40-652f-4d63-b598-b70f09816d85");
            UndoableEdit edit = editToBeRedone();
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f5b549d8-0c4a-4eb5-9d5c-dcac655b03a9");
            return edit != null && edit.canRedo();
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0df501cb-2f3d-4c77-9e83-240c6ed5a125");
            return !isInProgress() && alive && !hasBeenDone;
        }
    }

    @Override
    public void undoOrRedo() throws CannotRedoException, CannotUndoException {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6fb61bfb-7ba7-4d16-ae4c-547e62fe1159");
        if (indexOfNextAdd == edits.size()) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "920b5eb3-d29a-481e-83f3-9d52e7054e69");
            undo();
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9dd4a7d5-f0c0-4d6b-afec-ffb7cb1e7ad1");
            redo();
        }
    }

    @Override
    public boolean canUndoOrRedo() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "be62487d-8c87-4dcb-aa47-95d3f57575e9");
        if (indexOfNextAdd == edits.size()) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "68f622c6-24ef-4e04-8c25-b982d4ec7914");
            return canUndo();
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "163af8c4-3fcd-4387-a807-04a44d11a528");
            return canRedo();
        }
    }

    @Override
    public int getLimit() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9e925baf-0959-4780-ae84-de3515534791");
        return limit;
    }

    @Override
    public void setLimit(int l) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b39f12c3-7fc2-4b79-899f-53f82a26f256");
        if (!inProgress)
            throw new RuntimeException("Attempt to call UndoManager.setLimit() after UndoManager.end() has been called");
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f4e09257-df46-40e7-9dba-b2e96244983c");
        limit = l;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "49b5c809-2631-4519-b6e8-d4e28c250837");
        trimForLimit();
    }

    @Override
    protected void trimForLimit() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "781ffcfd-757e-43c5-b6eb-1eb1b235cd36");
        if (limit >= 0) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e3e4e1ac-2e1e-4e72-83fe-2a35cd7a4e08");
            int size = edits.size();
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5bd2bf68-7d4c-4af7-9218-f6bfbdb65acc");
            if (size > limit) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c8a3ec1a-8d06-4d8d-9189-0173e0e3af5a");
                int halfLimit = limit / 2;
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "df002000-7512-4792-a510-dd97c9f5fc77");
                int keepFrom = indexOfNextAdd - 1 - halfLimit;
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f24887da-9fa1-4adb-95fc-488afc2bc9a0");
                int keepTo = indexOfNextAdd - 1 + halfLimit;
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ac5e53d1-e07e-43c2-b7c9-2024019743e9");
                if (keepTo - keepFrom + 1 > limit) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7fff0c9f-75fa-48c4-9511-cb7276d83563");
                    keepFrom++;
                }
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1cd2bdb7-cc2d-4f8b-ac56-6662b7c42265");
                if (keepFrom < 0) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f80d0076-43f9-4497-a552-aef415955ef9");
                    keepTo -= keepFrom;
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6d5a0ec7-da57-4d57-8ba8-929b232789e0");
                    keepFrom = 0;
                }
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6d40a891-d209-4a6b-ae7a-948c6b12e2dc");
                if (keepTo >= size) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "127fdedf-0edc-4a3a-aa24-ddcf7192f1ca");
                    int delta = size - keepTo - 1;
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "86a11678-ef19-4b34-8ca6-8e59659b05de");
                    keepTo += delta;
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a2822c54-f929-42a5-8278-1911e0be1b8b");
                    keepFrom += delta;
                }
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2e3e3e5a-0555-4461-867c-6f1e1e905fee");
                trimEdits(keepTo + 1, size - 1);
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6d8e11f4-9e44-4115-b913-9feff54c15f6");
                trimEdits(0, keepFrom - 1);
            }
        }
    }

    @Override
    protected void trimEdits(int from, int to) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d6d8ff39-6d2f-4408-9a9b-f5da4e0ce970");
        if (from <= to) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "757cbe80-470a-4568-8a63-5e2a91dcc921");
            for (int i = to; from <= i; i--) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "06b2b680-bd29-40b1-8540-903c49ec9699");
                UndoableEdit e = edits.elementAt(i);
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6e60975f-c9d5-4dde-9847-a2f51bdb6a4f");
                e.die();
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f61bf83b-cbe6-45d3-853e-8bdb1570ec0a");
                edits.removeElementAt(i);
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ff91c0f4-3de3-4143-ba20-ec1221143915");
            if (indexOfNextAdd > to) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e865ee16-37d8-4ba4-91a9-64f40ee9951f");
                indexOfNextAdd -= to - from + 1;
            } else if (indexOfNextAdd >= from) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2b29a76e-3691-4940-bd23-f55caa25fa0b");
                indexOfNextAdd = from;
            }
        }
    }

    @Override
    public void discardAllEdits() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "84cf1c90-6508-412e-8c4c-180e78837b5f");
        Enumeration cursor = edits.elements();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "08ca0595-5032-4a9b-9208-f40bc0598b19");
        while (cursor.hasMoreElements()) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "40a6aebe-5ef7-4805-bafd-5d01928c882c");
            UndoableEdit e = (UndoableEdit) cursor.nextElement();
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "90b32331-791a-46a3-b076-e63e633c9a5c");
            e.die();
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3e4d7ad7-7861-4856-a3d9-2d7e9d594e99");
        edits = new Vector<UndoableEdit>();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1e2eb84d-7e2a-4fc1-8844-c26384070830");
        indexOfNextAdd = 0;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "46a3d1ec-a42c-42b5-bf1d-1e5de8e72f66");
        cs.fireChange();
    }

    @Override
    protected UndoableEdit lastEdit() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "25e93cac-4b3c-482f-a94f-2f05e918e862");
        int count = edits.size();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "195b3982-bf59-428d-b8b9-8b78bb96fc7d");
        if (count > 0)
            return edits.elementAt(count - 1);
        else
            return null;
    }

    @Override
    protected UndoableEdit editToBeUndone() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "50e2419c-6093-4980-b8f4-211d9b04790f");
        int i = indexOfNextAdd;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "91e4278d-fb4e-41e0-a6f8-4eb2396f2725");
        while (i > 0) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "32560f93-bc76-4777-9fc6-3324ab92bf2c");
            UndoableEdit edit = edits.elementAt(--i);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "522f8480-2a08-47cb-8743-49092ea14bef");
            if (edit.isSignificant()) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "dcd13668-53cf-4e35-a099-b118c7f21604");
                return edit;
            }
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c860c6a0-b167-475c-994f-884a0740760c");
        return null;
    }

    @Override
    protected UndoableEdit editToBeRedone() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "287e222e-7ef5-4b46-b988-7de0213645dc");
        int count = edits.size();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6ce3ef47-a9aa-4216-8d35-fc1eba31aa17");
        int i = indexOfNextAdd;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "82a9d445-80c4-471b-9388-649a382ee52f");
        while (i < count) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "28e1ab22-461f-4ae4-88bf-cf041064671f");
            UndoableEdit edit = edits.elementAt(i++);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "089bad45-15da-48b7-9131-239988bb59ad");
            if (edit.isSignificant()) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4908bcbe-0180-4ce7-9ed8-852aa623ed8a");
                return edit;
            }
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fb0fa9ee-b4e5-4cea-bd4b-cc06d6eca7f7");
        return null;
    }

    /**
     * Consume an undoable edit.
     * Delegates to superclass and notifies listeners.
     * @param ue the edit
     */
    @Override
    public void undoableEditHappened(final UndoableEditEvent ue) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d3e491fd-eed5-4410-b36e-b4fc7be30732");
        addEdit(ue.getEdit());
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "05728948-ae2c-4bd2-8760-913c81823224");
        cs.fireChange();
    }

    @Override
    public boolean addEdit(UndoableEdit anEdit) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ef97295e-1d5a-4eef-ac6e-196620a84edd");
        boolean retVal;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "073a4314-29dc-4aff-a641-05dc3e24169c");
        // Trim from the indexOfNextAdd to the end, as we'll
        // never reach these edits once the new one is added.
        trimEdits(indexOfNextAdd, edits.size() - 1);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0fd3ea0d-c400-416f-a934-beda0592c18a");
        if (!inProgress) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e8e66b06-2fce-465d-bedd-dbc5154d0140");
            retVal = false;
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5ce42888-d9ce-421e-aac3-c3e859dde1fa");
            UndoableEdit last = lastEdit();
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fcf52954-a77b-49c1-89e4-6858ce61e918");
            if (last == null) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b2cd26c8-a614-4ae1-b532-e26a7175484d");
                edits.addElement(anEdit);
            } else if (!last.addEdit(anEdit)) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "12f40c10-d5a8-47d2-ae2e-97ccd46e0bb9");
                if (anEdit.replaceEdit(last)) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e280e395-dbd6-479e-90e9-138c89ad2175");
                    edits.removeElementAt(edits.size() - 1);
                }
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "817c44d8-f3d6-48c1-8b73-36afad374b10");
                edits.addElement(anEdit);
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d82e0cd0-06c4-4a50-b1e9-7a08a0c1f629");
            retVal = true;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b529b86b-ce1e-4001-9f7a-8c519e6109e2");
        // Maybe super added this edit, maybe it didn't (perhaps
        // an in progress compound edit took it instead. Or perhaps
        // this UndoManager is no longer in progress). So make sure
        // the indexOfNextAdd is pointed at the right place.
        indexOfNextAdd = edits.size();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "693f3b0f-361a-4df8-af09-6b9c9d11aaf0");
        // Enforce the limit
        trimForLimit();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "651d506c-0997-4253-a116-28b75de762fc");
        return retVal;
    }

    @Override
    public boolean replaceEdit(UndoableEdit anEdit) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "eda3b407-4026-42bb-abb1-0dbb57d1a27d");
        return false;
    }

    @Override
    public boolean isSignificant() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "34efc734-dd9f-4320-817a-e5b8c96b29ed");
        Enumeration cursor = edits.elements();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1da9fe1e-991d-4c63-82cf-d716e4fd6332");
        while (cursor.hasMoreElements()) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a940a70d-4dd7-4c6c-ac0b-b5879e19792c");
            if (((UndoableEdit) cursor.nextElement()).isSignificant()) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d03eb7a0-f9db-469d-866a-a68fa9c52c7d");
                return true;
            }
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "01022f14-0a76-4f8b-ae39-f0c5711bd85f");
        return false;
    }

    @Override
    public String getPresentationName() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "64e16a1d-567f-4d28-a68d-e7668462409b");
        UndoableEdit last = lastEdit();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "03f72bb6-ccf0-4507-b806-564b36bb860a");
        if (last != null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a680ccdb-4afb-43fe-b66e-652ef4e59294");
            return last.getPresentationName();
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "30373d6e-6382-4cab-942e-39b872171724");
            return "";
        }
    }

    @Override
    public String getUndoPresentationName() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "dfa1eb49-42cd-43c2-a3ac-135539072e80");
        // The following code does an original code of: return this.canUndo() ? super.getUndoPresentationName() : "";
        if (canUndo()) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e32e6f1a-3983-4783-ad54-b7332c18139a");
            // UndoManager.getUndoPresentationName() follows
            if (inProgress) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f72b954e-3023-4713-b042-ec24fd4a5331");
                if (canUndo()) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e96c2115-5682-4145-8600-c27c0e20a9ef");
                    return editToBeUndone().getUndoPresentationName();
                } else {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "924a58ed-8c62-45a2-aea2-0519e36423a0");
                    return UIManager.getString("AbstractUndoableEdit.undoText");
                }
            } else {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c59e1b60-8639-4904-bd63-922aa9d1d084");
                UndoableEdit last = lastEdit();
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "dedb2f3a-02f1-4f0d-93cc-61ee7ac28a73");
                if (last != null) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fdd522b7-5791-4858-b728-eb7d11818815");
                    return last.getUndoPresentationName();
                } else {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0dbd199d-080d-45e5-93c5-1bdcb248a0cd");
                    String name = getPresentationName();
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b7fb4151-4b1b-4625-87f5-3c356ba86b41");
                    if (!"".equals(name)) {
                        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "83e56b95-5ba7-4da5-9f8a-fe8cdf4cd219");
                        name = UIManager.getString("AbstractUndoableEdit.undoText") + " " + name;
                    } else {
                        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "82f20b9f-b7f3-4652-9d7a-534c7fd6dab0");
                        name = UIManager.getString("AbstractUndoableEdit.undoText");
                    }
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fe42845c-3649-4c67-8297-176ad69c5952");
                    return name;
                }
            }
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5bda0366-e03f-4ccd-95ae-5badc2ef69e0");
            return "";
        }
    }

    @Override
    public String getRedoPresentationName() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "46a5e324-2597-401c-afdf-cba41a50e264");
        // The following code does an original code of: return this.canRedo() ? super.getRedoPresentationName() : "";
        if (canRedo()) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d9ae1fc7-f9ee-4b21-adbb-fb6e4717a2a7");
            // UndoManager.getRedoPresentationName() follows
            UndoableEdit last = lastEdit();
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "051daf3d-a881-412f-90a6-1c271a3e6c62");
            if (last != null) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a38416f3-a2ba-4ba6-aabc-e936a00be8f0");
                if (inProgress) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "07f79acf-d86f-42d6-88bc-be9e06acd308");
                    if (canRedo()) {
                        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a85d8265-e972-460e-a3d6-171052225b82");
                        return editToBeRedone().getRedoPresentationName();
                    } else {
                        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e8f21a68-8fc9-43fc-b39f-138c69413ff3");
                        return UIManager.getString("AbstractUndoableEdit.redoText");
                    }
                } else {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "935507e6-ef78-4b71-8412-222cb7239546");
                    return super.getRedoPresentationName();
                }
            } else {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bb6fd97b-3326-419c-a19b-292100dbab34");
                String name = getPresentationName();
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f8ae4132-d0ca-4078-9b6d-1c5c65cb2e35");
                if (!"".equals(name)) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "40432877-a01e-488a-885e-26f3e1d38428");
                    name = UIManager.getString("AbstractUndoableEdit.redoText") + " " + name;
                } else {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7fa12d7b-2d39-4499-9aee-a256bed0dc7e");
                    name = UIManager.getString("AbstractUndoableEdit.redoText");
                }
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "56972efa-2e2d-4ae4-b51a-f9121c3b6473");
                return name;
            }
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "01bfd529-6b10-4539-b7ba-60941b891019");
            return "";
        }
    }

    @Override
    public String getUndoOrRedoPresentationName() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "713e71d4-4e75-4490-926f-7626eff4769f");
        if (indexOfNextAdd == edits.size()) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2b2e8fe7-bd05-4d02-af19-7afd38896607");
            return getUndoPresentationName();
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8df804e3-4403-4c9c-8c84-770c794f9555");
            return getRedoPresentationName();
        }
    }

    @Override
    public String toString() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f5ced21a-13b6-4076-bf98-9103d9dc1e05");
        return super.toString() + " hasBeenDone: " + hasBeenDone + " alive: " + alive + " inProgress: " + inProgress + " edits: " + edits + " limit: " + limit + " indexOfNextAdd: " + indexOfNextAdd;
    }

    @Override
    public void addChangeListener(ChangeListener l) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a46b21bc-91f2-4446-8135-cde1ad3fa8cc");
        cs.addChangeListener(l);
    }

    /* Removes the listener
    */
    @Override
    public void removeChangeListener(ChangeListener l) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4e949f1b-5504-4ac4-a8be-a448d849607e");
        cs.removeChangeListener(l);
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
