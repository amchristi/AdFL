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
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4b712eee-d831-4c1c-a30b-a0f89915b2da");
        int size = edits.size();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4c87c47d-7858-4a6b-80b3-4686fbbafcab");
        for (int i = size - 1; i >= 0; i--) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8f696f92-d1d6-46ee-925e-6b54e22df00b");
            UndoableEdit e = edits.elementAt(i);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5f6e0e28-6e00-4cf3-8523-8eae5d18235e");
            e.die();
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5c99c829-8709-43be-86d6-cf2d7008c8ce");
        alive = false;
    }

    @Override
    public boolean isInProgress() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9212df1f-8cd2-4d68-b0d1-a43a16e660e2");
        return inProgress;
    }

    @Override
    public void end() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "023e0e38-ef91-4692-8a09-8beace4d03b9");
        inProgress = false;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "34e7f4ad-5732-46db-aa2c-fbb9ce6b7dad");
        this.trimEdits(indexOfNextAdd, edits.size() - 1);
    }

    @Override
    public void undo() throws CannotUndoException {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "dcfd04f9-e080-4ed6-9e6d-834f6d01eae1");
        if (inProgress) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "15d80b4b-1a86-4742-b9ff-ffd1e815f09c");
            UndoableEdit edit = editToBeUndone();
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "20bd2080-9f3e-4444-b424-99de9729b7a5");
            if (edit == null) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5e81bb62-61c2-4ee6-afbb-eff64e4f93d6");
                throw new CannotUndoException();
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fcb235b5-48a4-429c-9bc1-8475f49ae214");
            undoTo(edit);
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "916af8fd-a4eb-43c0-9a95-333c52020fc5");
            if (!canUndo()) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "dc41025d-cf0c-44f2-9d88-f6f6c9fb63c6");
                throw new CannotUndoException();
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "83844693-a4f4-4090-a849-77e3e9302bb9");
            int i = edits.size() - 1;
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e45caa65-4199-43ee-b434-ab48522bddf9");
            try {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d189fe9d-33cd-4ca0-b86b-47010d63cfcf");
                for (; i >= 0; i--) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "dd61e536-afda-4aa4-8be9-10e8f96b91e9");
                    // may throw CannotUndoException
                    edits.get(i).undo();
                }
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3819e314-ad38-46c1-922d-7daacdcd8569");
                hasBeenDone = false;
            } finally {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f59e7fd2-4312-4c10-acd0-7f7df627c1e1");
                if (i != -1) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9cce1177-e427-4cd3-b910-8d0bc5c32b96");
                    // i-th edit's undo failed => redo the ones above
                    int size = edits.size();
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "af8b6cb0-8580-4d3b-a72b-ff2e6038ad47");
                    while (++i < size) {
                        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3fe54eaa-d0f3-4851-820c-91e03dbf3807");
                        edits.get(i).redo();
                    }
                }
            }
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "853e815b-b227-4917-800b-7e41612d2502");
        cs.fireChange();
    }

    @Override
    protected void undoTo(UndoableEdit edit) throws CannotUndoException {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7c3fa597-36dd-488d-b438-98a3da33e062");
        int i = indexOfNextAdd;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7f8027eb-3358-48d7-bcfa-4ea456456337");
        boolean done = false;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "33e70554-8960-45a9-b616-15073f5b0d56");
        try {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bc90239f-3f80-4327-a842-fc93426c9198");
            while (!done) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1161b07d-cf2b-4299-bf76-63efd63a16a0");
                UndoableEdit next = edits.get(--i);
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e05c8267-6910-4013-a2e8-4974609d8f28");
                // may throw CannotUndoException
                next.undo();
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fb27b28d-942e-4fd9-bcdd-e44b8872bf32");
                done = next == edit;
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bc30292e-462a-41a4-897b-e3b05d06759b");
            indexOfNextAdd = i;
        } finally {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9637ddc9-aab3-49b1-9d08-0160d7a05891");
            if (!done) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "675db356-c430-4915-9bce-f251de488fc6");
                // i-th edit's undo failed => redo the ones above
                i++;
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c982a668-72e1-4d35-8a98-a816d4dfad2b");
                for (; i < indexOfNextAdd; i++) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "465d43b8-abb9-42a5-bf6a-2c6b45c3a312");
                    edits.get(i).redo();
                }
            }
        }
    }

    @Override
    public boolean canUndo() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bb9e0239-8033-4bed-9a85-3275108ebfb3");
        if (inProgress) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "994e281c-e13b-4460-8148-0d6adc7898e7");
            UndoableEdit edit = editToBeUndone();
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "28551365-4cfc-47ee-9ad9-a6829f6c391b");
            return edit != null && edit.canUndo();
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8a5e254e-e974-44e7-af2e-7d394db51789");
            return !isInProgress() && alive && hasBeenDone;
        }
    }

    @Override
    public void redo() throws CannotRedoException {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6c630f10-90f7-4a17-9747-53ded2f833a2");
        if (inProgress) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "03bf25c7-0520-4b48-aac2-79c606b85729");
            UndoableEdit edit = editToBeRedone();
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "17896490-c7d2-4720-bb68-f3b3a6d2f997");
            if (edit == null) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8c1e4675-1bfb-4394-9771-4b54e10df104");
                throw new CannotRedoException();
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1a1e9c95-c845-48fe-8721-569e8272395b");
            redoTo(edit);
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8df6e691-03e5-4dca-9c66-7daf4c2f00d5");
            if (!canRedo()) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2f465822-9298-429d-9b27-662c81115412");
                throw new CannotRedoException();
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b409dc94-0e0b-4f6c-84c8-7fb398385ca1");
            int i = 0;
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f69748e5-ee91-4d0e-a3f3-7cdbec9ec67a");
            int size = edits.size();
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7e2a3fef-2b77-4bf6-a26e-24590fd7713f");
            try {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "96b19c0f-8a0e-446a-ba2b-629ba4a5fa88");
                for (; i < size; i++) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "04f62e6c-e940-4acc-8901-ae5bc3478cd1");
                    // may throw CannotRedoException
                    edits.get(i).redo();
                }
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e7612221-54e7-4143-a9a3-f763af6fa80e");
                hasBeenDone = true;
            } finally {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "104db1ae-4aca-4e2e-b609-84031adf6dbd");
                if (i != size) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c1f552b3-7c9e-4b2e-b06b-631efccd023a");
                    // i-th edit's redo failed => undo the ones below
                    while (--i >= 0) {
                        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9216fd1d-3dad-43c8-b946-a10f598c946c");
                        edits.get(i).undo();
                    }
                }
            }
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "02b55c0b-8a8f-4cc1-8f9c-5315c911f464");
        cs.fireChange();
    }

    @Override
    protected void redoTo(UndoableEdit edit) throws CannotRedoException {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a59040b2-1dcd-494a-878b-9bed2a2ca38e");
        int i = indexOfNextAdd;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "73f39a71-b675-4586-a99f-e5f290a0a2ca");
        boolean done = false;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "80c7d727-62de-48f4-8845-4dcc11c6a7c6");
        try {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "77651d57-8525-4400-9bfd-2fba58159d36");
            while (!done) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "566c5173-61a4-4036-8fa2-e9d6e4ad60e7");
                UndoableEdit next = edits.elementAt(i++);
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "555702f4-9efe-496d-ab3c-01dfad152ab5");
                // may throw CannotRedoException
                next.redo();
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "24b79269-471a-4565-9cf4-370a68d515b1");
                done = next == edit;
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e034f10c-c10d-48a5-9f9f-fe936ff380e4");
            indexOfNextAdd = i;
        } finally {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "297b515d-8254-42ee-b6ad-a6159b973e1f");
            if (!done) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "086f91fd-83b4-491a-a245-97228d90365d");
                // (i-1)-th edit's redo failed => undo the ones below
                i -= 2;
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "79f82dc6-28b4-449e-a49d-0f1d30939cc5");
                for (; i >= indexOfNextAdd; i--) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c27b1d12-1251-4f6c-a001-d18ea05ee313");
                    edits.get(i).undo();
                }
            }
        }
    }

    @Override
    public boolean canRedo() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0b9dc334-af3d-4632-b3b1-43cc25618682");
        if (inProgress) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "298a90b4-7cf1-4983-b408-d67784eec7fd");
            UndoableEdit edit = editToBeRedone();
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d885bed3-30aa-4797-aae2-d5cb55200153");
            return edit != null && edit.canRedo();
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fd4f4f57-9ca2-4d61-b0da-1e5605dccbc9");
            return !isInProgress() && alive && !hasBeenDone;
        }
    }

    @Override
    public void undoOrRedo() throws CannotRedoException, CannotUndoException {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8718f34f-95c6-4000-ae63-a9ede412fd92");
        if (indexOfNextAdd == edits.size()) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "52ab2179-384a-4e30-84f5-8db06e48b5b0");
            undo();
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b28d7ab3-8596-400d-b54c-7dfe9c4ffe34");
            redo();
        }
    }

    @Override
    public boolean canUndoOrRedo() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ed290c13-ef99-4250-9f24-a85f9b7ec3a9");
        if (indexOfNextAdd == edits.size()) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "617b16e2-01ca-49fe-86ce-d08766f7816d");
            return canUndo();
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3440be84-4b91-493d-83a3-b5c5af241a57");
            return canRedo();
        }
    }

    @Override
    public int getLimit() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7ff24b58-3a5d-42fa-b790-4aac1e567533");
        return limit;
    }

    @Override
    public void setLimit(int l) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "649822fc-7e4d-456f-90fa-99c1a04fe0d6");
        if (!inProgress)
            throw new RuntimeException("Attempt to call UndoManager.setLimit() after UndoManager.end() has been called");
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4a420e38-f569-45e7-a49d-25c1318bd931");
        limit = l;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8a4cea01-e48a-424a-9a90-0c27474de7fe");
        trimForLimit();
    }

    @Override
    protected void trimForLimit() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c5ae6265-bcc9-4f13-831e-9378591d3b41");
        if (limit >= 0) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f5779d4a-160c-4ff9-9170-298020ac3312");
            int size = edits.size();
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a4fc3b9d-15f7-47c8-86ea-d934d9e5e2e7");
            if (size > limit) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5ef29a4d-639a-4dcb-a0d8-43d1dd4f67a8");
                int halfLimit = limit / 2;
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f1592eb2-c8cf-4929-af48-15155bea0c2d");
                int keepFrom = indexOfNextAdd - 1 - halfLimit;
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8fbf40ad-bb85-44be-bc6a-7d8d67a8627b");
                int keepTo = indexOfNextAdd - 1 + halfLimit;
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5d623e8c-5924-4a68-8e16-19e3769034fb");
                if (keepTo - keepFrom + 1 > limit) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4314cea1-109e-465f-a760-c4c96118b8ea");
                    keepFrom++;
                }
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "106119f4-e243-4470-8967-5f84faaeac04");
                if (keepFrom < 0) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3b21baff-8527-4f0c-9de9-650b140e3925");
                    keepTo -= keepFrom;
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3b60683d-48cb-4dce-953a-b26a7936be8a");
                    keepFrom = 0;
                }
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "29441aa0-254c-4927-b0df-37734fc0c000");
                if (keepTo >= size) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6b003bc0-5a69-4501-9ee7-d07533a7709a");
                    int delta = size - keepTo - 1;
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "eb7d2277-e808-4b9f-97f5-b641ee1da5a9");
                    keepTo += delta;
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b34c326a-9add-4d5b-b427-93819ea5b3f2");
                    keepFrom += delta;
                }
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ea673206-72c1-44f8-ada9-c6aff20cce04");
                trimEdits(keepTo + 1, size - 1);
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ec3c3f1d-5d42-4360-8028-fe7c5de949d5");
                trimEdits(0, keepFrom - 1);
            }
        }
    }

    @Override
    protected void trimEdits(int from, int to) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "04268256-d702-4d86-aab9-c71b1d79d028");
        if (from <= to) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "527d91a9-0f88-41fa-a1f9-5f0cb1c82b0c");
            for (int i = to; from <= i; i--) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4bc693cc-ece6-442e-9207-3a2eb6fbd03b");
                UndoableEdit e = edits.elementAt(i);
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f0d589e8-7e28-45eb-ba60-ceb352f0c269");
                e.die();
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e83386cf-bd11-49cb-9286-45e9e6fec3b2");
                edits.removeElementAt(i);
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "438e7842-d13f-4efc-be17-63e4c028672e");
            if (indexOfNextAdd > to) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e8006d65-b427-42c1-8df7-66da40c99b4f");
                indexOfNextAdd -= to - from + 1;
            } else if (indexOfNextAdd >= from) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c09c1d47-e374-42a3-aede-cf5f695c445b");
                indexOfNextAdd = from;
            }
        }
    }

    @Override
    public void discardAllEdits() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "79f03dc8-fbb7-4f55-b678-c8139515382f");
        Enumeration cursor = edits.elements();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f3097bee-9c8d-40be-ae01-deb16d93fee1");
        while (cursor.hasMoreElements()) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6581557f-bfb2-4dec-8d23-83e4fa68d89c");
            UndoableEdit e = (UndoableEdit) cursor.nextElement();
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4943bd46-dd69-46bc-9d8b-7341c814fb41");
            e.die();
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "027fd886-5d11-4493-8ace-c800c22fa60f");
        edits = new Vector<UndoableEdit>();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "219e51e0-76d0-48b6-878e-320a0c6b0f30");
        indexOfNextAdd = 0;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2b053424-3aa9-46a5-bb0d-2e39e5449845");
        cs.fireChange();
    }

    @Override
    protected UndoableEdit lastEdit() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "af444a5f-1c34-4ae8-9e3e-0fb0c906448b");
        int count = edits.size();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "18eeb55e-87f0-4fe6-8390-549956ea99f1");
        if (count > 0)
            return edits.elementAt(count - 1);
        else
            return null;
    }

    @Override
    protected UndoableEdit editToBeUndone() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "93cbdabe-be36-4606-b32e-b2a974331e78");
        int i = indexOfNextAdd;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c406ee57-2d02-499b-afb1-52cee18237a4");
        while (i > 0) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a7213b06-7112-413c-a69c-adb65534733e");
            UndoableEdit edit = edits.elementAt(--i);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "123a9977-89fb-4eae-96a6-b728aaec1d47");
            if (edit.isSignificant()) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "21e10ee5-b96b-48c9-9317-7438ded0f90a");
                return edit;
            }
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "81f17a51-ad49-464d-a199-ff99ed084e56");
        return null;
    }

    @Override
    protected UndoableEdit editToBeRedone() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "51b5b278-48e9-4fd7-99f6-7e2329e8dcae");
        int count = edits.size();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a1d75a41-fcef-40f1-8f16-b05f03f14488");
        int i = indexOfNextAdd;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fda501ca-febf-4559-85ef-e6fd6ff848c7");
        while (i < count) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "861299d4-bdef-4739-8012-4185784f4a02");
            UndoableEdit edit = edits.elementAt(i++);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f50e991c-c8c2-451d-8c3f-732525dc087b");
            if (edit.isSignificant()) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b8eaa122-2577-433f-8cd0-ac7d316dbc4b");
                return edit;
            }
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b15d19c3-6219-4eeb-8267-46716dd4c2fd");
        return null;
    }

    /**
     * Consume an undoable edit.
     * Delegates to superclass and notifies listeners.
     * @param ue the edit
     */
    @Override
    public void undoableEditHappened(final UndoableEditEvent ue) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ce7e8005-0904-4ea8-bf99-63087cfd72c0");
        addEdit(ue.getEdit());
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1f239a72-be18-42cb-a0bf-11fefecdc8ee");
        cs.fireChange();
    }

    @Override
    public boolean addEdit(UndoableEdit anEdit) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "24f05760-f0aa-4793-967e-b4896b520319");
        boolean retVal;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7c3acb7a-2fea-4df7-a9d9-6a99aa99581a");
        // Trim from the indexOfNextAdd to the end, as we'll
        // never reach these edits once the new one is added.
        trimEdits(indexOfNextAdd, edits.size() - 1);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f24c7973-0c86-491d-9d53-54e00f976586");
        if (!inProgress) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8baffcb9-c0a6-42cc-8ff5-dd8a0da43a18");
            retVal = false;
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "50ec8abb-7afb-4fa4-8a51-fb54054c7a20");
            UndoableEdit last = lastEdit();
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d32e1492-280d-476c-9aba-ae8b89b1b877");
            if (last == null) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c2f829eb-c379-4627-9b9b-834b4b9a4ccf");
                edits.addElement(anEdit);
            } else if (!last.addEdit(anEdit)) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cdd47c08-7b65-4a01-9715-0d1e7fcff0f1");
                if (anEdit.replaceEdit(last)) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "733d2376-3303-4ba2-8889-1b50819b5bb7");
                    edits.removeElementAt(edits.size() - 1);
                }
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "92d36048-f812-4bea-8647-66c4e0351544");
                edits.addElement(anEdit);
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "59756e6a-c5fb-496a-8234-3069e8fad421");
            retVal = true;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1b2dd98d-947e-4883-afb5-6a0d38079bbc");
        // Maybe super added this edit, maybe it didn't (perhaps
        // an in progress compound edit took it instead. Or perhaps
        // this UndoManager is no longer in progress). So make sure
        // the indexOfNextAdd is pointed at the right place.
        indexOfNextAdd = edits.size();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "de87dc5d-78a8-4750-9d8c-e13cc4f4e5e6");
        // Enforce the limit
        trimForLimit();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5ceb5d8c-af42-4a7d-b946-0e7f47fa18d6");
        return retVal;
    }

    @Override
    public boolean replaceEdit(UndoableEdit anEdit) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "29beed9c-28e0-45a1-b0d5-e7f1ef4bc251");
        return false;
    }

    @Override
    public boolean isSignificant() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "54df315d-8b5d-4ba4-96f4-aad6ffd04321");
        Enumeration cursor = edits.elements();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ff475f82-e607-4b37-bead-8c577750f4a6");
        while (cursor.hasMoreElements()) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "96912c63-5d1f-4c4a-9e22-974a6c0412a5");
            if (((UndoableEdit) cursor.nextElement()).isSignificant()) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8476c4cf-2b1e-4f45-ab04-d21eb3ab1de6");
                return true;
            }
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f7658f63-fb5b-48fd-9766-c64a46fb494e");
        return false;
    }

    @Override
    public String getPresentationName() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "072a161f-7a30-4ac3-a333-b22f0cb8db84");
        UndoableEdit last = lastEdit();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2bc908c9-eb6d-4400-907c-0563fc5e4069");
        if (last != null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "415cb947-b4cd-47f6-912a-bb05d6e2add9");
            return last.getPresentationName();
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c9ba6e24-1f21-42de-9696-baaf221116f9");
            return "";
        }
    }

    @Override
    public String getUndoPresentationName() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "adbde5c1-f082-4ac6-9424-ebc8d11de3ef");
        // The following code does an original code of: return this.canUndo() ? super.getUndoPresentationName() : "";
        if (canUndo()) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6d78858c-7421-4947-9a29-d84067a98c4c");
            // UndoManager.getUndoPresentationName() follows
            if (inProgress) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "868b21b4-8d15-41b9-80ab-e1bdfbe77bab");
                if (canUndo()) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "312365bc-1256-4855-a632-c1869ba36b10");
                    return editToBeUndone().getUndoPresentationName();
                } else {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7bdd59c2-2fad-47cf-8c18-e67c57df02ca");
                    return UIManager.getString("AbstractUndoableEdit.undoText");
                }
            } else {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c17b4b3d-c583-490e-b49a-85873711adad");
                UndoableEdit last = lastEdit();
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5c0f78c2-247f-43fd-b265-03f634e4bf02");
                if (last != null) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7cabe547-2d50-4f58-8496-7e3d348277dd");
                    return last.getUndoPresentationName();
                } else {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1eab47b2-e054-4efd-ad5f-e39e78c74bd2");
                    String name = getPresentationName();
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "65e2a668-ed7c-48f8-9c3a-37551d3dde09");
                    if (!"".equals(name)) {
                        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3902190c-0778-4f03-ba79-32771d2fcdb8");
                        name = UIManager.getString("AbstractUndoableEdit.undoText") + " " + name;
                    } else {
                        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "006cc744-9d71-4280-8d58-92ca426a6857");
                        name = UIManager.getString("AbstractUndoableEdit.undoText");
                    }
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7ff1d95b-b63a-469c-8219-a84fe9890da9");
                    return name;
                }
            }
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5a1228fd-fa09-434f-8747-16c3753eb701");
            return "";
        }
    }

    @Override
    public String getRedoPresentationName() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "10dd5044-ab01-4a48-8766-33a8ffceb4f2");
        // The following code does an original code of: return this.canRedo() ? super.getRedoPresentationName() : "";
        if (canRedo()) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5013d426-08e4-4076-a98d-bed5fd02e810");
            // UndoManager.getRedoPresentationName() follows
            UndoableEdit last = lastEdit();
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b933d0fc-3bcf-4d97-b3d1-01b568082230");
            if (last != null) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4f4b9614-b19f-420f-940a-9aecbbd632f0");
                if (inProgress) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8edc47ed-ab77-4bc8-9e0c-64de9111ef13");
                    if (canRedo()) {
                        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "23cdcce8-313b-4321-9138-ff45312e1600");
                        return editToBeRedone().getRedoPresentationName();
                    } else {
                        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "381a0c45-9bdc-4639-a737-1a7e79c80d00");
                        return UIManager.getString("AbstractUndoableEdit.redoText");
                    }
                } else {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e183942c-4ff7-495b-b8f9-4eb3c0b47dbd");
                    return super.getRedoPresentationName();
                }
            } else {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d6fba553-1939-47b8-bf92-5c482b6bb4a7");
                String name = getPresentationName();
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cb600d50-cc90-424c-85f8-5660156ee941");
                if (!"".equals(name)) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "373eb874-42e4-4d95-b7f6-9ee8d96e1331");
                    name = UIManager.getString("AbstractUndoableEdit.redoText") + " " + name;
                } else {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a9740a7e-6e48-4ee4-a0f1-4959468c6a4b");
                    name = UIManager.getString("AbstractUndoableEdit.redoText");
                }
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bd19b35f-2230-47a2-ad94-8813c146e9da");
                return name;
            }
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d3ee8601-0cb4-4dec-828b-16a52eb30953");
            return "";
        }
    }

    @Override
    public String getUndoOrRedoPresentationName() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "75fc78cd-d975-4272-9b90-9f828d6a08bd");
        if (indexOfNextAdd == edits.size()) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8aaa1921-7e49-4656-9c05-fae8e9367181");
            return getUndoPresentationName();
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "90022ee7-a143-45eb-ae9e-5c13c962eb76");
            return getRedoPresentationName();
        }
    }

    @Override
    public String toString() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e3b7bb73-4d6d-400c-af8c-1df2707b3d3e");
        return super.toString() + " hasBeenDone: " + hasBeenDone + " alive: " + alive + " inProgress: " + inProgress + " edits: " + edits + " limit: " + limit + " indexOfNextAdd: " + indexOfNextAdd;
    }

    @Override
    public void addChangeListener(ChangeListener l) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "60cb3cca-0fa5-45ee-8888-cf8e32024586");
        cs.addChangeListener(l);
    }

    /* Removes the listener
        */
    @Override
    public void removeChangeListener(ChangeListener l) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1a84aa27-90a2-43e9-8e79-a10bee3fa28c");
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
