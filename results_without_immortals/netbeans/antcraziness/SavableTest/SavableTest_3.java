package org.netbeans.api.actions;

import java.io.IOException;
import org.netbeans.junit.NbTestCase;
import org.netbeans.spi.actions.AbstractSavable;
import org.openide.util.Lookup.Result;
import org.openide.util.LookupEvent;
import org.openide.util.LookupListener;

/**
 * @author Jaroslav Tulach <jtulach@netbeans.org>
 */
public class SavableTest extends NbTestCase {

    public SavableTest(String n) {
        super(n);
    }

    static class DoSave extends AbstractSavable {

        boolean save;

        private final Object id;

        private final CharSequence displayName, ch2;

        public DoSave(Object id, CharSequence displayName, CharSequence ch2) {
            this.id = id;
            this.displayName = displayName;
            this.ch2 = ch2;
            register();
        }

        @Override
        public String findDisplayName() {
            return displayName.toString();
        }

        @Override
        protected void handleSave() throws IOException {
            save = true;
        }

        @Override
        public boolean equals(Object obj) {
            if (obj instanceof DoSave) {
                return ((DoSave) obj).id.equals(id);
            }
            return false;
        }

        @Override
        public int hashCode() {
            return id.hashCode();
        }

        final void cleanup() {
            unregister();
        }
    }

    @Override
    protected void tearDown() throws Exception {
        for (DoSave savable : Savable.REGISTRY.lookupAll(DoSave.class)) {
            savable.cleanup();
        }
    }

    public void testEventDeliveredAsynchronously() throws Exception {
        class L implements LookupListener {

            int change;

            Object id = new Object();

            @Override
            public synchronized void resultChanged(LookupEvent ev) {
                change++;
                notifyAll();
            }

            public synchronized void createSavable() {
                assertEquals("No changes yet", 0, change);
                Savable s = new DoSave(id, null, null);
                assertEquals("The first", s, Savable.REGISTRY.lookup(Savable.class));
                assertEquals("Still no changes", 0, change);
            }

            public synchronized void waitForChange() throws InterruptedException {
                while (change == 0) {
                    wait();
                }
                assertEquals("One change delivered", 1, change);
            }
        }
        L listener = new L();
        Result<Savable> res = Savable.REGISTRY.lookupResult(Savable.class);
        try {
            res.addLookupListener(listener);
            listener.createSavable();
            listener.waitForChange();
        } finally {
            res.removeLookupListener(listener);
        }
    }
}
