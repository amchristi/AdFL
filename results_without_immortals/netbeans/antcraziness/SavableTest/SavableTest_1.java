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

    public void testSavablesAreRegistered() throws IOException {
        String id = "identity";
        DoSave savable = new DoSave(id, null, null);
        assertNotNull("Savable created", savable);
        assertTrue("Is is among the list of savables that need save", Savable.REGISTRY.lookupAll(Savable.class).contains(savable));
        savable.save();
        assertTrue("called", savable.save);
        assertTrue("No other pending saves", Savable.REGISTRY.lookupAll(Savable.class).isEmpty());
    }
}
