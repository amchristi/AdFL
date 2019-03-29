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

    public void testTwoSavablesForEqual() throws IOException {
        Object id = new Object();
        DoSave s = new DoSave(id, null, null);
        assertEquals("The first", s, Savable.REGISTRY.lookup(Savable.class));
        DoSave s2 = new DoSave(id, null, null);
        assertEquals("Only one savable", 1, Savable.REGISTRY.lookupAll(Savable.class).size());
        assertEquals("The later", s2, Savable.REGISTRY.lookup(Savable.class));
        s.save();
        assertFalse("Calling save on replaced savables has no effect", s.save);
    }
}
