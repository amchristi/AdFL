package net.sourceforge.cruisecontrol.distributed;

import junit.framework.TestCase;
import junit.framework.Test;
import junit.framework.TestSuite;
import org.apache.log4j.Logger;
import net.sourceforge.cruisecontrol.builders.DistributedMasterBuilderTest;
import net.sourceforge.cruisecontrol.distributed.core.MulticastDiscovery;
import net.jini.core.lookup.ServiceRegistrar;
import net.jini.core.lookup.ServiceTemplate;
import net.jini.core.entry.Entry;
import net.jini.lookup.ServiceIDListener;

import java.rmi.RemoteException;

import java.awt.GraphicsEnvironment;
import java.util.prefs.Preferences;

/**
 * @author Dan Rollo
 * Date: Jul 6, 2005
 * Time: 4:12:20 PM
 */
public class BuildAgentTest extends TestCase {

    private static final Logger LOG = Logger.getLogger(BuildAgentTest.class);

    private static final PropertyEntry[] EMPTY_PROP_ENTRIES = new PropertyEntry[]{};


    /**
     * Use LUSTestSetup decorator to run Jini LUS once for this test class.
     * @return  a TestSuite wrapper by the LUSTestSetup decorator
     */
    public static Test suite() {
        final TestSuite ts = new TestSuite();
        ts.addTestSuite(BuildAgentTest.class);
        return new DistributedMasterBuilderTest.LUSTestSetup(ts);
    }
    // @todo Remove one slash in front of "/*" below to run individual tests in an IDE
    //*
    protected void setUp() throws Exception {
        BuildAgentTest.setSkipMainSystemExit();
        BuildAgentTest.setTerminateFast();
    }
    protected void tearDown() throws Exception {
        BuildAgent.kill();
    }
    //*/

    // @todo Add one slash in front of "/*" below to run individual tests in an IDE
    /*
    private static DistributedMasterBuilderTest.ProcessInfoPump jiniProcessPump;
    protected void setUp() throws Exception {
        jiniProcessPump = DistributedMasterBuilderTest.startJini();
        BuildAgent.setSkipMainSystemExit();
        BuildAgentTest.setTerminateFast();
    }
    protected void tearDown() throws Exception {
        DistributedMasterBuilderTest.killJini(jiniProcessPump);
        BuildAgent.kill();
    }
    //*/



    private static void assertFindAgent(final ServiceRegistrar reg,
                                    final int retries, final boolean expectedFoundResult)
            throws RemoteException, InterruptedException {

        final Entry[] entries = SearchablePropertyEntries.getPropertiesAsEntryArray(
                new SearchablePropertyEntries(
                        BuildAgentServiceImplTest.TEST_USER_DEFINED_PROPERTIES_FILE).getProperties()
        );
        findAgent(reg, retries, expectedFoundResult, entries);
    }

    private static Object findAgent(final ServiceRegistrar reg,
                                    final int retries, final boolean expectedFoundResult, final Entry[] entries)
            throws RemoteException, InterruptedException {

        int retryCount = 0;
        Object result;
        boolean isFound;
        do {
            if (retryCount > 0) {
                LOG.info("\tFind agent unit test retry " + retryCount + "...Entries: "
                        + MulticastDiscovery.toStringEntries(entries));
                Thread.sleep(1000 + (250 * retryCount)); // wait a bit before retry, longer for subsequent retries
            }

            result = reg.lookup(new ServiceTemplate(null,
                new Class[]{BuildAgentService.class},
                entries));

            isFound = (result != null);

            retryCount++;

        } while ((expectedFoundResult != isFound) && (retryCount < retries));

        if (expectedFoundResult) {
            assertNotNull("Should find agent", result);
        } else {
            assertNull("Should not find agent", result);
        }

        return result;
    }

    public static void setSkipMainSystemExit() { BuildAgent.setSkipMainSystemExit(); }

    public static void setTerminateFast() {
        BuildAgent.setTerminateFast();
    }


    public static BuildAgent createTestBuildAgent(final String propsFile, final String userDefinedPropertiesFilename,
                                                  final boolean isSkipUI,
                                                  final ServiceIDListener testListener, final int testAgentID) {

        return new BuildAgent(propsFile, userDefinedPropertiesFilename, isSkipUI, testListener, testAgentID);
    }

    public static void terminateTestAgent(final BuildAgent agent) {
        agent.terminateTestAgent(agent);
    }

    /**
     * This test requires a bunch of manual steps:
     * 1. Build the cc-agent.war (created via: ant war-agent).
     * 2. Deploy cc-agent.war to a web server.
     * 3. Manually launch agent via webstat (http://localhost:8080/cc-agent/agent.jnlp).
     * 4. Manually run this test.
     * @throws Exception if anything unexpected goes wrong in the test
     */
    public void manual_testRestart() throws Exception {
        final ServiceRegistrar reg = DistributedMasterBuilderTest.findTestLookupService(20 * 1000);
        assertNotNull("Couldn't find registrar.", reg);

        final Entry[] entries = SearchablePropertyEntries.getPropertiesAsEntryArray(
                new SearchablePropertyEntries(
                        BuildAgentServiceImplTest.TEST_USER_DEFINED_PROPERTIES_FILE).getProperties()
        );
        // work around timestamp prefix in build.type entry
        final int idxBuildTypeEntry = 3;
        assertEquals("Wrong entry in position where we expected to find 'build.type'.",
                BuildAgentServiceImplTest.ENTRY_NAME_BUILD_TYPE, ((PropertyEntry) entries[idxBuildTypeEntry]).name);
        entries[idxBuildTypeEntry] = new PropertyEntry(((PropertyEntry) entries[idxBuildTypeEntry]).name, "test");

        final BuildAgentService agentService = (BuildAgentService) findAgent(reg, 3, true, entries);
        assertNotNull(agentService.getMachineName());
        agentService.restart(false);
        // allow time for the relaunched agent to spin up and register
        Thread.sleep(20 * 1000);
        // verify first agent is dead
        try {
            agentService.getMachineName();
            fail("Agent should be dead");
        } catch (Exception e) {
            // good, this is what we want.
        }
        // find the newly relaunched agent
        final BuildAgentService agentService2 = (BuildAgentService) findAgent(reg, 3, true, entries);
        assertNotNull(agentService2.getMachineName());
        agentService2.kill(false);
    }

    public void testKillNoUI() throws Exception {
        ServiceRegistrar reg = DistributedMasterBuilderTest.findTestLookupService(20 * 1000);
        assertNotNull("Couldn't find registrar.", reg);
        assertFindAgent(reg, 3, false);

        final Thread t = new Thread("BuildAgentTest testKillNoUI Thread") {
            public void run() {
                BuildAgent.main(new String[] {
                    "-" + BuildAgent.MAIN_ARG_AGENT_PROPS, BuildAgentServiceImplTest.TEST_AGENT_PROPERTIES_FILE,
                    "-" + BuildAgent.MAIN_ARG_USER_PROPS, BuildAgentServiceImplTest.TEST_USER_DEFINED_PROPERTIES_FILE,
                    "-" + BuildAgent.MAIN_ARG_SKIP_UI
                });
            }
        };
        t.start();
        // allow BuildAgent main to load and register
        final int maxWaitStartup = 30;
        int count = 0;
        while (count < maxWaitStartup && t.isAlive() && BuildAgent.getMainThread() == null) {
            Thread.sleep(500);
            count++;
        }
        assertTrue("Agent start thread should be alive.", t.isAlive());
        assertNotNull("Agent didn't start before timeout.", BuildAgent.getMainThread());
        assertTrue("Agent didn't init before timeout.", BuildAgent.getMainThread().isAlive());
        assertFindAgent(reg, 10, true);
        final Thread mainThread = BuildAgent.getMainThread(); // hold onto main thread since kill nullifies it
        assertNotNull("Main thread should not be null.", mainThread);

        BuildAgent.kill();
        assertFalse("Agent start thread should be dead.", t.isAlive());
        assertFalse("Agent didn't die before timeout.", mainThread.isAlive()); // check held thread
        assertFindAgent(reg, 10, false);
    }

    public void testKill() throws Exception {
        if (GraphicsEnvironment.isHeadless()) {
            LOG.warn("WARNING: DETECTED HEADLESS ENVIRONMENT. Skipping test: " + getClass().getName() + ".testKill()");
            return;
        }

        ServiceRegistrar reg = DistributedMasterBuilderTest.findTestLookupService(20 * 1000);
        assertNotNull("Couldn't find registrar.", reg);
        assertFindAgent(reg, 3, false);

        final Thread t = new Thread("BuildAgentTest testKill Thread") {
            public void run() {
                BuildAgent.main(new String[] {
                    "-" + BuildAgent.MAIN_ARG_AGENT_PROPS, BuildAgentServiceImplTest.TEST_AGENT_PROPERTIES_FILE,
                    "-" + BuildAgent.MAIN_ARG_USER_PROPS, BuildAgentServiceImplTest.TEST_USER_DEFINED_PROPERTIES_FILE
                });
            }
        };
        t.start();
        // allow BuildAgent main to load and register
        final int maxWaitStartup = 30;
        int count = 0;
        while (count < maxWaitStartup && t.isAlive() && BuildAgent.getMainThread() == null) {
            Thread.sleep(500);
            count++;
        }
        assertTrue("Agent start thread should be alive.", t.isAlive());
        assertNotNull("Agent didn't start before timeout.", BuildAgent.getMainThread());
        assertTrue("Agent didn't init before timeout.", BuildAgent.getMainThread().isAlive());
        assertFindAgent(reg, 10, true);
        final Thread mainThread = BuildAgent.getMainThread(); // hold onto main thread since kill nullifies it
        assertNotNull("Main thread should not be null.", mainThread);

        BuildAgent.kill();
        assertFalse("Agent start thread should be dead.", t.isAlive());
        assertFalse("Agent didn't die before timeout.", mainThread.isAlive()); // check held thread
        assertFindAgent(reg, 20, false);
    }

    /**
     * Info holder class used to restore any existing agent entryOverrides.
     */
    static final class ClearEntryOverridesInfo {

        final Preferences parentNode;
        final String origPrefsRootName;
        final PropertyEntry[] origEntryOverrides;

        ClearEntryOverridesInfo() throws InterruptedException {
            final BuildAgent agentReadPrefs = DistributedMasterBuilderTest.createBuildAgent(false);
            parentNode = agentReadPrefs.getPrefsRoot().parent();
            origPrefsRootName = agentReadPrefs.getPrefsRoot().name();
            origEntryOverrides = agentReadPrefs.getEntryOverrides();
            agentReadPrefs.clearEntryOverrides();
            terminateTestAgent(agentReadPrefs);
        }

        void restoreOriginalEntryOverrides() {
            // restore entryOverrides, otherwise building CC clears JRE prefs storage on disk in build farm agents
            // need to recreate prefs sub-node deleted during cleanup above
            final Preferences prefsEntryOverrides
                    // recreate deleted agent node
                    = parentNode.node(origPrefsRootName)
                    // recreate deleted entry overrides node
                    .node(BuildAgent.PREFS_NODE_ENTRY_OVERRIDES);
            BuildAgent.putEntryOverrides(prefsEntryOverrides, origEntryOverrides);
        }
    }

    public void testSetEntryOverrides() throws Exception {

        // hold reference original info to restore any entryOverrides that existed before this test ran
        final ClearEntryOverridesInfo origEntryOverridesInfo = new ClearEntryOverridesInfo();

        final BuildAgent buildAgent = DistributedMasterBuilderTest.createBuildAgent(false);
        try {
            final int expectedOrigEntryCount = 5;
            final PropertyEntry[] origEntries = buildAgent.getEntries();
            assertEquals("Did the unit test props file change? : "
                    + BuildAgentServiceImplTest.TEST_USER_DEFINED_PROPERTIES_FILE,
                    expectedOrigEntryCount, origEntries.length);

            // add new prop entry
            final PropertyEntry newPropEntry = new PropertyEntry("newEntryName", "newEntryValue");
            buildAgent.setEntryOverrides(new PropertyEntry[] { newPropEntry });
            assertEquals(expectedOrigEntryCount + 1, buildAgent.getEntries().length);

            // set to empty array
            buildAgent.setEntryOverrides(EMPTY_PROP_ENTRIES);
            assertEquals(expectedOrigEntryCount, buildAgent.getEntries().length);

            // test clear overrides
            buildAgent.setEntryOverrides(new PropertyEntry[] { newPropEntry });
            assertEquals(expectedOrigEntryCount + 1, buildAgent.getEntries().length);
            buildAgent.clearEntryOverrides();
            assertEquals(expectedOrigEntryCount, buildAgent.getEntries().length);
            // test multiple clear calls
            buildAgent.clearEntryOverrides();
            assertEquals(expectedOrigEntryCount, buildAgent.getEntries().length);

            // test override user-defined entry and system entry
            final PropertyEntry newOverrideUser = new PropertyEntry("build.type", "OverrideTest");
            final PropertyEntry newOverrideSystem
                    = new PropertyEntry(SearchablePropertyEntries.SYSTEM_ENTRY_KEYS[0], "OverrideSystemEntry");
            buildAgent.setEntryOverrides(new PropertyEntry[] { newPropEntry, newOverrideUser, newOverrideSystem });
            assertEquals(expectedOrigEntryCount + 1, buildAgent.getEntries().length);

            buildAgent.clearEntryOverrides();
            assertEquals(expectedOrigEntryCount, buildAgent.getEntries().length);
        } finally {

            // clear overrides
            buildAgent.clearEntryOverrides();

            // clear all agent prefs
            buildAgent.getPrefsRoot().removeNode();
            buildAgent.getPrefsRoot().flush();

            // restore entryOverrides, otherwise building CC clears JRE prefs storage on disk in build farm agents
            origEntryOverridesInfo.restoreOriginalEntryOverrides();

            terminateTestAgent(buildAgent);
        }
    }
}
