/**
 * *****************************************************************************
 * CruiseControl, a Continuous Integration Toolkit
 * Copyright (c) 2001, ThoughtWorks, Inc.
 * 200 E. Randolph, 25th Floor
 * Chicago, IL 60601 USA
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * + Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * + Redistributions in binary form must reproduce the above
 * copyright notice, this list of conditions and the following
 * disclaimer in the documentation and/or other materials provided
 * with the distribution.
 *
 * + Neither the name of ThoughtWorks, Inc., CruiseControl, nor the
 * names of its contributors may be used to endorse or promote
 * products derived from this software without specific prior
 * written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * ******************************************************************************
 */
package net.sourceforge.cruisecontrol.builders;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import net.sourceforge.cruisecontrol.Builder;
import net.sourceforge.cruisecontrol.CruiseControlException;
import net.sourceforge.cruisecontrol.Progress;
import net.sourceforge.cruisecontrol.gendoc.annotations.Cardinality;
import net.sourceforge.cruisecontrol.gendoc.annotations.Default;
import net.sourceforge.cruisecontrol.gendoc.annotations.Description;
import net.sourceforge.cruisecontrol.gendoc.annotations.DescriptionFile;
import net.sourceforge.cruisecontrol.gendoc.annotations.ExamplesFile;
import net.sourceforge.cruisecontrol.gendoc.annotations.Optional;
import net.sourceforge.cruisecontrol.gendoc.annotations.Required;
import net.sourceforge.cruisecontrol.gendoc.annotations.SkipDoc;
import net.sourceforge.cruisecontrol.util.EmptyElementFilter;
import net.sourceforge.cruisecontrol.util.OSEnvironment;
import net.sourceforge.cruisecontrol.util.Util;
import net.sourceforge.cruisecontrol.util.ValidationHelper;
import net.sourceforge.cruisecontrol.util.BuildOutputLogger;
import org.jdom.Element;
import org.jdom.input.SAXBuilder;
import org.xml.sax.SAXException;
import org.xml.sax.XMLFilter;
import org.xml.sax.helpers.XMLFilterImpl;
import org.apache.log4j.Logger;
import java.io.*;

/**
 * we often see builds that fail because the previous build is still holding on to some resource.
 * we can avoid this by just building in a different process which will completely die after every
 * build.
 */
@DescriptionFile
@ExamplesFile
public class AntBuilder extends Builder {

    protected static final String DEFAULT_LOGGER = "org.apache.tools.ant.XmlLogger";

    private static final Logger LOG = Logger.getLogger(AntBuilder.class);

    private String antWorkingDir;

    private String buildFile = "build.xml";

    private String target = "";

    private String tempFileName = "log.xml";

    private String antScript;

    private String antHome;

    private boolean useLogger;

    private final List<JVMArg> args = new ArrayList<JVMArg>();

    private final List<Lib> libs = new ArrayList<Lib>();

    private final List<Listener> listeners = new ArrayList<Listener>();

    private final List<Property> properties = new ArrayList<Property>();

    private boolean useDebug = false;

    private boolean useQuiet = false;

    private boolean keepGoing = false;

    private String loggerClassName = DEFAULT_LOGGER;

    private boolean isLoggerClassNameSet;

    private File saveLogDir;

    private long timeout = ScriptRunner.NO_TIMEOUT;

    private boolean wasValidated;

    private String propertyfile;

    private String progressLoggerLib;

    public void validate() throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "7c0339e3-fcfc-4653-994f-5e5f816f9e7a");
        super.validate();
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "124e3fe2-25c3-46e2-b011-22c4a9dddc0a");
        ValidationHelper.assertIsSet(buildFile, "buildfile", this.getClass());
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "59b3ade5-69af-4117-8e01-75b1e7964f53");
        ValidationHelper.assertIsSet(target, "target", this.getClass());
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "ed6be574-9d83-4e0a-bfd8-16c8a7d043cd");
        ValidationHelper.assertFalse(useDebug && useQuiet, "'useDebug' and 'useQuiet' can't be used together");
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "b7c852f4-f990-4849-8e39-3365a5580f4c");
        if (!useLogger && (useDebug || useQuiet)) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "16803743-f617-4621-8a6e-ef17a56e5a42");
            LOG.warn("usedebug and usequiet are ignored if uselogger is not set to 'true'!");
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "068f7a3c-c231-43ad-936e-bce7812bc66a");
        if (saveLogDir != null) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "410f2071-5e8e-4c06-ab5a-4fa69dd98422");
            ValidationHelper.assertTrue(saveLogDir.isDirectory(), "'saveLogDir' must exist and be a directory");
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "f8d21816-dda3-4703-aefb-e8b80ff0bd52");
        ValidationHelper.assertFalse(antScript != null && antHome != null, "'antHome' and 'antscript' cannot both be set");
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "78206797-10e3-4b18-b82c-c04781b4ae9b");
        // Validate showAntOutput
        if (shouldAddDashboardLoggerJarToCommandLine(isLiveOutput(), useLogger)) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "3b4656fd-63f0-455d-bcb8-6b90acdbb812");
            if (progressLoggerLib == null) {
                writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "ffbc7805-620b-45f3-b2fa-1d77ddadd14c");
                // since progressLoggerLib is not specified in the config.xml,
                // we must be able to find the path to {@link AntScript#LIBNAME_PROGRESS_LOGGER}
                // to ensure the separate ant VM will have access to the required listeners
                AntScript.findDefaultProgressLoggerLib();
            } else {
                writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "b73f194c-8a43-4fcd-bafc-f5146471da88");
                // config.xml specified progressLoggerLib, so just make sure it exists
                ValidationHelper.assertExists(new File(progressLoggerLib), "progressLoggerLib", this.getClass());
            }
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "6ac48420-8a2f-4715-9cd5-2f22f5865714");
        if (antHome != null) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "b1a03e93-c978-40fd-9102-35ce3430b83c");
            final File antHomeFile = new File(antHome);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "373c02f9-998d-4647-abc2-9b225ecfd6ac");
            ValidationHelper.assertTrue(antHomeFile.exists() && antHomeFile.isDirectory(), "'antHome' must exist and be a directory. Expected to find " + antHomeFile.getAbsolutePath());
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "8056495e-90e6-4fa5-8361-1e7f3b6fdb06");
            final File antScriptInAntHome = new File(findAntScript(Util.isWindows()));
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "441c42f5-d9fc-4be7-89ce-254c7e8b2bf9");
            ValidationHelper.assertTrue(antScriptInAntHome.exists() && antScriptInAntHome.isFile(), "'antHome' must contain an ant execution script. Expected to find " + antScriptInAntHome.getAbsolutePath());
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "2491fe97-18cd-4739-a623-95c927137106");
            antScript = antScriptInAntHome.getAbsolutePath();
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "9f5d4f2e-35ce-49a1-bc15-2e1c4c9b4c50");
        if (antScript != null && !args.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "39d16883-9659-4d0a-b658-16d0df5b9f68");
            LOG.warn("jvmargs will be ignored if you specify anthome or your own antscript!");
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "7ea1f98e-15e6-42d2-87e2-4951d3ba7556");
        wasValidated = true;
    }

    /**
     * build and return the results via xml.  debug status can be determined
     * from log4j category once we get all the logging in place.
     */
    public Element build(final Map<String, String> buildProperties, final Progress progressIn) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "c1b5a636-b7b5-4683-b284-0e3cabbf5e47");
        if (!wasValidated) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "76fdc6b7-2a1b-4d9b-81f9-8ca05004f5c4");
            throw new IllegalStateException("This builder was never validated." + " The build method should not be getting called.");
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "15e72a3e-1ae2-4cd9-8204-0839c45eb35c");
        validateBuildFileExists();
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "d71f3ab8-06e1-4236-893e-39135ae445e0");
        final Progress progress = getShowProgress() ? progressIn : null;
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "8d8134b9-38f2-48de-87c2-d3f0e58422fb");
        final OSEnvironment antEnv = new OSEnvironment();
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "6ab32352-af07-49e7-ab41-951d7469e003");
        // Merge the environment with the configuration
        mergeEnv(antEnv);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "1c2adf73-a287-4319-93c9-31e939e772cc");
        final AntScript script = new AntScript();
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "21fa8193-1300-47e0-b8e8-8da13eb74c4d");
        script.setBuildProperties(buildProperties);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "dc267134-900f-46ac-8d55-59c072c3d2b9");
        script.setProperties(properties);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "0b2bc85b-dad6-4086-9600-a499f186ff69");
        script.setLibs(libs);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "b69bb529-0d41-4505-8e55-370e47988746");
        script.setListeners(listeners);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "308269da-5b9e-47e2-959f-3b952ece7dff");
        script.setUseLogger(useLogger);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "6d5f4598-8acd-402e-84bf-67002ce270dd");
        script.setUseScript(antScript != null);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "e23e2457-6285-4d0a-80d1-f84189e23190");
        script.setWindows(Util.isWindows());
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "d8d97ebb-aee4-47b3-b677-2e9d55abd5ed");
        script.setAntScript(antScript);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "1d868d3a-40a4-47ee-b05b-9e1bf6166ee6");
        script.setArgs(args);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "90c7aa1a-1898-49a8-8e62-672eb2d3740b");
        script.setBuildFile(buildFile);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "b30129e8-834e-4422-be77-b0cfb0c14df0");
        script.setTarget(target);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "e94c5f8f-34bc-45ff-b774-3dc9f9f40dbe");
        script.setLoggerClassName(loggerClassName);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "2260e4b8-cc9b-48b9-a04d-4473335df892");
        script.setIsLoggerClassNameSet(isLoggerClassNameSet);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "58f291cf-e364-480b-96ee-60e457f626e9");
        script.setShowAntOutput(isLiveOutput());
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "c4ab0986-78ae-4bb7-9b15-a58a52a9dadf");
        script.setTempFileName(tempFileName);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "4f2f7e82-5b92-41bb-ae90-c4f8b173bafb");
        script.setUseDebug(useDebug);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "f9df1faf-00d2-4c31-9063-d7c591ec921b");
        script.setUseQuiet(useQuiet);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "015cd75a-de7d-4b8a-bef7-de6b7d1ac995");
        script.setKeepGoing(keepGoing);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "4f5029a7-8f4e-435f-86f3-00809435e97d");
        script.setSystemClassPath(getSystemClassPath());
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "6eeaf587-7201-46be-90e0-6fe8cb680ab9");
        script.setPropertyFile(propertyfile);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "18111fc7-6597-4975-bfe3-f9cd6eda1289");
        script.setProgressLoggerLib(progressLoggerLib);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "32ecc77a-3523-4639-8e43-4c742ae52977");
        script.setProgress(progress);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "00ac1fc6-84df-4b6a-a9d4-9d2c16326bf2");
        script.setAntEnv(antEnv);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "1d1f7d93-2527-4324-a8e2-bc4c83387e3c");
        final File workingDir = antWorkingDir != null ? new File(antWorkingDir) : null;
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "68163788-d340-46ef-90d0-f458d6133a5f");
        final BuildOutputLogger buildOutputConsumer;
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "64bc2b02-ab61-41c7-ba78-460342d34905");
        if (isLiveOutput()) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "4f5f47f3-2105-4681-bf17-158f8ce14779");
            // TODO: I think there's a bug here when workingDir == null
            buildOutputConsumer = getBuildOutputConsumer(buildProperties.get(Builder.BUILD_PROP_PROJECTNAME), workingDir, AntOutputLogger.DEFAULT_OUTFILE_NAME);
        } else {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "230cdd3c-07d5-4ecc-8594-80e8e107c7c6");
            buildOutputConsumer = null;
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "7790f2c1-9b1a-46ad-8865-b474166a3a71");
        final boolean scriptCompleted = runScript(script, workingDir, buildOutputConsumer);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "95301ce6-cf62-4327-ae90-c361a5827ce3");
        final File logFile = new File(antWorkingDir, tempFileName);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "22955755-f3e5-4c93-8147-8ef8040d0dd5");
        final Element buildLogElement;
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "00315a50-06d5-461d-a1c0-7781d3fb8912");
        if (!scriptCompleted) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "50c8aefe-1656-492b-a41c-b780573707cb");
            LOG.warn("Build timeout timer of " + timeout + " seconds has expired");
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "9daed1b7-93f8-4933-ab4b-1b3913ecd150");
            buildLogElement = new Element("build");
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "eae9dfa8-5fae-4343-a6d0-b641247b08b4");
            buildLogElement.setAttribute("error", "build timeout");
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "03183431-5856-450f-b1e6-990d19be2c09");
            // somebody should really fix ant's XmlLogger
            if (logFile.exists()) {
                writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "21e085c6-5fa9-4657-8ccd-cc7ba13a8807");
                try {
                    writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "a379ecc1-6242-48c2-8615-118028a4918a");
                    buildLogElement.setText(Util.readFileToString(logFile));
                } catch (IOException likely) {
                // ignored
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "dc71e2f6-5c13-4903-b6f0-eafe9cf9dd8a");
            // read in log file as element, return it
            buildLogElement = getAntLogAsElement(logFile);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "baee0615-8038-4ef8-a8b7-41454f7ce6c9");
            saveAntLog(logFile);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "8f835c67-d0a7-4c89-8554-f468d67cbd73");
            logFile.delete();
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "946b3379-9802-452f-bd5b-b2b4bac503f0");
        return buildLogElement;
    }

    boolean runScript(final AntScript script, final File workingDir, final BuildOutputLogger outputLogger) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "59776d65-9fd4-445d-a641-aa4a34a25048");
        return new ScriptRunner().runScript(workingDir, script, timeout, outputLogger);
    }

    public Element buildWithTarget(final Map<String, String> properties, final String buildTarget, final Progress progress) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "7f2ccab8-11a0-4656-a81b-8fd74820375c");
        final String origTarget = target;
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "86f0791e-8326-4715-bd50-1d32d966c718");
        try {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "e1bb803d-67dd-44a9-8c6b-cdad1201a8f4");
            target = buildTarget;
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "bedaa56e-033b-4682-928b-b4612d5ac175");
            return build(properties, progress);
        } finally {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "e30b04d2-fe02-43ca-8935-105c2e9a3878");
            target = origTarget;
        }
    }

    void validateBuildFileExists() throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "70f826cf-ff06-4889-b14e-2b7fa1a5b677");
        File build = new File(buildFile);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "d3f2f574-5401-4bfd-abbf-1a595ed8371f");
        if (!build.exists() && !build.isAbsolute() && antWorkingDir != null) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "279f6744-b5ed-4576-ae06-6041cf652eb4");
            build = new File(antWorkingDir, buildFile);
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "58317b74-6bfb-4344-b3cc-200ba0719515");
        ValidationHelper.assertExists(build, "buildfile", this.getClass());
    }

    @Description("If supplied, a copy of the ant log will be saved in the specified " + "local directory. Example: saveLogDir=\"/usr/local/dev/projects/cc/logs\".")
    @Optional
    public void setSaveLogDir(String dir) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "c2f26fef-2413-4dd8-bc28-8dd250b0368b");
        saveLogDir = null;
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "94e0d3ff-ed18-4ba9-9650-99d94a260a06");
        if (dir != null && !dir.trim().equals("")) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "678a2548-40c4-4dcd-bcf8-9a5bec45e93a");
            saveLogDir = new File(dir.trim());
        }
    }

    void saveAntLog(File logFile) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "b61336c4-0cdf-4c21-bfdf-5b28a954311a");
        if (saveLogDir == null) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "fe67ac2c-e786-486f-9220-4779cb9a8025");
            return;
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "c87eefe9-0f3d-4e3e-b435-cd830043f810");
        try {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "0475126f-d970-4301-812d-f715bc54e6c6");
            final File newAntLogFile = new File(saveLogDir, tempFileName);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "2ace62ce-d3ae-40e6-8d10-1a71d99a1cce");
            newAntLogFile.createNewFile();
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "a198ff12-2512-4e56-99b3-c0bdadbbbcb2");
            final FileInputStream in = new FileInputStream(logFile);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "efee0152-cba7-428d-9ef2-adbfe5e08cf1");
            try {
                writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "e3ae4f63-ebb2-4c7c-91f2-42351ed527cb");
                final FileOutputStream out = new FileOutputStream(newAntLogFile);
                writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "bb2c9fe0-cfab-4de2-ac71-8328d814c4eb");
                try {
                    writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "535fe43e-0305-4eec-9c8b-41567ba219e7");
                    byte[] buf = new byte[1024];
                    writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "73bc42eb-8b3e-452e-8d6f-07717b72073e");
                    int len;
                    writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "0cb23a0e-c4d0-403e-9359-cc1a9086259b");
                    while ((len = in.read(buf)) > 0) {
                        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "92c1303d-c988-4788-adf8-3d30f8bd5446");
                        out.write(buf, 0, len);
                    }
                } finally {
                    writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "51fcaaaf-51d9-498b-b0c1-1eec6795a2b2");
                    out.close();
                }
            } finally {
                writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "5f660c91-becc-47b1-afea-149c68943479");
                in.close();
            }
        } catch (IOException ioe) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "55557d9f-e29c-4614-9603-0027420747f6");
            LOG.error(ioe);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "3193b348-876e-4539-a0d4-45a91701ce06");
            LOG.error("Unable to create file: " + new File(saveLogDir, tempFileName));
        }
    }

    @Description("Will invoke ANT in the specified directory. This directory can be " + "absolute or relative to the cruisecontrol working directory.")
    @Optional
    public void setAntWorkingDir(String dir) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "6019d5f7-b620-44c4-8883-726e0602250c");
        antWorkingDir = dir;
    }

    @Description("Absolute filename of script (shell script or bat file) used to start Ant. " + "You can use this to make CruiseControl use your own Ant installation. " + "If this is not specified, the AntBuilder uses the Ant distribution that " + "ships with CruiseControl. See below for <a href=\"#ant-examples\">examples" + "</a>.")
    @Optional("Recommended, however. Cannot be specified if anthome attribute " + "is also specified")
    public void setAntScript(String antScript) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "ef6f8afa-df53-4ac3-801f-4f75901d86b1");
        this.antScript = antScript;
    }

    @Description("Directory in which Ant is installed. CruiseControl will attempt to use the " + "standard Ant execution scripts (i.e. ant.bat or ant). See below for " + "<a href=\"#ant-examples\">examples</a>.")
    @Optional("Cannot be specified if antscript attribute is also specified.")
    public void setAntHome(String antHome) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "60c87e7b-e455-41f7-92b3-74db1d08f13d");
        this.antHome = antHome;
    }

    /**
     * @param isWindows if true, running under windows
     * @return If the anthome attribute is set, then this method returns the correct shell script
     * to use for a specific environment.
     * @throws CruiseControlException if <code>antHome</code> is not set
     */
    protected String findAntScript(boolean isWindows) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "06207bda-6749-49e8-827b-2a78d9710b29");
        if (antHome == null) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "527d3c78-a692-4a64-b9dc-40314f55b4ae");
            throw new CruiseControlException("anthome attribute not set.");
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "4a8f0cba-804c-428c-8706-44144b3f2f85");
        if (isWindows) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "7b9f0d2a-cd5e-489a-b22c-d23090d86e70");
            return antHome + "\\bin\\ant.bat";
        } else {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "d2e99552-b089-4bf5-8099-6e04b93ee918");
            return antHome + "/bin/ant";
        }
    }

    @Description("Name of temp file used to capture output.")
    @Optional
    @Default("log.xml")
    public void setTempFile(String tempFileName) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "f9f5f742-13e0-4b21-8dac-792cf39b717a");
        this.tempFileName = tempFileName;
    }

    @Description("Ant target(s) to run. Default is \"\", or the default target for " + "the build file.")
    @Optional
    public void setTarget(String target) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "e707fc64-6c2b-4e02-9beb-6669d9590af4");
        this.target = target;
    }

    @Description("Path to Ant build file.")
    @Optional
    @Default("build.xml")
    public void setBuildFile(String buildFile) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "33a2a5c8-eec9-4e9a-9035-419c8a92f58f");
        this.buildFile = buildFile;
    }

    @Description("'true' if CruiseControl should call Ant using -logger; 'false' to call Ant " + "using '-listener', thus using the loggerclass as a Listener. uselogger=" + "\"true\" will make Ant log its messages using the class specified by " + "loggerclassname as an Ant Logger, which can make for smaller log files since " + "it doesn't log DEBUG messages (see useDebug and useQuiet attributes below, " + "and the <a href=\"http://ant.apache.org/manual/listeners.html\">Ant manual</a>). " + "Set to false to have Ant echo ant messages to console " + "using its DefaultLogger, which is useful when debugging your ant build. " + "Defaults to 'false' to make initial setup easier but setting it to 'true' is " + "recommended for production situations." + "<br/><br/>" + "RE: liveOutput: If liveOutput=true AND uselogger=true, this builder will write " + "the ant output to a file (antBuilderOutput.log) that can be read by the " + "Dashboard reporting application. The liveOutput setting has no effect if " + "uselogger=false. <a href=\"#antbootstrapper\">AntBootstrapper</a> and " + "<a href=\"#antpublisher\">AntPublisher</a> do not provide access to " + "liveOutput, and operate as if liveOutput=false. NOTE: In order to show ant " + "output while uselogger=true, the AntBuilder uses a custom Build Listener. If " + "this interferes with your Ant build, set liveOutput=false (and please report " + "the problem)")
    @Optional
    public void setUseLogger(boolean useLogger) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "388e46bd-cd0d-4629-8650-fd4a1518f527");
        this.useLogger = useLogger;
    }

    /**
     * Sets whether Ant will use the custom AntOutputLogger as a listener in order to show live output.
     * @param showAntOutput if true, add AntOutputLogger as a listener.
     * @deprecated Use {@link #setLiveOutput(boolean)} instead.
     */
    @SkipDoc
    public void setShowAntOutput(final boolean showAntOutput) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "70029503-45b1-4016-9694-83f8c0b93e85");
        setLiveOutput(showAntOutput);
    }

    /**
     * @return true if Ant will use the custom AntOutputLogger as a listener in order to show live output.
     * @deprecated Use {@link #isLiveOutput()} instead.
     */
    boolean getShowAntOutput() {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "2a65a714-2298-4ae1-8fdc-4b80488addd1");
        return isLiveOutput();
    }

    /**
     * @param showAntOutput if false, disables Dashboard AntOutputLogger
     * @param useLogger if false, disables Dashboard AntOutputLogger
     * @return true if the jar containing the custom Dashboard logger class must be added to the command line used
     * to execute Ant.
     */
    static boolean shouldAddDashboardLoggerJarToCommandLine(final boolean showAntOutput, final boolean useLogger) {
        writelineStatic("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "9b366b0e-aa8f-42d6-8698-4d529b34ab31");
        return showAntOutput && useLogger;
    }

    @Description("Pass specified argument to the jvm used to invoke ant." + "Ignored if using anthome or antscript. The element has a single required" + "attribute: \"arg\".<br />" + "<strong>Example:</strong> <code>&lt;jvmarg arg=\"-Xmx120m\"/&gt;</code>")
    @Cardinality(min = 0, max = -1)
    public JVMArg createJVMArg() {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "b10afa51-467d-46b6-b922-9847f58b8a72");
        final JVMArg arg = new JVMArg();
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "096854ba-fcc0-44fe-9c60-85816cc1e48c");
        args.add(arg);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "9c7dae6e-4b4c-4032-bf2e-75734b6c5758");
        return arg;
    }

    @Description("Used to define additional <a " + "href=\"http://ant.apache.org/manual/running.html#libs\">library directories</a> " + "for the ant build. The element has one required attribute: \"searchPath\".<br /> " + "<strong>Example:</strong> <code>&lt;lib searchPath=\"/home/me/myantextensions\"/" + "&gt;</code>")
    @Cardinality(min = 0, max = -1)
    public Lib createLib() {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "978b35c4-966f-4793-89a5-73e7306836e8");
        final Lib lib = new Lib();
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "d2d8b877-f8a6-4a40-a8aa-38f4ceaabcd4");
        libs.add(lib);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "597bc76d-aa3c-44a9-8335-1cd73a63bd3d");
        return lib;
    }

    @Description("Used to define additional <a " + "href=\"http://ant.apache.org/manual/listeners.html\">listeners</a> for the " + "ant build. The element has one required attribute: \"classname\".<br />" + "<strong>Example:</strong> <code>&lt;listener classname=\"org.apache.tools." + "ant.listener.Log4jListener\"/&gt;</code>")
    @Cardinality(min = 0, max = -1)
    public Listener createListener() {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "77e8d68a-f8af-439f-8e4c-ca80279967f9");
        final Listener listener = new Listener();
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "a654ee0a-a6f2-4bdf-8dfb-2a1814c30ca6");
        listeners.add(listener);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "42853ede-5669-4e08-9aee-e4c34f9516ea");
        return listener;
    }

    @Description("Used to define properties for the ant build. The element has two " + "required attributes: \"name\" and \"value\". These will be passed on the " + "ant command-line as \"-Dname=value\"<br />" + "<strong>Example:</strong> <code>&lt;property name=\"foo\" value=\"bar\"/" + "&gt;</code>")
    @Cardinality(min = 0, max = -1)
    public Property createProperty() {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "62347651-aba9-424d-a149-e188608b7f9d");
        final Property property = new Property();
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "17d49018-86ed-41ae-96b2-f4a3a3e04db4");
        properties.add(property);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "57f5bd4e-19bd-4c21-8e8b-a75d607fc07f");
        return property;
    }

    protected String getSystemClassPath() {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "fcdc0696-1b47-4b49-ba27-da0182f3bc90");
        return System.getProperty("java.class.path");
    }

    protected Element getAntLogAsElement(File file) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "7b674686-ae5a-4566-bdfd-cec7bd024b50");
        if (!file.exists()) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "aac4f422-dda6-4587-bf29-6d1e8201db4e");
            throw new CruiseControlException("ant logfile " + file.getAbsolutePath() + " does not exist.");
        } else if (file.length() == 0) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "9d700ea3-dec2-422e-bac5-8d650b4fea99");
            throw new CruiseControlException("ant logfile " + file.getAbsolutePath() + " is empty. Your build probably failed. Check your CruiseControl logs.");
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "f34f316f-f903-426b-855d-68833a622905");
        try {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "70943617-ba91-45fa-a1ab-0fe929fdb407");
            SAXBuilder builder = new SAXBuilder("org.apache.xerces.parsers.SAXParser");
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "c10eb33b-d6ad-4d2b-8883-76ddc007a3d6");
            // old Ant-versions contain a bug in the XmlLogger that outputs
            // an invalid PI containing the target "xml:stylesheet"
            // instead of "xml-stylesheet": fix this
            XMLFilter piFilter = new XMLFilterImpl() {

                public void processingInstruction(String target, String data) throws SAXException {
                    if (target.equals("xml:stylesheet")) {
                        target = "xml-stylesheet";
                    }
                    super.processingInstruction(target, data);
                }
            };
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "c8226708-a884-4961-8a8a-05a36f8bd3ee");
            // get rid of empty <task>- and <message>-elements created by Ant's XmlLogger
            XMLFilter emptyTaskFilter = new EmptyElementFilter("task");
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "ea930a22-9d93-4c9b-9970-340c09b5a0fb");
            emptyTaskFilter.setParent(piFilter);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "a5e18dfb-1705-49e7-bef7-433bd0a928fc");
            XMLFilter emptyMessageFilter = new EmptyElementFilter("message");
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "16e55f8f-04f1-45bc-af1b-9dd3d67d9c1a");
            emptyMessageFilter.setParent(emptyTaskFilter);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "309f24db-76b0-4be7-9d6b-60b2e55fa493");
            builder.setXMLFilter(emptyMessageFilter);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "13b98da5-12c2-498f-a02a-7c3fbef2ffee");
            return builder.build(file).getRootElement();
        } catch (Exception ee) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "f4383a9a-bd65-4c88-8380-3f4b0dfdd33a");
            if (ee instanceof CruiseControlException) {
                writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "bd69d09c-a20e-48df-aa3f-59615c2db45e");
                throw (CruiseControlException) ee;
            }
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "075804b3-3269-4271-8a5d-3f81a4302045");
            File saveFile = new File(file.getParentFile(), System.currentTimeMillis() + file.getName());
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "c821516b-2c6a-4c2e-8897-2020c8c292fa");
            file.renameTo(saveFile);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "10f8ea7b-22bd-4252-8143-a575f1d09399");
            throw new CruiseControlException("Error reading : " + file.getAbsolutePath() + ".  Saved as : " + saveFile.getAbsolutePath(), ee);
        }
    }

    @Description("If true will invoke ant with -debug, which can be useful for debugging your " + "ant build. Defaults to 'false', cannot be set to 'true' if usequiet is " + "also set to 'true'. When used in combination with uselogger=\"true\", " + "this will result in bigger XML log files; otherwise, it will cause more " + "output to be written to the console by Ant's DefaultLogger.")
    @Optional
    public void setUseDebug(boolean debug) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "85bdc733-6558-4cb7-8f08-345f93ca1a31");
        useDebug = debug;
    }

    @Description("If true will invoke ant with -quiet, which can be useful for creating smaller " + "log files since messages with a priority of INFO will not be logged. Defaults " + "to 'false', cannot be set to 'true' if usedebug is also set to 'true'. " + "Smaller logfiles are only achieved when used in combination with uselogger=" + "\"true\", otherwise there will just be less output echoed to the console by " + "Ant's DefaultLogger." + "<br/><br/>" + "RE: showProgress: useQuiet=\"true\" will prevent any progress messages from " + "being displayed. NOTE: In order to show progress, the AntBuilder uses custom " + "Build Loggers and Listeners. If these interfere with your Ant build, set " + "showProgress=false (and please report the problem).")
    @Optional
    public void setUseQuiet(boolean quiet) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "c720581a-f926-4c67-9379-771c4805fb63");
        useQuiet = quiet;
    }

    @Description("If true will invoke ant with -keep-going, which can be useful for performing " + "build steps after an optional step fails. Defaults to 'false'.")
    @Optional
    public void setKeepGoing(boolean keepGoing) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "63868c10-f765-4c43-b93d-7aa10d2cabdb");
        this.keepGoing = keepGoing;
    }

    public String getLoggerClassName() {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "f867c085-7d7a-4daa-890a-ef9743274794");
        return loggerClassName;
    }

    @Description("If you want to use another logger (or listener, when uselogger=\"false\") than " + "Ant's XmlLogger, you can specify the classname of the logger here. The logger " + "needs to output compatible XML, and the class needs to be available on the " + "classpath at buildtime.")
    @Optional
    @Default("org.apache.tools.ant.XmlLogger")
    public void setLoggerClassName(String string) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "8786debc-0cf1-4d23-8802-49624c20e9d9");
        loggerClassName = string;
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "12a9970b-ff10-4cd9-88ca-6a86e6e1e52d");
        isLoggerClassNameSet = true;
    }

    @Description("Passes an argument to the JVM used to invoke ANT.")
    public class JVMArg implements Serializable {

        private static final long serialVersionUID = 402625457108399047L;

        private String arg;

        @Description("Command-line argument to pass to the ANT JVM.")
        @Required
        public void setArg(String arg) {
            this.arg = arg;
        }

        public String getArg() {
            return arg;
        }
    }

    @Description("Provides additional library directories for an ANT build.")
    public class Lib implements Serializable {

        private static final long serialVersionUID = 1804469347425625224L;

        private String searchPath;

        @Description("Path to use for loading libraries into the ANT JVM.")
        @Required
        public void setSearchPath(String searchPath) {
            this.searchPath = searchPath;
        }

        public String getSearchPath() {
            return searchPath;
        }
    }

    @Description("Provides additional listeners for an ANT build.")
    public class Listener implements Serializable {

        private static final long serialVersionUID = 4813682685614734386L;

        private String className;

        @Description("Name of the Java listener class to register with this ANT build.")
        @Required
        public void setClassName(String className) {
            this.className = className;
        }

        public String getClassName() {
            return className;
        }
    }

    @Description("Ant build will be halted if it continues longer than the specified timeout. " + "Value in seconds.")
    @Optional
    public void setTimeout(long timeout) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "3b311fd0-1428-465b-97f8-fd1e5ed67e6d");
        this.timeout = timeout;
    }

    @Description("Load all properties from file with -D properties (like child <code><a href=\"" + "#antbuilderchildprop\">&lt;property&gt;</a></code> elements) taking " + "precedence. Useful when the propertyfile content can change for every build.")
    @Optional
    public void setPropertyfile(String propertyfile) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "8eb516c8-0565-46ef-b548-d18b8f8beaed");
        this.propertyfile = propertyfile;
    }

    @Description("Overrides the default -lib search path used to add support for showProgress " + "features in the ant builder. This search path ensures customized ant " + "Loggers/Listeners are available on the classpath of the ant builder VM. You " + "should not normally set this value. If you do set this value, you should " + "use the full path (including filename) to cruisecontrol-antprogresslogger.jar. " + "This setting has no effect if showProgress=false.")
    @Optional
    public void setProgressLoggerLib(String progressLoggerLib) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "711ac224-5de4-43c9-a68d-21dc5e9ac03a");
        this.progressLoggerLib = progressLoggerLib;
    }

    /**
     * @return The path (including filename) to the jar file
     * ({@link AntScript#LIBNAME_PROGRESS_LOGGER cruisecontrol-antprogresslogger.jar})
     * containing the AntProgressLogger/Listener classes.
     */
    public String getProgressLoggerLib() {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_10_10.coverage", "9d439ce2-0e6c-40ed-958b-ac4fe23d9539");
        return progressLoggerLib;
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
