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
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "2ea0496b-5b74-40ae-877f-6274a6de1f3f");
        super.validate();
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "a3537219-4fed-4011-88e1-435e02227171");
        ValidationHelper.assertIsSet(buildFile, "buildfile", this.getClass());
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "2e47ca90-af82-4af4-b3bb-4803306318ed");
        ValidationHelper.assertIsSet(target, "target", this.getClass());
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "b11514a2-0129-4513-be55-67a48727ecdb");
        ValidationHelper.assertFalse(useDebug && useQuiet, "'useDebug' and 'useQuiet' can't be used together");
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "2921452d-fe6d-4ba2-85c5-6a1b27afedf9");
        if (!useLogger && (useDebug || useQuiet)) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "6206d7f8-a94e-44d1-9584-df1aa0df393e");
            LOG.warn("usedebug and usequiet are ignored if uselogger is not set to 'true'!");
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "feaa281c-12d6-4e3b-8b7f-33dc82d8a8cd");
        if (saveLogDir != null) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "0737b92e-62a7-4b71-8886-d6f9c9b16e24");
            ValidationHelper.assertTrue(saveLogDir.isDirectory(), "'saveLogDir' must exist and be a directory");
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "618a11c9-b941-495c-80e2-d94f9c33c8f4");
        ValidationHelper.assertFalse(antScript != null && antHome != null, "'antHome' and 'antscript' cannot both be set");
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "314668b9-c08e-4e7d-8900-5ee362c5baa2");
        // Validate showAntOutput
        if (shouldAddDashboardLoggerJarToCommandLine(isLiveOutput(), useLogger)) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "4e805b8f-7cbf-44e5-b781-09fae1524a4c");
            if (progressLoggerLib == null) {
                writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "53f8f79d-4e31-41a5-ac20-1ab16874eaed");
                // since progressLoggerLib is not specified in the config.xml,
                // we must be able to find the path to {@link AntScript#LIBNAME_PROGRESS_LOGGER}
                // to ensure the separate ant VM will have access to the required listeners
                AntScript.findDefaultProgressLoggerLib();
            } else {
                writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "7cb237e1-bf12-442d-b22a-b28412d2a1fd");
                // config.xml specified progressLoggerLib, so just make sure it exists
                ValidationHelper.assertExists(new File(progressLoggerLib), "progressLoggerLib", this.getClass());
            }
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "165befbb-2012-4d2e-9eb6-6678a691fd90");
        if (antHome != null) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "3c0d0d6d-d9bb-4767-bc52-941d3a0d18da");
            final File antHomeFile = new File(antHome);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "25403078-c7a7-4a75-b7ec-5e4236ed0d24");
            ValidationHelper.assertTrue(antHomeFile.exists() && antHomeFile.isDirectory(), "'antHome' must exist and be a directory. Expected to find " + antHomeFile.getAbsolutePath());
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "e895790f-5fbd-4930-b159-20bc3b51269f");
            final File antScriptInAntHome = new File(findAntScript(Util.isWindows()));
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "97cd5286-032e-41d9-8d4e-0a8869388c03");
            ValidationHelper.assertTrue(antScriptInAntHome.exists() && antScriptInAntHome.isFile(), "'antHome' must contain an ant execution script. Expected to find " + antScriptInAntHome.getAbsolutePath());
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "73c2f2af-683f-41b0-bf22-415937ed8d9d");
            antScript = antScriptInAntHome.getAbsolutePath();
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "24c12a74-4bbf-4076-85e2-3056b6c09211");
        if (antScript != null && !args.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "38da1ce3-336d-4de7-b9c1-cb370157f5b9");
            LOG.warn("jvmargs will be ignored if you specify anthome or your own antscript!");
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "f4db5dae-d4fe-4bf4-8dc5-dbfb3c15798c");
        wasValidated = true;
    }

    /**
     * build and return the results via xml.  debug status can be determined
     * from log4j category once we get all the logging in place.
     */
    public Element build(final Map<String, String> buildProperties, final Progress progressIn) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "8d08c02b-4043-402a-8905-5d263e94e59c");
        if (!wasValidated) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "5ebfdffb-3ddc-4e13-bd80-f1ff9c442d0e");
            throw new IllegalStateException("This builder was never validated." + " The build method should not be getting called.");
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "1010c558-4b6b-45d2-a40f-e5066d21aaa4");
        validateBuildFileExists();
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "64f34233-90a8-4a45-bf0a-08a19bc59519");
        final Progress progress = getShowProgress() ? progressIn : null;
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "358a73ff-3d06-4f62-8284-5899a133ea49");
        final OSEnvironment antEnv = new OSEnvironment();
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "ca0cc978-e983-412f-aeac-93f83996c93e");
        // Merge the environment with the configuration
        mergeEnv(antEnv);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "77cdf4ee-c39c-4ef0-8f7d-14d605961fcd");
        final AntScript script = new AntScript();
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "31205bfb-20fe-469e-aee4-60e3f51a5383");
        script.setBuildProperties(buildProperties);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "a6915e13-cd7d-462a-85ba-cb5066f22d53");
        script.setProperties(properties);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "2b016173-646c-4422-aafa-b55bfde47c83");
        script.setLibs(libs);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "c591ac3e-45e1-411b-b61f-680c1256d01f");
        script.setListeners(listeners);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "c722c0e5-d2e5-4c16-9037-36d445b97199");
        script.setUseLogger(useLogger);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "3299b6ad-d3aa-4f49-ab44-e38ce72de070");
        script.setUseScript(antScript != null);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "15a08f5f-bf7a-440c-bd37-1ba0f71b98e6");
        script.setWindows(Util.isWindows());
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "93ea7baf-033d-4993-b469-fee54bc78b22");
        script.setAntScript(antScript);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "b1731372-729b-4da3-9c44-6c84c7ee648d");
        script.setArgs(args);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "800d2a17-3577-4edd-9378-e3ac1ff1a946");
        script.setBuildFile(buildFile);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "57883502-69cc-4800-ba16-7ec7c64d5ab6");
        script.setTarget(target);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "be042a25-1e0b-4603-98e0-f552ca9029fc");
        script.setLoggerClassName(loggerClassName);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "66787b5e-0ad0-4000-b7e7-9cbbfef33a0a");
        script.setIsLoggerClassNameSet(isLoggerClassNameSet);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "41daafa8-40bf-425f-8974-0b37d5bb1e92");
        script.setShowAntOutput(isLiveOutput());
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "deb71d77-6d4d-4ecd-92b1-1903c062b625");
        script.setTempFileName(tempFileName);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "7204e7fc-b58a-448c-906f-e06f038b3c24");
        script.setUseDebug(useDebug);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "2b7d3078-596b-4d47-b222-a3313787b3f3");
        script.setUseQuiet(useQuiet);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "51f5acf7-e108-45a6-a53b-4dfa2a0e9131");
        script.setKeepGoing(keepGoing);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "5fdab7b9-7099-4b91-bc89-560030747bd2");
        script.setSystemClassPath(getSystemClassPath());
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "751d2203-b8e1-4f74-90af-2a9d204a792d");
        script.setPropertyFile(propertyfile);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "77ab943e-e503-4b72-bf88-80a52c37ddbc");
        script.setProgressLoggerLib(progressLoggerLib);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "7feebffb-8134-4e66-85b0-bdb00ae10142");
        script.setProgress(progress);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "bc55e798-63b7-4530-aeb1-5b3a7d7e2611");
        script.setAntEnv(antEnv);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "f4980b4c-10c0-4612-a434-ce89c2ad167f");
        final File workingDir = antWorkingDir != null ? new File(antWorkingDir) : null;
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "43ebc8a2-b94a-4204-b415-90b88e3a37de");
        final BuildOutputLogger buildOutputConsumer;
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "d9fb64c4-c29e-4cfc-8925-12e5d883ec81");
        if (isLiveOutput()) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "ef2234fe-479c-4352-8517-ca714fd184fe");
            // TODO: I think there's a bug here when workingDir == null
            buildOutputConsumer = getBuildOutputConsumer(buildProperties.get(Builder.BUILD_PROP_PROJECTNAME), workingDir, AntOutputLogger.DEFAULT_OUTFILE_NAME);
        } else {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "3b8ab133-fb67-4fac-b93f-b93d1b609672");
            buildOutputConsumer = null;
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "97079956-2c9c-450b-bbcb-e589f771a1a6");
        final boolean scriptCompleted = runScript(script, workingDir, buildOutputConsumer);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "a54fec67-c37f-4142-aab5-22b9b4a208ef");
        final File logFile = new File(antWorkingDir, tempFileName);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "39145b36-e4c5-47d4-a21f-3cdf929c5334");
        final Element buildLogElement;
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "ce8a099d-00e0-4253-8b13-2ec6befe9572");
        if (!scriptCompleted) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "6ccfae42-b6a3-49d2-89cd-bc1d6df96cc2");
            LOG.warn("Build timeout timer of " + timeout + " seconds has expired");
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "98a1ef43-5384-48c6-8262-5f1adb961c2b");
            buildLogElement = new Element("build");
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "bbd58f1f-6e39-4e49-b912-765b5ce1485c");
            buildLogElement.setAttribute("error", "build timeout");
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "e3b23587-558a-4ead-a2e4-e1b21f1804d9");
            // somebody should really fix ant's XmlLogger
            if (logFile.exists()) {
                writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "06a69b3b-a3e9-4c77-b5b0-2f42c120aad6");
                try {
                    writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "7c51af49-fdbd-4594-b8ce-e9b861fbe394");
                    buildLogElement.setText(Util.readFileToString(logFile));
                } catch (IOException likely) {
                // ignored
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "86cca08a-20d6-4aa0-ba6e-e5b1e6192b29");
            // read in log file as element, return it
            buildLogElement = getAntLogAsElement(logFile);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "49e2c241-400f-488f-97f9-7e8ff4b2d6ae");
            saveAntLog(logFile);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "bfe72932-f6de-4e92-81c8-c364f8370262");
            logFile.delete();
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "de21465f-787d-498f-bc08-ea960ec29b1d");
        return buildLogElement;
    }

    boolean runScript(final AntScript script, final File workingDir, final BuildOutputLogger outputLogger) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "a9be20f7-505d-4d43-83e0-3714911d0b77");
        return new ScriptRunner().runScript(workingDir, script, timeout, outputLogger);
    }

    public Element buildWithTarget(final Map<String, String> properties, final String buildTarget, final Progress progress) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "17bec81f-41bb-4cf4-a924-8fc806b8b66d");
        final String origTarget = target;
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "b778a2d2-0152-49e7-99a2-cbbaed997e71");
        try {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "1c436b08-65bf-4ba4-832d-154bc0c0c242");
            target = buildTarget;
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "5b5d671a-d730-4736-8524-ffe6f9ec36af");
            return build(properties, progress);
        } finally {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "d0f46d14-da42-4644-9873-172f083663f5");
            target = origTarget;
        }
    }

    void validateBuildFileExists() throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "4b2d30fc-7061-4230-9552-5ca6a703e9e5");
        File build = new File(buildFile);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "42c4f8e8-1580-407d-8785-18bb6c15dcfe");
        if (!build.exists() && !build.isAbsolute() && antWorkingDir != null) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "07b399f6-8f29-4ba2-82e5-07c449b6eb63");
            build = new File(antWorkingDir, buildFile);
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "606dd4b1-fc08-46b2-8169-6d2ee69ae7bb");
        ValidationHelper.assertExists(build, "buildfile", this.getClass());
    }

    @Description("If supplied, a copy of the ant log will be saved in the specified " + "local directory. Example: saveLogDir=\"/usr/local/dev/projects/cc/logs\".")
    @Optional
    public void setSaveLogDir(String dir) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "d573f821-6496-467d-b75f-0d0347a77e1f");
        saveLogDir = null;
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "8845f993-f832-455e-9d19-5fbbccadf242");
        if (dir != null && !dir.trim().equals("")) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "450d6e24-cf78-4fbb-b842-0992ade02b6c");
            saveLogDir = new File(dir.trim());
        }
    }

    void saveAntLog(File logFile) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "f911b944-b24f-4a1b-b125-9b2698c64027");
        if (saveLogDir == null) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "bc6346f0-320d-438c-8b9d-5393a2b07104");
            return;
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "92f60f98-e0f6-4431-a830-dde6c7be6d1f");
        try {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "3c331078-49aa-4ad1-864e-28a1292a1683");
            final File newAntLogFile = new File(saveLogDir, tempFileName);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "90ca9f2b-d868-4da8-9d45-ba72be66996c");
            newAntLogFile.createNewFile();
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "8815e2c0-dccd-4030-9b50-97fe4230d8f7");
            final FileInputStream in = new FileInputStream(logFile);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "506a4064-6291-4d46-90ca-6f429c1e9a7b");
            try {
                writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "2297a2a1-6c70-43a6-bd2c-5fc356d6d64a");
                final FileOutputStream out = new FileOutputStream(newAntLogFile);
                writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "9b8e46fb-b0a9-4cf1-81cd-15b7525951a7");
                try {
                    writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "53bd3ac3-3e72-41d4-94e3-bc038a4de476");
                    byte[] buf = new byte[1024];
                    writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "165b36d1-2eaa-4ad5-8f8d-3e8b3fac50a7");
                    int len;
                    writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "4a0ae022-1516-4372-9850-bca7b9b66ba5");
                    while ((len = in.read(buf)) > 0) {
                        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "3e581da1-b8f0-45b0-a685-2b532616337c");
                        out.write(buf, 0, len);
                    }
                } finally {
                    writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "a47c604c-92c2-41c0-9efe-be64f577c80f");
                    out.close();
                }
            } finally {
                writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "65051f18-3adc-446e-b92a-b77b6e4bec71");
                in.close();
            }
        } catch (IOException ioe) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "71344742-774c-45e2-809f-a5faf2b63da3");
            LOG.error(ioe);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "2480d6e7-3e0d-433c-a82e-fe22c178e890");
            LOG.error("Unable to create file: " + new File(saveLogDir, tempFileName));
        }
    }

    @Description("Will invoke ANT in the specified directory. This directory can be " + "absolute or relative to the cruisecontrol working directory.")
    @Optional
    public void setAntWorkingDir(String dir) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "21b5a349-7fb3-4625-bb4e-747bbd533df7");
        antWorkingDir = dir;
    }

    @Description("Absolute filename of script (shell script or bat file) used to start Ant. " + "You can use this to make CruiseControl use your own Ant installation. " + "If this is not specified, the AntBuilder uses the Ant distribution that " + "ships with CruiseControl. See below for <a href=\"#ant-examples\">examples" + "</a>.")
    @Optional("Recommended, however. Cannot be specified if anthome attribute " + "is also specified")
    public void setAntScript(String antScript) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "43ef1399-0e30-419c-864c-6c8743174671");
        this.antScript = antScript;
    }

    @Description("Directory in which Ant is installed. CruiseControl will attempt to use the " + "standard Ant execution scripts (i.e. ant.bat or ant). See below for " + "<a href=\"#ant-examples\">examples</a>.")
    @Optional("Cannot be specified if antscript attribute is also specified.")
    public void setAntHome(String antHome) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "f097e451-36a0-4097-9bd8-a2cedbe4301f");
        this.antHome = antHome;
    }

    /**
     * @param isWindows if true, running under windows
     * @return If the anthome attribute is set, then this method returns the correct shell script
     * to use for a specific environment.
     * @throws CruiseControlException if <code>antHome</code> is not set
     */
    protected String findAntScript(boolean isWindows) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "3b4d5521-151e-4092-92cd-a2ec1910ba00");
        if (antHome == null) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "58d185a6-e21d-4ece-b54c-0113cf942375");
            throw new CruiseControlException("anthome attribute not set.");
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "0c581a70-fc4f-42b0-990e-ba9328369d9d");
        if (isWindows) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "c3bc515d-4ac6-44ce-9adf-d6fec127f41f");
            return antHome + "\\bin\\ant.bat";
        } else {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "be26c44d-35a5-4f9b-b1aa-8dd3b8720127");
            return antHome + "/bin/ant";
        }
    }

    @Description("Name of temp file used to capture output.")
    @Optional
    @Default("log.xml")
    public void setTempFile(String tempFileName) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "efd10470-8d67-4b03-a041-198abacacb46");
        this.tempFileName = tempFileName;
    }

    @Description("Ant target(s) to run. Default is \"\", or the default target for " + "the build file.")
    @Optional
    public void setTarget(String target) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "c2c1ce0c-54ca-4a52-9aa9-c4c576c5f86b");
        this.target = target;
    }

    @Description("Path to Ant build file.")
    @Optional
    @Default("build.xml")
    public void setBuildFile(String buildFile) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "5a68cd2f-217c-40af-a886-0df9de4c040e");
        this.buildFile = buildFile;
    }

    @Description("'true' if CruiseControl should call Ant using -logger; 'false' to call Ant " + "using '-listener', thus using the loggerclass as a Listener. uselogger=" + "\"true\" will make Ant log its messages using the class specified by " + "loggerclassname as an Ant Logger, which can make for smaller log files since " + "it doesn't log DEBUG messages (see useDebug and useQuiet attributes below, " + "and the <a href=\"http://ant.apache.org/manual/listeners.html\">Ant manual</a>). " + "Set to false to have Ant echo ant messages to console " + "using its DefaultLogger, which is useful when debugging your ant build. " + "Defaults to 'false' to make initial setup easier but setting it to 'true' is " + "recommended for production situations." + "<br/><br/>" + "RE: liveOutput: If liveOutput=true AND uselogger=true, this builder will write " + "the ant output to a file (antBuilderOutput.log) that can be read by the " + "Dashboard reporting application. The liveOutput setting has no effect if " + "uselogger=false. <a href=\"#antbootstrapper\">AntBootstrapper</a> and " + "<a href=\"#antpublisher\">AntPublisher</a> do not provide access to " + "liveOutput, and operate as if liveOutput=false. NOTE: In order to show ant " + "output while uselogger=true, the AntBuilder uses a custom Build Listener. If " + "this interferes with your Ant build, set liveOutput=false (and please report " + "the problem)")
    @Optional
    public void setUseLogger(boolean useLogger) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "65d76f5a-455f-4abd-b6e4-aa81e795b25d");
        this.useLogger = useLogger;
    }

    /**
     * Sets whether Ant will use the custom AntOutputLogger as a listener in order to show live output.
     * @param showAntOutput if true, add AntOutputLogger as a listener.
     * @deprecated Use {@link #setLiveOutput(boolean)} instead.
     */
    @SkipDoc
    public void setShowAntOutput(final boolean showAntOutput) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "8f750307-2e91-4b1e-ac03-c807d4eef6e2");
        setLiveOutput(showAntOutput);
    }

    /**
     * @return true if Ant will use the custom AntOutputLogger as a listener in order to show live output.
     * @deprecated Use {@link #isLiveOutput()} instead.
     */
    boolean getShowAntOutput() {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "b59fd85c-ee2b-4d78-9523-3634240a7198");
        return isLiveOutput();
    }

    /**
     * @param showAntOutput if false, disables Dashboard AntOutputLogger
     * @param useLogger if false, disables Dashboard AntOutputLogger
     * @return true if the jar containing the custom Dashboard logger class must be added to the command line used
     * to execute Ant.
     */
    static boolean shouldAddDashboardLoggerJarToCommandLine(final boolean showAntOutput, final boolean useLogger) {
        writelineStatic("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "2a5683d8-5176-484b-9cc9-d3ac3e884989");
        return showAntOutput && useLogger;
    }

    @Description("Pass specified argument to the jvm used to invoke ant." + "Ignored if using anthome or antscript. The element has a single required" + "attribute: \"arg\".<br />" + "<strong>Example:</strong> <code>&lt;jvmarg arg=\"-Xmx120m\"/&gt;</code>")
    @Cardinality(min = 0, max = -1)
    public JVMArg createJVMArg() {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "39495d05-114e-48b4-818d-9e6841add1a0");
        final JVMArg arg = new JVMArg();
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "7a853b13-c12f-4e80-82ab-a4e466d1aecd");
        args.add(arg);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "bbee58de-78c9-4bb9-b54f-827df44c13eb");
        return arg;
    }

    @Description("Used to define additional <a " + "href=\"http://ant.apache.org/manual/running.html#libs\">library directories</a> " + "for the ant build. The element has one required attribute: \"searchPath\".<br /> " + "<strong>Example:</strong> <code>&lt;lib searchPath=\"/home/me/myantextensions\"/" + "&gt;</code>")
    @Cardinality(min = 0, max = -1)
    public Lib createLib() {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "6c3e1705-a969-4531-ad89-92ee083220e9");
        final Lib lib = new Lib();
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "803e97eb-cd03-4cd6-b2e3-5dedc396e786");
        libs.add(lib);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "3eeea8dc-1f3e-46c2-8a9f-b28e239c9732");
        return lib;
    }

    @Description("Used to define additional <a " + "href=\"http://ant.apache.org/manual/listeners.html\">listeners</a> for the " + "ant build. The element has one required attribute: \"classname\".<br />" + "<strong>Example:</strong> <code>&lt;listener classname=\"org.apache.tools." + "ant.listener.Log4jListener\"/&gt;</code>")
    @Cardinality(min = 0, max = -1)
    public Listener createListener() {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "3853c3a8-78c2-4e6b-9177-fd99711e85b3");
        final Listener listener = new Listener();
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "59da599c-a2e7-4180-af77-267186df5b35");
        listeners.add(listener);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "c644ea89-12f4-4a1e-a11a-a5c47f2de43b");
        return listener;
    }

    @Description("Used to define properties for the ant build. The element has two " + "required attributes: \"name\" and \"value\". These will be passed on the " + "ant command-line as \"-Dname=value\"<br />" + "<strong>Example:</strong> <code>&lt;property name=\"foo\" value=\"bar\"/" + "&gt;</code>")
    @Cardinality(min = 0, max = -1)
    public Property createProperty() {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "1a500c83-91f6-4a05-8723-34cb6c603c4c");
        final Property property = new Property();
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "14bb373e-e940-4adc-a85a-e7efc7d92c98");
        properties.add(property);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "b0b2b5b0-7cc0-4a2c-8fb6-7989230e72cf");
        return property;
    }

    protected String getSystemClassPath() {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "444306a9-f7e5-4491-a673-68187609a0af");
        return System.getProperty("java.class.path");
    }

    protected Element getAntLogAsElement(File file) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "25dae1ed-7a0d-4590-844a-08ce62d7eacd");
        if (!file.exists()) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "5eb655be-50d5-41ff-9154-5de6a2eb75c3");
            throw new CruiseControlException("ant logfile " + file.getAbsolutePath() + " does not exist.");
        } else if (file.length() == 0) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "15ce0896-dbf5-4d0b-afaa-769b9213982a");
            throw new CruiseControlException("ant logfile " + file.getAbsolutePath() + " is empty. Your build probably failed. Check your CruiseControl logs.");
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "569e3887-846e-44dc-8d19-69aadc3e10bf");
        try {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "e623167f-4b82-49ea-a388-7295b3ac19f8");
            SAXBuilder builder = new SAXBuilder("org.apache.xerces.parsers.SAXParser");
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "772736ca-c48c-4a87-9a6d-fc321e9d4623");
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
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "a4608435-8a85-48d1-ac5b-fbc95d806574");
            // get rid of empty <task>- and <message>-elements created by Ant's XmlLogger
            XMLFilter emptyTaskFilter = new EmptyElementFilter("task");
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "00f0c3a6-504a-451e-ad70-ad9283649467");
            emptyTaskFilter.setParent(piFilter);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "45a78135-b6bb-47f3-b87a-fcb90e27d216");
            XMLFilter emptyMessageFilter = new EmptyElementFilter("message");
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "989d8b83-89e8-4fb2-b828-5661c9386547");
            emptyMessageFilter.setParent(emptyTaskFilter);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "911e6f5f-af63-4297-990d-384172e486b4");
            builder.setXMLFilter(emptyMessageFilter);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "5d71958c-022c-4fec-82aa-70a86871ca5e");
            return builder.build(file).getRootElement();
        } catch (Exception ee) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "4b495a36-f0ea-4236-99d0-d474570d3cad");
            if (ee instanceof CruiseControlException) {
                writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "8c4b9709-f4c7-45de-ada7-e14ac346fd2c");
                throw (CruiseControlException) ee;
            }
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "42704fcd-a8bc-44dc-bab1-c1acd4f33006");
            File saveFile = new File(file.getParentFile(), System.currentTimeMillis() + file.getName());
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "492afee6-551d-465d-b0d7-2b1beede750d");
            file.renameTo(saveFile);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "124d79ff-de22-4567-9d3b-ee4cf068eb39");
            throw new CruiseControlException("Error reading : " + file.getAbsolutePath() + ".  Saved as : " + saveFile.getAbsolutePath(), ee);
        }
    }

    @Description("If true will invoke ant with -debug, which can be useful for debugging your " + "ant build. Defaults to 'false', cannot be set to 'true' if usequiet is " + "also set to 'true'. When used in combination with uselogger=\"true\", " + "this will result in bigger XML log files; otherwise, it will cause more " + "output to be written to the console by Ant's DefaultLogger.")
    @Optional
    public void setUseDebug(boolean debug) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "3465ce94-3882-4c3a-bd30-59eba73ebf26");
        useDebug = debug;
    }

    @Description("If true will invoke ant with -quiet, which can be useful for creating smaller " + "log files since messages with a priority of INFO will not be logged. Defaults " + "to 'false', cannot be set to 'true' if usedebug is also set to 'true'. " + "Smaller logfiles are only achieved when used in combination with uselogger=" + "\"true\", otherwise there will just be less output echoed to the console by " + "Ant's DefaultLogger." + "<br/><br/>" + "RE: showProgress: useQuiet=\"true\" will prevent any progress messages from " + "being displayed. NOTE: In order to show progress, the AntBuilder uses custom " + "Build Loggers and Listeners. If these interfere with your Ant build, set " + "showProgress=false (and please report the problem).")
    @Optional
    public void setUseQuiet(boolean quiet) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "f63a77b1-887c-4a46-bd57-391281d5bce9");
        useQuiet = quiet;
    }

    @Description("If true will invoke ant with -keep-going, which can be useful for performing " + "build steps after an optional step fails. Defaults to 'false'.")
    @Optional
    public void setKeepGoing(boolean keepGoing) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "6cca262a-a928-4772-865d-23893049b5b6");
        this.keepGoing = keepGoing;
    }

    public String getLoggerClassName() {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "a5aeaa75-2be2-4114-89d0-41c9dde08ab5");
        return loggerClassName;
    }

    @Description("If you want to use another logger (or listener, when uselogger=\"false\") than " + "Ant's XmlLogger, you can specify the classname of the logger here. The logger " + "needs to output compatible XML, and the class needs to be available on the " + "classpath at buildtime.")
    @Optional
    @Default("org.apache.tools.ant.XmlLogger")
    public void setLoggerClassName(String string) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "6e97a84d-7d01-40e6-90f4-24eaecc66f72");
        loggerClassName = string;
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "e1b91e20-d572-4eaa-b211-3c6c3a0a7319");
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
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "48f43780-9843-4e79-8a36-24936bbbff00");
        this.timeout = timeout;
    }

    @Description("Load all properties from file with -D properties (like child <code><a href=\"" + "#antbuilderchildprop\">&lt;property&gt;</a></code> elements) taking " + "precedence. Useful when the propertyfile content can change for every build.")
    @Optional
    public void setPropertyfile(String propertyfile) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "ea894f7c-a810-4055-90d5-803de24b91b3");
        this.propertyfile = propertyfile;
    }

    @Description("Overrides the default -lib search path used to add support for showProgress " + "features in the ant builder. This search path ensures customized ant " + "Loggers/Listeners are available on the classpath of the ant builder VM. You " + "should not normally set this value. If you do set this value, you should " + "use the full path (including filename) to cruisecontrol-antprogresslogger.jar. " + "This setting has no effect if showProgress=false.")
    @Optional
    public void setProgressLoggerLib(String progressLoggerLib) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "4453006d-c098-4ffc-954a-e3f4a587c8f1");
        this.progressLoggerLib = progressLoggerLib;
    }

    /**
     * @return The path (including filename) to the jar file
     * ({@link AntScript#LIBNAME_PROGRESS_LOGGER cruisecontrol-antprogresslogger.jar})
     * containing the AntProgressLogger/Listener classes.
     */
    public String getProgressLoggerLib() {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_7_10.coverage", "12a97179-3736-4242-8dc6-dea4a8044310");
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
