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
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "62054208-5b19-445e-92bf-dc13fbb9e0af");
        super.validate();
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "a4e1a9ca-a4dc-404a-a45a-7c1ae0702571");
        ValidationHelper.assertIsSet(buildFile, "buildfile", this.getClass());
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "6c3c8e54-393d-47c7-831f-e608b64303da");
        ValidationHelper.assertIsSet(target, "target", this.getClass());
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "53c2ddbd-0b53-46ec-a818-46f8bdbca2ea");
        ValidationHelper.assertFalse(useDebug && useQuiet, "'useDebug' and 'useQuiet' can't be used together");
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "77711412-dd90-4418-bd4d-1c2ede1781a7");
        if (!useLogger && (useDebug || useQuiet)) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "b087f51f-9393-4c3a-9dae-78fb34ff2296");
            LOG.warn("usedebug and usequiet are ignored if uselogger is not set to 'true'!");
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "f114315a-a9cf-47c6-a494-11f65e1e4060");
        if (saveLogDir != null) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "68fffb6f-36c7-401f-ac1f-a1420e91ccb0");
            ValidationHelper.assertTrue(saveLogDir.isDirectory(), "'saveLogDir' must exist and be a directory");
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "d9ce70e4-62ae-4bd0-908c-ca8c990aa3c4");
        ValidationHelper.assertFalse(antScript != null && antHome != null, "'antHome' and 'antscript' cannot both be set");
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "7a2f7ee0-3bc7-4837-a244-9deb2a1bbbc2");
        // Validate showAntOutput
        if (shouldAddDashboardLoggerJarToCommandLine(isLiveOutput(), useLogger)) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "ad1d489d-633a-46ba-8716-d333fd8924f9");
            if (progressLoggerLib == null) {
                writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "d3420d6b-ffd0-476d-b26d-ee74c5431af7");
                // since progressLoggerLib is not specified in the config.xml,
                // we must be able to find the path to {@link AntScript#LIBNAME_PROGRESS_LOGGER}
                // to ensure the separate ant VM will have access to the required listeners
                AntScript.findDefaultProgressLoggerLib();
            } else {
                writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "2bd73a53-636b-411f-b99b-6a00011a3cb8");
                // config.xml specified progressLoggerLib, so just make sure it exists
                ValidationHelper.assertExists(new File(progressLoggerLib), "progressLoggerLib", this.getClass());
            }
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "46dec4a8-3d03-4070-94f1-171b79448909");
        if (antHome != null) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "5f25ea44-b94b-46f0-b48e-053c4edae04f");
            final File antHomeFile = new File(antHome);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "52444844-042c-40fc-ab03-3c69986181c1");
            ValidationHelper.assertTrue(antHomeFile.exists() && antHomeFile.isDirectory(), "'antHome' must exist and be a directory. Expected to find " + antHomeFile.getAbsolutePath());
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "20b32d60-3393-4967-b262-67e464e14747");
            final File antScriptInAntHome = new File(findAntScript(Util.isWindows()));
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "f3ec8866-450d-4b20-a46d-5745c4291d33");
            ValidationHelper.assertTrue(antScriptInAntHome.exists() && antScriptInAntHome.isFile(), "'antHome' must contain an ant execution script. Expected to find " + antScriptInAntHome.getAbsolutePath());
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "15b4c9f1-c596-45f9-9d9e-54ef10b285d2");
            antScript = antScriptInAntHome.getAbsolutePath();
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "de33c94e-c7ce-487e-89f4-944195e0ff5f");
        if (antScript != null && !args.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "d6be944c-0d3d-46f2-bca4-8cfb1606a70b");
            LOG.warn("jvmargs will be ignored if you specify anthome or your own antscript!");
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "04699798-a021-47b7-83c5-cf2d6228bb8b");
        wasValidated = true;
    }

    /**
     * build and return the results via xml.  debug status can be determined
     * from log4j category once we get all the logging in place.
     */
    public Element build(final Map<String, String> buildProperties, final Progress progressIn) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "f5cbd945-ffe7-4a53-9cc4-0890da2bdf36");
        if (!wasValidated) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "c441ad4c-144c-4b8e-9424-f31d1a016b53");
            throw new IllegalStateException("This builder was never validated." + " The build method should not be getting called.");
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "c77bb943-6d01-4910-b61c-fc41d8da91fc");
        validateBuildFileExists();
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "c6c84d0c-46d1-45c6-b63d-86e589459a12");
        final Progress progress = getShowProgress() ? progressIn : null;
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "6d545992-e3a8-45e4-bf15-8eeb568068f6");
        final OSEnvironment antEnv = new OSEnvironment();
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "beaaed09-b73a-4b89-92d8-b498adfb95f7");
        // Merge the environment with the configuration
        mergeEnv(antEnv);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "60c10f80-db3a-47a2-89b5-d7c32a8f63af");
        final AntScript script = new AntScript();
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "55524b5d-c6d5-4d6b-89ea-5525600c7c8e");
        script.setBuildProperties(buildProperties);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "5bec02ba-dfe2-4511-8c85-6fda6b19cd01");
        script.setProperties(properties);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "67b35000-c9aa-4303-a333-0bf32b7ac090");
        script.setLibs(libs);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "466d69b7-e5aa-44ce-865e-226bb1d0cef2");
        script.setListeners(listeners);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "9952af1d-1d20-4cd4-9940-43cc2e87f6cf");
        script.setUseLogger(useLogger);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "9e6b9b06-3756-4d28-afe4-8065ffe5e423");
        script.setUseScript(antScript != null);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "a7e69293-56a7-4983-8c3b-b8d6a457948b");
        script.setWindows(Util.isWindows());
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "3fdcd841-ec20-4002-a3d3-277ee243c014");
        script.setAntScript(antScript);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "f6f42ed1-e33d-4f4f-9e70-22fbdff8338f");
        script.setArgs(args);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "272e432c-23f5-4db7-8785-c6394c02c465");
        script.setBuildFile(buildFile);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "2a00dd24-1073-4f66-a314-3fd1cf8688d3");
        script.setTarget(target);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "a28c1bc2-1a63-4997-b3a2-1787a9011552");
        script.setLoggerClassName(loggerClassName);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "9efc14e1-7023-4733-a83d-34d6cd4eabeb");
        script.setIsLoggerClassNameSet(isLoggerClassNameSet);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "8ff2cc74-3af1-4330-80d2-6c8a2d4eb6d4");
        script.setShowAntOutput(isLiveOutput());
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "0b183b0a-8fbe-4b98-bdee-c2db604d4503");
        script.setTempFileName(tempFileName);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "94158b25-ca89-45d0-beb7-72252d5a2250");
        script.setUseDebug(useDebug);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "8defd3ca-31a3-4bc9-a952-2970497df6d0");
        script.setUseQuiet(useQuiet);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "c6cd57e2-7174-4daa-8b78-8ccf788219a2");
        script.setKeepGoing(keepGoing);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "df06da31-905a-4f1f-a318-0b526f827d18");
        script.setSystemClassPath(getSystemClassPath());
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "8d21dcf5-0710-44d1-8ccf-04882fcafcad");
        script.setPropertyFile(propertyfile);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "05e89472-e3e3-490f-b69e-395ec340a790");
        script.setProgressLoggerLib(progressLoggerLib);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "6f35399b-d8e5-4968-9b58-9cde59c94264");
        script.setProgress(progress);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "347cc0d5-5ff5-4bd2-b6b7-554e7d0cf812");
        script.setAntEnv(antEnv);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "90f680a3-7435-4797-a45e-8b4dba54c78d");
        final File workingDir = antWorkingDir != null ? new File(antWorkingDir) : null;
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "c6722a3c-a3aa-4b21-ae4c-a8908ee964f3");
        final BuildOutputLogger buildOutputConsumer;
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "65fdf1a5-a9e0-4b03-a835-bf446acfca31");
        if (isLiveOutput()) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "001d6d77-6d16-4c16-876a-114ce7b7dc98");
            // TODO: I think there's a bug here when workingDir == null
            buildOutputConsumer = getBuildOutputConsumer(buildProperties.get(Builder.BUILD_PROP_PROJECTNAME), workingDir, AntOutputLogger.DEFAULT_OUTFILE_NAME);
        } else {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "b6155120-8ed7-4828-b6f0-a3894c5ba909");
            buildOutputConsumer = null;
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "dc09d1ce-2c1f-42b1-8e69-4683b8d5e581");
        final boolean scriptCompleted = runScript(script, workingDir, buildOutputConsumer);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "a7650b54-e434-4661-8c4b-f847f22d4008");
        final File logFile = new File(antWorkingDir, tempFileName);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "aa9e330e-b5ce-4497-9c7a-c80e00e2d013");
        final Element buildLogElement;
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "92bafcf2-21f4-47c7-986c-8f2103e9abef");
        if (!scriptCompleted) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "b51609d2-3dab-4dab-a931-a2f3b960fc67");
            LOG.warn("Build timeout timer of " + timeout + " seconds has expired");
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "a9a4b527-5987-4e08-9ba1-41cfebdcd2ca");
            buildLogElement = new Element("build");
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "970d529b-3b83-4811-a859-9057dc4572e0");
            buildLogElement.setAttribute("error", "build timeout");
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "3dcaf02c-bdf2-4173-b7b9-805b6f3c7f55");
            // somebody should really fix ant's XmlLogger
            if (logFile.exists()) {
                writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "057164be-3525-4c74-be3a-889c3466a13b");
                try {
                    writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "b6be67e6-01d1-4e2b-8b1f-d2372d48663c");
                    buildLogElement.setText(Util.readFileToString(logFile));
                } catch (IOException likely) {
                // ignored
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "09c53676-1ad4-4fd8-a2de-ef868b722105");
            // read in log file as element, return it
            buildLogElement = getAntLogAsElement(logFile);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "5656e8e0-ce94-4ca6-821b-e0ab8c5974eb");
            saveAntLog(logFile);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "c9b93abb-444b-443a-9b61-ee16bf3bb805");
            logFile.delete();
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "6849aa47-584f-4a91-8cbd-e0db8127d563");
        return buildLogElement;
    }

    boolean runScript(final AntScript script, final File workingDir, final BuildOutputLogger outputLogger) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "ed1d60be-bb59-4d7f-bfef-500bd456cd37");
        return new ScriptRunner().runScript(workingDir, script, timeout, outputLogger);
    }

    public Element buildWithTarget(final Map<String, String> properties, final String buildTarget, final Progress progress) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "ca47b751-7d8b-4bc5-a83e-f1bad3f4a4a3");
        final String origTarget = target;
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "79fa3925-899e-4583-b1e0-1e5c6fc9a6f3");
        try {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "e0f365d7-f38c-4d78-9146-0c55c37595e2");
            target = buildTarget;
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "0cc87dd2-9e80-4d3a-bbcc-77a3f2ca86e7");
            return build(properties, progress);
        } finally {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "d84b1a18-a7df-4a05-83bd-5d13bd324b53");
            target = origTarget;
        }
    }

    void validateBuildFileExists() throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "63c24aef-3eed-4113-8038-4484e9db609a");
        File build = new File(buildFile);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "103a05af-73b2-4afe-a765-eed2db43af9b");
        if (!build.exists() && !build.isAbsolute() && antWorkingDir != null) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "4572fa03-bb1d-4f1d-86c8-96d3d094063f");
            build = new File(antWorkingDir, buildFile);
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "35dbb917-9178-4f11-9d22-33a61df7364a");
        ValidationHelper.assertExists(build, "buildfile", this.getClass());
    }

    @Description("If supplied, a copy of the ant log will be saved in the specified " + "local directory. Example: saveLogDir=\"/usr/local/dev/projects/cc/logs\".")
    @Optional
    public void setSaveLogDir(String dir) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "c788e557-3548-44f8-8ada-4768b063aeb2");
        saveLogDir = null;
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "f689042d-d677-4321-992a-5fa4d932946e");
        if (dir != null && !dir.trim().equals("")) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "fa61368f-855c-46e5-84f1-6e97f7ac9a10");
            saveLogDir = new File(dir.trim());
        }
    }

    void saveAntLog(File logFile) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "f091b1c6-5fff-4567-87be-5b82d6034367");
        if (saveLogDir == null) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "a537b47e-7c98-435c-885f-69898f7d4a5b");
            return;
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "7f8f8f1d-62ee-4ceb-89aa-742d469e290e");
        try {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "78cfedb6-bfa7-466e-a3d4-1758c48167c0");
            final File newAntLogFile = new File(saveLogDir, tempFileName);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "562073ce-a6a4-430f-b0e6-d7011dc397c5");
            newAntLogFile.createNewFile();
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "00a5b1e5-4655-4a1a-bdc9-ed61e8a1b0d7");
            final FileInputStream in = new FileInputStream(logFile);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "da7b4d63-baff-4a19-9e6e-c6683f0ae845");
            try {
                writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "4de10c65-fb7b-4190-9f67-2e6352fb57e1");
                final FileOutputStream out = new FileOutputStream(newAntLogFile);
                writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "0e9267e9-f083-4a20-991b-0e46029a293b");
                try {
                    writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "5314a303-39b3-4ab7-bab9-ea6da91752fd");
                    byte[] buf = new byte[1024];
                    writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "0b763ff3-e20c-4da0-b8a3-f4c5887a4e9c");
                    int len;
                    writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "09054a7a-870c-401d-b00e-c5cfd1c5b905");
                    while ((len = in.read(buf)) > 0) {
                        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "1d3b948c-58eb-4bb1-b2c2-28b22817def3");
                        out.write(buf, 0, len);
                    }
                } finally {
                    writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "e48f8ec6-7cc5-4611-8c80-297170e7c7b8");
                    out.close();
                }
            } finally {
                writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "07eeb698-debd-41f1-a508-5e730ee44a58");
                in.close();
            }
        } catch (IOException ioe) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "981b00d9-c9f8-4cff-b021-8dabb21234c5");
            LOG.error(ioe);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "5c00a077-9c37-467a-99b0-fd0696c52688");
            LOG.error("Unable to create file: " + new File(saveLogDir, tempFileName));
        }
    }

    @Description("Will invoke ANT in the specified directory. This directory can be " + "absolute or relative to the cruisecontrol working directory.")
    @Optional
    public void setAntWorkingDir(String dir) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "171b8ca5-fc56-4cf9-8220-e8a88af851d5");
        antWorkingDir = dir;
    }

    @Description("Absolute filename of script (shell script or bat file) used to start Ant. " + "You can use this to make CruiseControl use your own Ant installation. " + "If this is not specified, the AntBuilder uses the Ant distribution that " + "ships with CruiseControl. See below for <a href=\"#ant-examples\">examples" + "</a>.")
    @Optional("Recommended, however. Cannot be specified if anthome attribute " + "is also specified")
    public void setAntScript(String antScript) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "3b855d41-066d-4a8e-8e69-bfebd98ed760");
        this.antScript = antScript;
    }

    @Description("Directory in which Ant is installed. CruiseControl will attempt to use the " + "standard Ant execution scripts (i.e. ant.bat or ant). See below for " + "<a href=\"#ant-examples\">examples</a>.")
    @Optional("Cannot be specified if antscript attribute is also specified.")
    public void setAntHome(String antHome) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "76ba8b1b-cebb-479a-9f9e-ba437819c21b");
        this.antHome = antHome;
    }

    /**
     * @param isWindows if true, running under windows
     * @return If the anthome attribute is set, then this method returns the correct shell script
     * to use for a specific environment.
     * @throws CruiseControlException if <code>antHome</code> is not set
     */
    protected String findAntScript(boolean isWindows) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "504dd6cf-ef6d-4aec-8b1d-c5129a99f747");
        if (antHome == null) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "d0c81672-6322-45fc-b073-9df6188da716");
            throw new CruiseControlException("anthome attribute not set.");
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "f75dd4ae-1dfb-4a53-a314-5fc3887ba793");
        if (isWindows) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "965219e5-8989-4355-b6e6-49751a71ff97");
            return antHome + "\\bin\\ant.bat";
        } else {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "6087c236-43ae-4971-b4ea-f89dc4bb4e00");
            return antHome + "/bin/ant";
        }
    }

    @Description("Name of temp file used to capture output.")
    @Optional
    @Default("log.xml")
    public void setTempFile(String tempFileName) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "cecad5e5-4a2b-4447-b460-5b56263efff3");
        this.tempFileName = tempFileName;
    }

    @Description("Ant target(s) to run. Default is \"\", or the default target for " + "the build file.")
    @Optional
    public void setTarget(String target) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "767f553c-cc75-4c14-b0c0-374de5eb4570");
        this.target = target;
    }

    @Description("Path to Ant build file.")
    @Optional
    @Default("build.xml")
    public void setBuildFile(String buildFile) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "4de3bb09-86fd-430b-995d-25e07a1b2f5c");
        this.buildFile = buildFile;
    }

    @Description("'true' if CruiseControl should call Ant using -logger; 'false' to call Ant " + "using '-listener', thus using the loggerclass as a Listener. uselogger=" + "\"true\" will make Ant log its messages using the class specified by " + "loggerclassname as an Ant Logger, which can make for smaller log files since " + "it doesn't log DEBUG messages (see useDebug and useQuiet attributes below, " + "and the <a href=\"http://ant.apache.org/manual/listeners.html\">Ant manual</a>). " + "Set to false to have Ant echo ant messages to console " + "using its DefaultLogger, which is useful when debugging your ant build. " + "Defaults to 'false' to make initial setup easier but setting it to 'true' is " + "recommended for production situations." + "<br/><br/>" + "RE: liveOutput: If liveOutput=true AND uselogger=true, this builder will write " + "the ant output to a file (antBuilderOutput.log) that can be read by the " + "Dashboard reporting application. The liveOutput setting has no effect if " + "uselogger=false. <a href=\"#antbootstrapper\">AntBootstrapper</a> and " + "<a href=\"#antpublisher\">AntPublisher</a> do not provide access to " + "liveOutput, and operate as if liveOutput=false. NOTE: In order to show ant " + "output while uselogger=true, the AntBuilder uses a custom Build Listener. If " + "this interferes with your Ant build, set liveOutput=false (and please report " + "the problem)")
    @Optional
    public void setUseLogger(boolean useLogger) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "8fbdeeff-1cf9-4802-999d-ab7692c7da72");
        this.useLogger = useLogger;
    }

    /**
     * Sets whether Ant will use the custom AntOutputLogger as a listener in order to show live output.
     * @param showAntOutput if true, add AntOutputLogger as a listener.
     * @deprecated Use {@link #setLiveOutput(boolean)} instead.
     */
    @SkipDoc
    public void setShowAntOutput(final boolean showAntOutput) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "a031a1b3-1b4f-4188-8e5c-88c1d0cfc861");
        setLiveOutput(showAntOutput);
    }

    /**
     * @return true if Ant will use the custom AntOutputLogger as a listener in order to show live output.
     * @deprecated Use {@link #isLiveOutput()} instead.
     */
    boolean getShowAntOutput() {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "605666f5-98c5-4947-80e4-f8a75545d427");
        return isLiveOutput();
    }

    /**
     * @param showAntOutput if false, disables Dashboard AntOutputLogger
     * @param useLogger if false, disables Dashboard AntOutputLogger
     * @return true if the jar containing the custom Dashboard logger class must be added to the command line used
     * to execute Ant.
     */
    static boolean shouldAddDashboardLoggerJarToCommandLine(final boolean showAntOutput, final boolean useLogger) {
        writelineStatic("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "d73bc2ab-4131-488a-88c3-2e7fe22d06af");
        return showAntOutput && useLogger;
    }

    @Description("Pass specified argument to the jvm used to invoke ant." + "Ignored if using anthome or antscript. The element has a single required" + "attribute: \"arg\".<br />" + "<strong>Example:</strong> <code>&lt;jvmarg arg=\"-Xmx120m\"/&gt;</code>")
    @Cardinality(min = 0, max = -1)
    public JVMArg createJVMArg() {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "5c1068c7-951e-449a-93d3-c5977323c184");
        final JVMArg arg = new JVMArg();
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "e3a493db-8356-40e6-938c-00f8273588ab");
        args.add(arg);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "0e5a329a-efbf-476f-806f-31782a37c26e");
        return arg;
    }

    @Description("Used to define additional <a " + "href=\"http://ant.apache.org/manual/running.html#libs\">library directories</a> " + "for the ant build. The element has one required attribute: \"searchPath\".<br /> " + "<strong>Example:</strong> <code>&lt;lib searchPath=\"/home/me/myantextensions\"/" + "&gt;</code>")
    @Cardinality(min = 0, max = -1)
    public Lib createLib() {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "c2f99e82-3fe6-497a-beac-d6a646f729c2");
        final Lib lib = new Lib();
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "e2eb07b4-039a-44d7-8dbc-0182dd98b710");
        libs.add(lib);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "47bd4a6e-a5ff-4def-b838-971ce3866a21");
        return lib;
    }

    @Description("Used to define additional <a " + "href=\"http://ant.apache.org/manual/listeners.html\">listeners</a> for the " + "ant build. The element has one required attribute: \"classname\".<br />" + "<strong>Example:</strong> <code>&lt;listener classname=\"org.apache.tools." + "ant.listener.Log4jListener\"/&gt;</code>")
    @Cardinality(min = 0, max = -1)
    public Listener createListener() {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "fdce73a9-9235-424f-9ed2-123963120434");
        final Listener listener = new Listener();
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "da59500f-52f3-4339-90bc-449f925d44bc");
        listeners.add(listener);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "34877c26-e149-4e8f-8507-c2db2b40d551");
        return listener;
    }

    @Description("Used to define properties for the ant build. The element has two " + "required attributes: \"name\" and \"value\". These will be passed on the " + "ant command-line as \"-Dname=value\"<br />" + "<strong>Example:</strong> <code>&lt;property name=\"foo\" value=\"bar\"/" + "&gt;</code>")
    @Cardinality(min = 0, max = -1)
    public Property createProperty() {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "0390f245-28ee-492b-ad68-c4f0a63a48a6");
        final Property property = new Property();
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "8af69b16-8b51-4cc9-94d6-462f17785834");
        properties.add(property);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "e413de69-6747-454b-b394-f7b4c3ab960e");
        return property;
    }

    protected String getSystemClassPath() {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "7d01cf35-6b28-4f41-b0d4-619266e549eb");
        return System.getProperty("java.class.path");
    }

    protected Element getAntLogAsElement(File file) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "16ca1745-256b-4872-be7a-2863cf324950");
        if (!file.exists()) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "37bc10c7-a166-4d6c-ad05-47f7feac6ddb");
            throw new CruiseControlException("ant logfile " + file.getAbsolutePath() + " does not exist.");
        } else if (file.length() == 0) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "eeeb68c3-5fbe-428d-8e3c-48578fcb8f67");
            throw new CruiseControlException("ant logfile " + file.getAbsolutePath() + " is empty. Your build probably failed. Check your CruiseControl logs.");
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "535bfd45-e854-4474-8aa7-ad58f45c99e7");
        try {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "8089e5df-1d9b-471c-b980-64351ab70649");
            SAXBuilder builder = new SAXBuilder("org.apache.xerces.parsers.SAXParser");
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "24b19ef1-d9d8-412f-a2d3-51cbede0acfc");
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
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "1ec1917e-52af-4d0c-91dd-2adad9999da2");
            // get rid of empty <task>- and <message>-elements created by Ant's XmlLogger
            XMLFilter emptyTaskFilter = new EmptyElementFilter("task");
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "6f605c43-d418-4a57-975c-542137e8b520");
            emptyTaskFilter.setParent(piFilter);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "8fcd59ec-35c3-4dc7-a1ae-b1c6eee7b338");
            XMLFilter emptyMessageFilter = new EmptyElementFilter("message");
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "c6cb1c46-7386-4ae2-ad19-50c80e4fc8a5");
            emptyMessageFilter.setParent(emptyTaskFilter);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "ba945878-b609-4a37-abbe-cda87b7ddc84");
            builder.setXMLFilter(emptyMessageFilter);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "850c225c-7c21-4418-aa2c-397f08a8cbc3");
            return builder.build(file).getRootElement();
        } catch (Exception ee) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "09657d20-ca6b-44ff-9084-e45287c059f8");
            if (ee instanceof CruiseControlException) {
                writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "6e2d3e38-578f-4d68-aa67-1b24d4658c5c");
                throw (CruiseControlException) ee;
            }
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "91c0ce60-2c95-4e58-a103-eb891cbce36d");
            File saveFile = new File(file.getParentFile(), System.currentTimeMillis() + file.getName());
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "4aa929d6-ee20-4e30-802f-cd9ac4b7fb35");
            file.renameTo(saveFile);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "8e9759a5-6982-486c-815e-7e47df1fc232");
            throw new CruiseControlException("Error reading : " + file.getAbsolutePath() + ".  Saved as : " + saveFile.getAbsolutePath(), ee);
        }
    }

    @Description("If true will invoke ant with -debug, which can be useful for debugging your " + "ant build. Defaults to 'false', cannot be set to 'true' if usequiet is " + "also set to 'true'. When used in combination with uselogger=\"true\", " + "this will result in bigger XML log files; otherwise, it will cause more " + "output to be written to the console by Ant's DefaultLogger.")
    @Optional
    public void setUseDebug(boolean debug) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "1dfef2f9-5987-4a43-ab85-50c9c6ede2ca");
        useDebug = debug;
    }

    @Description("If true will invoke ant with -quiet, which can be useful for creating smaller " + "log files since messages with a priority of INFO will not be logged. Defaults " + "to 'false', cannot be set to 'true' if usedebug is also set to 'true'. " + "Smaller logfiles are only achieved when used in combination with uselogger=" + "\"true\", otherwise there will just be less output echoed to the console by " + "Ant's DefaultLogger." + "<br/><br/>" + "RE: showProgress: useQuiet=\"true\" will prevent any progress messages from " + "being displayed. NOTE: In order to show progress, the AntBuilder uses custom " + "Build Loggers and Listeners. If these interfere with your Ant build, set " + "showProgress=false (and please report the problem).")
    @Optional
    public void setUseQuiet(boolean quiet) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "3d2fc1a9-37d3-4d9f-96fc-23d41c927305");
        useQuiet = quiet;
    }

    @Description("If true will invoke ant with -keep-going, which can be useful for performing " + "build steps after an optional step fails. Defaults to 'false'.")
    @Optional
    public void setKeepGoing(boolean keepGoing) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "5c265ef1-377c-4d37-94e5-3c7846e12d18");
        this.keepGoing = keepGoing;
    }

    public String getLoggerClassName() {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "b3faa3c9-875d-4697-b4d7-9161cdf7f7e2");
        return loggerClassName;
    }

    @Description("If you want to use another logger (or listener, when uselogger=\"false\") than " + "Ant's XmlLogger, you can specify the classname of the logger here. The logger " + "needs to output compatible XML, and the class needs to be available on the " + "classpath at buildtime.")
    @Optional
    @Default("org.apache.tools.ant.XmlLogger")
    public void setLoggerClassName(String string) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "666f9e6a-8641-40b0-b7da-f6d2c327822a");
        loggerClassName = string;
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "56652403-d0d0-4dc8-a650-6f198889888f");
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
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "7482cff6-8a50-486a-9a78-4eef748d5b24");
        this.timeout = timeout;
    }

    @Description("Load all properties from file with -D properties (like child <code><a href=\"" + "#antbuilderchildprop\">&lt;property&gt;</a></code> elements) taking " + "precedence. Useful when the propertyfile content can change for every build.")
    @Optional
    public void setPropertyfile(String propertyfile) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "35e22859-e6b7-48a4-90dd-ccdd1bc11381");
        this.propertyfile = propertyfile;
    }

    @Description("Overrides the default -lib search path used to add support for showProgress " + "features in the ant builder. This search path ensures customized ant " + "Loggers/Listeners are available on the classpath of the ant builder VM. You " + "should not normally set this value. If you do set this value, you should " + "use the full path (including filename) to cruisecontrol-antprogresslogger.jar. " + "This setting has no effect if showProgress=false.")
    @Optional
    public void setProgressLoggerLib(String progressLoggerLib) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "07c05658-fb74-4332-bb24-e30e7b379d54");
        this.progressLoggerLib = progressLoggerLib;
    }

    /**
     * @return The path (including filename) to the jar file
     * ({@link AntScript#LIBNAME_PROGRESS_LOGGER cruisecontrol-antprogresslogger.jar})
     * containing the AntProgressLogger/Listener classes.
     */
    public String getProgressLoggerLib() {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_5_10.coverage", "dcae712f-32ea-42dd-8ace-f0680faa7ca6");
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
