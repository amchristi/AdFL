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
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "07b1a5cd-f155-412f-8865-bd950217fd53");
        super.validate();
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "d6d581fd-90d3-4f75-8ddc-e74e385c7c0d");
        ValidationHelper.assertIsSet(buildFile, "buildfile", this.getClass());
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "b2f6f5ea-d362-426e-8e4b-430c5c2daa92");
        ValidationHelper.assertIsSet(target, "target", this.getClass());
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "0212c6e8-adf8-4264-ae0a-fe1c2eb5ebd2");
        ValidationHelper.assertFalse(useDebug && useQuiet, "'useDebug' and 'useQuiet' can't be used together");
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "3d4ab71b-1b88-43ab-ab45-c8c2944e3b71");
        if (!useLogger && (useDebug || useQuiet)) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "9b4a1f38-6fb1-4c19-af43-e56d8ecb03c8");
            LOG.warn("usedebug and usequiet are ignored if uselogger is not set to 'true'!");
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "cbb9f49c-1510-446b-81c6-b593025e0e59");
        if (saveLogDir != null) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "2d9debd9-40a8-4a01-93f3-e8506c52c67c");
            ValidationHelper.assertTrue(saveLogDir.isDirectory(), "'saveLogDir' must exist and be a directory");
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "16b3d540-04fe-45b4-88b4-d50cf418a750");
        ValidationHelper.assertFalse(antScript != null && antHome != null, "'antHome' and 'antscript' cannot both be set");
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "8c65cb08-133c-4fe5-9533-5866bd33053f");
        // Validate showAntOutput
        if (shouldAddDashboardLoggerJarToCommandLine(isLiveOutput(), useLogger)) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "5b822322-13c4-461a-8223-8e7800184c84");
            if (progressLoggerLib == null) {
                writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "9048ed0a-4265-475f-aa50-d8c98b7802d1");
                // since progressLoggerLib is not specified in the config.xml,
                // we must be able to find the path to {@link AntScript#LIBNAME_PROGRESS_LOGGER}
                // to ensure the separate ant VM will have access to the required listeners
                AntScript.findDefaultProgressLoggerLib();
            } else {
                writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "7e817725-4bbd-4f8a-bf2f-8cc6ec1c50eb");
                // config.xml specified progressLoggerLib, so just make sure it exists
                ValidationHelper.assertExists(new File(progressLoggerLib), "progressLoggerLib", this.getClass());
            }
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "849ce686-a0f6-4d2c-a246-b48abc34ab11");
        if (antHome != null) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "7f22f5db-f37f-4da0-b6dc-e4ab829d2873");
            final File antHomeFile = new File(antHome);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "571b96bd-d134-462d-ab77-74e55e129108");
            ValidationHelper.assertTrue(antHomeFile.exists() && antHomeFile.isDirectory(), "'antHome' must exist and be a directory. Expected to find " + antHomeFile.getAbsolutePath());
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "717272ff-4484-4992-8bf6-1e718db84a07");
            final File antScriptInAntHome = new File(findAntScript(Util.isWindows()));
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "881334a1-8d64-4033-918d-c487ad56f066");
            ValidationHelper.assertTrue(antScriptInAntHome.exists() && antScriptInAntHome.isFile(), "'antHome' must contain an ant execution script. Expected to find " + antScriptInAntHome.getAbsolutePath());
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "88f90438-cc24-4251-b0e7-d6086c7f9377");
            antScript = antScriptInAntHome.getAbsolutePath();
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "4d1d46f2-74db-4e9c-80a3-556a225fdeab");
        if (antScript != null && !args.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "dae27c6f-b111-41ed-ade2-847b7f1eecb9");
            LOG.warn("jvmargs will be ignored if you specify anthome or your own antscript!");
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "bbbdb288-3e98-4308-aea6-7224a3d63f49");
        wasValidated = true;
    }

    /**
     * build and return the results via xml.  debug status can be determined
     * from log4j category once we get all the logging in place.
     */
    public Element build(final Map<String, String> buildProperties, final Progress progressIn) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "7930c470-4045-4b12-8449-bc0c4563d0b3");
        if (!wasValidated) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "7b2d93dc-ea63-4aba-910f-633446f837e8");
            throw new IllegalStateException("This builder was never validated." + " The build method should not be getting called.");
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "44b686b7-81c3-48e1-b221-e4ae5571036b");
        validateBuildFileExists();
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "c64b98e6-be2a-4b0a-8f5b-8ee9abaeac6b");
        final Progress progress = getShowProgress() ? progressIn : null;
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "07b49d61-ce17-467e-8527-44daffc1f21f");
        final OSEnvironment antEnv = new OSEnvironment();
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "7b4ad96b-13a1-4098-8987-31264dd91af5");
        // Merge the environment with the configuration
        mergeEnv(antEnv);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "9552786f-ff5c-4f71-a886-d322da19b50f");
        final AntScript script = new AntScript();
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "5376a90f-e3ba-48bc-9141-e13e56f5110b");
        script.setBuildProperties(buildProperties);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "a3331bc0-3c29-45b4-853a-0f239a99c06b");
        script.setProperties(properties);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "cfc43bac-3c8e-403d-89b9-dfd152b539c1");
        script.setLibs(libs);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "56f20255-6d2b-49b8-a507-e9e9682f873f");
        script.setListeners(listeners);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "c96621b9-fcb5-4175-a61c-822aad0caf1a");
        script.setUseLogger(useLogger);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "eb79f741-d6b2-4723-af7d-c2ffbd7a9051");
        script.setUseScript(antScript != null);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "b91d26c5-eda3-42fe-a3ff-e81462c134af");
        script.setWindows(Util.isWindows());
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "eb7fed09-7fdd-4c88-a614-f0163dc06872");
        script.setAntScript(antScript);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "5ca96927-51ff-4f3e-9716-613b39456d0d");
        script.setArgs(args);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "1455ecd7-7d0d-41ab-a48a-5edbd024daff");
        script.setBuildFile(buildFile);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "765002e3-cf37-49e1-bf3d-944ca817ff85");
        script.setTarget(target);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "9c8295ef-c04c-48f2-8f80-d96e8816de69");
        script.setLoggerClassName(loggerClassName);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "282f81a8-0f20-4b6d-a2b3-5f09d7738d1f");
        script.setIsLoggerClassNameSet(isLoggerClassNameSet);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "c221529a-e7c6-4981-ac48-fbcc823ed587");
        script.setShowAntOutput(isLiveOutput());
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "852805a6-037e-43cf-9472-147192d62e3d");
        script.setTempFileName(tempFileName);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "6a33cc71-4400-4b20-9d43-1feba11371ff");
        script.setUseDebug(useDebug);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "1822eab7-b309-49ef-8eff-5e82d875ef0f");
        script.setUseQuiet(useQuiet);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "5b5449da-9922-4838-ac13-145c2db73232");
        script.setKeepGoing(keepGoing);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "5c5726b5-1a25-40ae-a665-476d7443ca2c");
        script.setSystemClassPath(getSystemClassPath());
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "5a29d20b-f7b4-40ed-a869-464aed21f034");
        script.setPropertyFile(propertyfile);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "01688763-8156-4c3e-97c1-2e5e38b38631");
        script.setProgressLoggerLib(progressLoggerLib);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "25b92ef7-fe37-4706-8678-c89488e74cca");
        script.setProgress(progress);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "8b068ff2-1a09-44fc-9512-506978f95a37");
        script.setAntEnv(antEnv);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "bc3a2283-d278-48bc-ad13-d15c4b54d2ca");
        final File workingDir = antWorkingDir != null ? new File(antWorkingDir) : null;
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "1e12a7f3-a51a-4e8a-bac3-60dd640c6060");
        final BuildOutputLogger buildOutputConsumer;
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "fef14fcf-f22f-4f97-a02f-5e21607f4d35");
        if (isLiveOutput()) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "0d1e48cb-5dbb-43e5-9332-56107e00f5ea");
            // TODO: I think there's a bug here when workingDir == null
            buildOutputConsumer = getBuildOutputConsumer(buildProperties.get(Builder.BUILD_PROP_PROJECTNAME), workingDir, AntOutputLogger.DEFAULT_OUTFILE_NAME);
        } else {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "c0cf1762-7a11-490a-837f-c8bfd9ffc1ee");
            buildOutputConsumer = null;
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "9b936122-f992-4316-adf5-89d01a1ac67f");
        final boolean scriptCompleted = runScript(script, workingDir, buildOutputConsumer);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "d0c95554-a7e3-4c4c-ae14-f95590952915");
        final File logFile = new File(antWorkingDir, tempFileName);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "51826cc2-850b-4e06-81c0-c16c41ad17cd");
        final Element buildLogElement;
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "73df9064-68d6-41cb-99d5-843877254eb5");
        if (!scriptCompleted) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "6a162bc5-8b73-4792-a961-da3fd533c153");
            LOG.warn("Build timeout timer of " + timeout + " seconds has expired");
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "eae0de27-db34-471f-aabb-b00c7f362913");
            buildLogElement = new Element("build");
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "7c143ca5-2dbc-48d3-b5d5-6457caae663f");
            buildLogElement.setAttribute("error", "build timeout");
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "c8e88386-19b7-430c-a7d1-f493b76d0b1f");
            // somebody should really fix ant's XmlLogger
            if (logFile.exists()) {
                writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "e969f0fb-926b-4e04-b6a5-b5e408f7d608");
                try {
                    writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "78571cd8-a448-4a58-96ad-0ec3f9374760");
                    buildLogElement.setText(Util.readFileToString(logFile));
                } catch (IOException likely) {
                // ignored
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "4357b927-5497-4616-81e1-428fe39d6e77");
            // read in log file as element, return it
            buildLogElement = getAntLogAsElement(logFile);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "f5442141-5791-4de6-ae90-39f49865a5ed");
            saveAntLog(logFile);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "17337c5e-0f47-4363-bca0-f4d7aff67c20");
            logFile.delete();
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "092d2180-952d-4af1-a213-b0f556e0e488");
        return buildLogElement;
    }

    boolean runScript(final AntScript script, final File workingDir, final BuildOutputLogger outputLogger) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "df744ab0-790c-41ea-9c4b-8eb89c6a1392");
        return new ScriptRunner().runScript(workingDir, script, timeout, outputLogger);
    }

    public Element buildWithTarget(final Map<String, String> properties, final String buildTarget, final Progress progress) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "4edeecd0-d6cd-4272-8daf-5a2b9778fd84");
        final String origTarget = target;
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "023f10cf-522e-4260-9eaf-65e7340b2a74");
        try {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "e56ba088-387e-4b58-835a-42fced057552");
            target = buildTarget;
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "cb8fe28a-42b0-417e-a4fb-f8058c6225fe");
            return build(properties, progress);
        } finally {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "3c7d4e65-2cd5-4992-9e9b-310d0ef06d49");
            target = origTarget;
        }
    }

    void validateBuildFileExists() throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "37d41d69-eb95-4ef5-9c66-6ea2fd0d4243");
        File build = new File(buildFile);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "e6ebdff9-513c-4903-a6db-6cc4aa63994b");
        if (!build.exists() && !build.isAbsolute() && antWorkingDir != null) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "d6873f3a-2d1a-41ed-87d0-98e4f5c38b84");
            build = new File(antWorkingDir, buildFile);
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "d13eefbc-1669-4372-aa5f-f1543a804fd0");
        ValidationHelper.assertExists(build, "buildfile", this.getClass());
    }

    @Description("If supplied, a copy of the ant log will be saved in the specified " + "local directory. Example: saveLogDir=\"/usr/local/dev/projects/cc/logs\".")
    @Optional
    public void setSaveLogDir(String dir) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "1320bba6-327b-4430-82b7-bd152c4f1f49");
        saveLogDir = null;
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "fa22825c-5ae3-4f5a-a0c9-97997ffa031c");
        if (dir != null && !dir.trim().equals("")) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "b5f04df5-f27d-43a6-bc8a-6eeb420492a4");
            saveLogDir = new File(dir.trim());
        }
    }

    void saveAntLog(File logFile) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "405f5bbf-4ca0-4e34-8f61-147e25f574e3");
        if (saveLogDir == null) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "983998c8-32a1-4102-bf15-c956e52b0ea0");
            return;
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "4dd77b18-74fc-429d-8d0c-bf54e25bf2ca");
        try {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "16433c73-7687-4bf5-84ec-2f2744e13f20");
            final File newAntLogFile = new File(saveLogDir, tempFileName);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "af5d18b5-b463-46db-9422-080472c51f90");
            newAntLogFile.createNewFile();
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "192af802-b863-4d68-ba0d-dd540480c6d1");
            final FileInputStream in = new FileInputStream(logFile);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "aeb0fd64-6496-4f6c-8396-0fb1769a17a5");
            try {
                writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "c6af7310-d794-4e1a-b751-772b4d4ce3a4");
                final FileOutputStream out = new FileOutputStream(newAntLogFile);
                writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "76a72146-0b6a-4ddc-b7b3-9de002585623");
                try {
                    writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "57d50ee1-5d72-4056-944b-11303085aa09");
                    byte[] buf = new byte[1024];
                    writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "efa640b0-4c4f-4a93-a508-3f9533acdbd1");
                    int len;
                    writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "9869fef7-e7b4-4c5d-8682-ceea614377ba");
                    while ((len = in.read(buf)) > 0) {
                        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "d168d0d6-f8c3-4149-a7f2-dbf4491d779d");
                        out.write(buf, 0, len);
                    }
                } finally {
                    writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "c80544ac-0075-4496-a75d-2240a64f3cd8");
                    out.close();
                }
            } finally {
                writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "0cdd85c1-7e37-42dc-b818-3be44c886987");
                in.close();
            }
        } catch (IOException ioe) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "cc98a26f-b056-42ec-9c5f-0cf7c4f88f75");
            LOG.error(ioe);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "1391126b-87a8-46f6-b66c-d547ca104e72");
            LOG.error("Unable to create file: " + new File(saveLogDir, tempFileName));
        }
    }

    @Description("Will invoke ANT in the specified directory. This directory can be " + "absolute or relative to the cruisecontrol working directory.")
    @Optional
    public void setAntWorkingDir(String dir) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "64eeec1d-8f8a-42e8-9a59-7182bcc75c64");
        antWorkingDir = dir;
    }

    @Description("Absolute filename of script (shell script or bat file) used to start Ant. " + "You can use this to make CruiseControl use your own Ant installation. " + "If this is not specified, the AntBuilder uses the Ant distribution that " + "ships with CruiseControl. See below for <a href=\"#ant-examples\">examples" + "</a>.")
    @Optional("Recommended, however. Cannot be specified if anthome attribute " + "is also specified")
    public void setAntScript(String antScript) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "43b2c79f-ad6d-4d0f-925c-dc0c2d6411c1");
        this.antScript = antScript;
    }

    @Description("Directory in which Ant is installed. CruiseControl will attempt to use the " + "standard Ant execution scripts (i.e. ant.bat or ant). See below for " + "<a href=\"#ant-examples\">examples</a>.")
    @Optional("Cannot be specified if antscript attribute is also specified.")
    public void setAntHome(String antHome) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "c33e3197-0da6-4e70-813f-711a635c0cd7");
        this.antHome = antHome;
    }

    /**
     * @param isWindows if true, running under windows
     * @return If the anthome attribute is set, then this method returns the correct shell script
     * to use for a specific environment.
     * @throws CruiseControlException if <code>antHome</code> is not set
     */
    protected String findAntScript(boolean isWindows) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "90696c94-d64d-48bf-819e-a496c4e30090");
        if (antHome == null) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "d7fae61c-b20c-4330-ab24-d4a76223d5b6");
            throw new CruiseControlException("anthome attribute not set.");
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "dccc4ecc-c815-4ef1-9dba-3371483beab6");
        if (isWindows) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "461c7553-c726-48d4-b002-6145ae07540b");
            return antHome + "\\bin\\ant.bat";
        } else {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "4db459ac-880d-4c66-be70-32011f30b2d9");
            return antHome + "/bin/ant";
        }
    }

    @Description("Name of temp file used to capture output.")
    @Optional
    @Default("log.xml")
    public void setTempFile(String tempFileName) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "f93ab3bd-204a-4ef1-a5bb-9d6e024fedea");
        this.tempFileName = tempFileName;
    }

    @Description("Ant target(s) to run. Default is \"\", or the default target for " + "the build file.")
    @Optional
    public void setTarget(String target) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "a7d641cf-e15b-491d-88fe-d07c05fe8529");
        this.target = target;
    }

    @Description("Path to Ant build file.")
    @Optional
    @Default("build.xml")
    public void setBuildFile(String buildFile) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "b487f494-1b4e-4bc0-8553-eaefe2ec9de3");
        this.buildFile = buildFile;
    }

    @Description("'true' if CruiseControl should call Ant using -logger; 'false' to call Ant " + "using '-listener', thus using the loggerclass as a Listener. uselogger=" + "\"true\" will make Ant log its messages using the class specified by " + "loggerclassname as an Ant Logger, which can make for smaller log files since " + "it doesn't log DEBUG messages (see useDebug and useQuiet attributes below, " + "and the <a href=\"http://ant.apache.org/manual/listeners.html\">Ant manual</a>). " + "Set to false to have Ant echo ant messages to console " + "using its DefaultLogger, which is useful when debugging your ant build. " + "Defaults to 'false' to make initial setup easier but setting it to 'true' is " + "recommended for production situations." + "<br/><br/>" + "RE: liveOutput: If liveOutput=true AND uselogger=true, this builder will write " + "the ant output to a file (antBuilderOutput.log) that can be read by the " + "Dashboard reporting application. The liveOutput setting has no effect if " + "uselogger=false. <a href=\"#antbootstrapper\">AntBootstrapper</a> and " + "<a href=\"#antpublisher\">AntPublisher</a> do not provide access to " + "liveOutput, and operate as if liveOutput=false. NOTE: In order to show ant " + "output while uselogger=true, the AntBuilder uses a custom Build Listener. If " + "this interferes with your Ant build, set liveOutput=false (and please report " + "the problem)")
    @Optional
    public void setUseLogger(boolean useLogger) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "c1ea26d4-d9e1-4e42-bca3-97a4f41149c0");
        this.useLogger = useLogger;
    }

    /**
     * Sets whether Ant will use the custom AntOutputLogger as a listener in order to show live output.
     * @param showAntOutput if true, add AntOutputLogger as a listener.
     * @deprecated Use {@link #setLiveOutput(boolean)} instead.
     */
    @SkipDoc
    public void setShowAntOutput(final boolean showAntOutput) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "7b97036a-626f-4aa8-803e-34859868b053");
        setLiveOutput(showAntOutput);
    }

    /**
     * @return true if Ant will use the custom AntOutputLogger as a listener in order to show live output.
     * @deprecated Use {@link #isLiveOutput()} instead.
     */
    boolean getShowAntOutput() {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "3f2ee11e-cd7b-4fa9-9fe0-6fe76fc8230d");
        return isLiveOutput();
    }

    /**
     * @param showAntOutput if false, disables Dashboard AntOutputLogger
     * @param useLogger if false, disables Dashboard AntOutputLogger
     * @return true if the jar containing the custom Dashboard logger class must be added to the command line used
     * to execute Ant.
     */
    static boolean shouldAddDashboardLoggerJarToCommandLine(final boolean showAntOutput, final boolean useLogger) {
        writelineStatic("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "36275779-2617-4b60-8710-cb2d4e491ea3");
        return showAntOutput && useLogger;
    }

    @Description("Pass specified argument to the jvm used to invoke ant." + "Ignored if using anthome or antscript. The element has a single required" + "attribute: \"arg\".<br />" + "<strong>Example:</strong> <code>&lt;jvmarg arg=\"-Xmx120m\"/&gt;</code>")
    @Cardinality(min = 0, max = -1)
    public JVMArg createJVMArg() {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "c117912b-75c7-4338-9cdb-ed0571fa00a1");
        final JVMArg arg = new JVMArg();
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "e40595d6-ce99-4c8a-8e22-7957701c8a88");
        args.add(arg);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "7f093162-93c6-447d-ba67-6c6f0e6581ff");
        return arg;
    }

    @Description("Used to define additional <a " + "href=\"http://ant.apache.org/manual/running.html#libs\">library directories</a> " + "for the ant build. The element has one required attribute: \"searchPath\".<br /> " + "<strong>Example:</strong> <code>&lt;lib searchPath=\"/home/me/myantextensions\"/" + "&gt;</code>")
    @Cardinality(min = 0, max = -1)
    public Lib createLib() {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "18d9223d-c01e-48df-bfbe-e5ab8118117b");
        final Lib lib = new Lib();
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "19903215-b3b0-44bc-addb-b687579feac6");
        libs.add(lib);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "08e71d13-85a2-4f70-9bbd-dd915d0448fa");
        return lib;
    }

    @Description("Used to define additional <a " + "href=\"http://ant.apache.org/manual/listeners.html\">listeners</a> for the " + "ant build. The element has one required attribute: \"classname\".<br />" + "<strong>Example:</strong> <code>&lt;listener classname=\"org.apache.tools." + "ant.listener.Log4jListener\"/&gt;</code>")
    @Cardinality(min = 0, max = -1)
    public Listener createListener() {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "9d5e65d8-f715-4587-9900-68f559c14073");
        final Listener listener = new Listener();
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "457fe38c-9f95-4003-a268-600b90ef1c3e");
        listeners.add(listener);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "929b29b9-13b2-47dd-bf4d-91160ba32ee3");
        return listener;
    }

    @Description("Used to define properties for the ant build. The element has two " + "required attributes: \"name\" and \"value\". These will be passed on the " + "ant command-line as \"-Dname=value\"<br />" + "<strong>Example:</strong> <code>&lt;property name=\"foo\" value=\"bar\"/" + "&gt;</code>")
    @Cardinality(min = 0, max = -1)
    public Property createProperty() {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "7d6ccd42-7ae5-4701-bf3c-19dbeb10f957");
        final Property property = new Property();
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "ab655819-bacb-4e4c-a415-4b8d2782ea07");
        properties.add(property);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "d05489a7-4352-4989-b7a5-d38ef5f50ab3");
        return property;
    }

    protected String getSystemClassPath() {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "a79bc6da-7d96-4b1f-9faa-e027bc92fba2");
        return System.getProperty("java.class.path");
    }

    protected Element getAntLogAsElement(File file) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "1744534e-ac6b-4f7f-8e44-fe90993e586d");
        if (!file.exists()) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "fe0cd2e9-6de6-4ae5-853a-5eab2724a79c");
            throw new CruiseControlException("ant logfile " + file.getAbsolutePath() + " does not exist.");
        } else if (file.length() == 0) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "ed4b64a9-8dc0-4d21-bce0-dcaeb8e850d8");
            throw new CruiseControlException("ant logfile " + file.getAbsolutePath() + " is empty. Your build probably failed. Check your CruiseControl logs.");
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "01baf342-678e-4aa5-a8dc-84dcdc3abe44");
        try {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "be3aa188-1d0e-4d25-a5e0-d3172187aeca");
            SAXBuilder builder = new SAXBuilder("org.apache.xerces.parsers.SAXParser");
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "d7d0b20a-ebe2-4bb4-b9a4-e5b3d1bf701c");
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
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "02ac6510-1b3a-46a4-942e-ffcc66b3719c");
            // get rid of empty <task>- and <message>-elements created by Ant's XmlLogger
            XMLFilter emptyTaskFilter = new EmptyElementFilter("task");
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "7ea38224-33d8-4f63-8d09-b2b4d86625cf");
            emptyTaskFilter.setParent(piFilter);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "969ba312-b7ee-4313-aef7-5fcac86c9ecc");
            XMLFilter emptyMessageFilter = new EmptyElementFilter("message");
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "23967f09-808a-4436-bf7a-bf6b52093817");
            emptyMessageFilter.setParent(emptyTaskFilter);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "32ba49f4-c672-4a21-93cd-37ddb95960da");
            builder.setXMLFilter(emptyMessageFilter);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "f47d62dc-7c87-44eb-b28e-af59c52073cb");
            return builder.build(file).getRootElement();
        } catch (Exception ee) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "8a261265-64be-4ea5-925c-f02407999d68");
            if (ee instanceof CruiseControlException) {
                writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "714b7da5-dc0e-4075-8090-8587a6523630");
                throw (CruiseControlException) ee;
            }
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "3ad7cea6-3d2a-454e-9d2d-9c141820dd7f");
            File saveFile = new File(file.getParentFile(), System.currentTimeMillis() + file.getName());
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "45a37043-adfa-455f-84bc-d74c24c936d7");
            file.renameTo(saveFile);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "968b5b40-a152-4ade-9d81-3c9f387ac259");
            throw new CruiseControlException("Error reading : " + file.getAbsolutePath() + ".  Saved as : " + saveFile.getAbsolutePath(), ee);
        }
    }

    @Description("If true will invoke ant with -debug, which can be useful for debugging your " + "ant build. Defaults to 'false', cannot be set to 'true' if usequiet is " + "also set to 'true'. When used in combination with uselogger=\"true\", " + "this will result in bigger XML log files; otherwise, it will cause more " + "output to be written to the console by Ant's DefaultLogger.")
    @Optional
    public void setUseDebug(boolean debug) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "616204ff-ff7b-4277-8d2a-3a2be9dcf9a6");
        useDebug = debug;
    }

    @Description("If true will invoke ant with -quiet, which can be useful for creating smaller " + "log files since messages with a priority of INFO will not be logged. Defaults " + "to 'false', cannot be set to 'true' if usedebug is also set to 'true'. " + "Smaller logfiles are only achieved when used in combination with uselogger=" + "\"true\", otherwise there will just be less output echoed to the console by " + "Ant's DefaultLogger." + "<br/><br/>" + "RE: showProgress: useQuiet=\"true\" will prevent any progress messages from " + "being displayed. NOTE: In order to show progress, the AntBuilder uses custom " + "Build Loggers and Listeners. If these interfere with your Ant build, set " + "showProgress=false (and please report the problem).")
    @Optional
    public void setUseQuiet(boolean quiet) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "e708cd50-e5f8-41b8-b307-4e12db9f5405");
        useQuiet = quiet;
    }

    @Description("If true will invoke ant with -keep-going, which can be useful for performing " + "build steps after an optional step fails. Defaults to 'false'.")
    @Optional
    public void setKeepGoing(boolean keepGoing) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "09294acd-ecdd-4302-8a5c-d26bda3d2e50");
        this.keepGoing = keepGoing;
    }

    public String getLoggerClassName() {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "eec82292-373b-48c5-b195-6691bb61e25d");
        return loggerClassName;
    }

    @Description("If you want to use another logger (or listener, when uselogger=\"false\") than " + "Ant's XmlLogger, you can specify the classname of the logger here. The logger " + "needs to output compatible XML, and the class needs to be available on the " + "classpath at buildtime.")
    @Optional
    @Default("org.apache.tools.ant.XmlLogger")
    public void setLoggerClassName(String string) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "4f188862-54a8-44da-afaa-4d399dc646ca");
        loggerClassName = string;
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "ffa31435-d5bb-4f49-9d1a-e41b726c027a");
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
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "ed14cf9b-ca45-451c-8646-8f02d1941b28");
        this.timeout = timeout;
    }

    @Description("Load all properties from file with -D properties (like child <code><a href=\"" + "#antbuilderchildprop\">&lt;property&gt;</a></code> elements) taking " + "precedence. Useful when the propertyfile content can change for every build.")
    @Optional
    public void setPropertyfile(String propertyfile) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "cdbad0ae-f863-471a-a5cc-d8dbb0f88968");
        this.propertyfile = propertyfile;
    }

    @Description("Overrides the default -lib search path used to add support for showProgress " + "features in the ant builder. This search path ensures customized ant " + "Loggers/Listeners are available on the classpath of the ant builder VM. You " + "should not normally set this value. If you do set this value, you should " + "use the full path (including filename) to cruisecontrol-antprogresslogger.jar. " + "This setting has no effect if showProgress=false.")
    @Optional
    public void setProgressLoggerLib(String progressLoggerLib) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "72d917a0-3f8d-4a00-8347-bced1f91008c");
        this.progressLoggerLib = progressLoggerLib;
    }

    /**
     * @return The path (including filename) to the jar file
     * ({@link AntScript#LIBNAME_PROGRESS_LOGGER cruisecontrol-antprogresslogger.jar})
     * containing the AntProgressLogger/Listener classes.
     */
    public String getProgressLoggerLib() {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_2_10.coverage", "5c5e1f93-2465-4df2-850d-82f90cef5810");
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
