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
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "743e64d0-40bf-479a-9faf-5125a3bc52b9");
        super.validate();
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "f2f108b5-8bc7-4b85-85b9-890559fb2e00");
        ValidationHelper.assertIsSet(buildFile, "buildfile", this.getClass());
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "706317eb-4b39-43ec-abd2-8dae420cb499");
        ValidationHelper.assertIsSet(target, "target", this.getClass());
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "7f76682b-453f-4bd4-b888-86f8eeb186a4");
        ValidationHelper.assertFalse(useDebug && useQuiet, "'useDebug' and 'useQuiet' can't be used together");
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "6f65867d-a7e4-4158-86ff-b758e1559884");
        if (!useLogger && (useDebug || useQuiet)) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "a05094e2-bf09-41ce-b5a9-e0fff386c5de");
            LOG.warn("usedebug and usequiet are ignored if uselogger is not set to 'true'!");
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "da6b30a0-2236-459c-a919-6bf7f1baf893");
        if (saveLogDir != null) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "9498d7b9-fcf8-4823-b087-c93565636535");
            ValidationHelper.assertTrue(saveLogDir.isDirectory(), "'saveLogDir' must exist and be a directory");
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "2e80c2b1-f0d8-4b9d-bb80-f0d8c922204b");
        ValidationHelper.assertFalse(antScript != null && antHome != null, "'antHome' and 'antscript' cannot both be set");
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "52051128-8130-4c66-9c3d-bee9fc0b039b");
        // Validate showAntOutput
        if (shouldAddDashboardLoggerJarToCommandLine(isLiveOutput(), useLogger)) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "abe71d6a-a875-4335-82dd-8fdcc9a58483");
            if (progressLoggerLib == null) {
                writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "0affdb9d-abc9-455b-8a3a-b48622c0f677");
                // since progressLoggerLib is not specified in the config.xml,
                // we must be able to find the path to {@link AntScript#LIBNAME_PROGRESS_LOGGER}
                // to ensure the separate ant VM will have access to the required listeners
                AntScript.findDefaultProgressLoggerLib();
            } else {
                writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "a085d535-9336-452c-b1b9-62cb56c5dbad");
                // config.xml specified progressLoggerLib, so just make sure it exists
                ValidationHelper.assertExists(new File(progressLoggerLib), "progressLoggerLib", this.getClass());
            }
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "03e7a975-acd8-4ddb-bdde-216790dec4f1");
        if (antHome != null) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "ef11d807-b77b-4199-99a3-7d578853fd09");
            final File antHomeFile = new File(antHome);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "43df42ed-5367-4f1f-8b86-cdf546efd004");
            ValidationHelper.assertTrue(antHomeFile.exists() && antHomeFile.isDirectory(), "'antHome' must exist and be a directory. Expected to find " + antHomeFile.getAbsolutePath());
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "c3510a0d-9390-4216-93d4-6c177583f96c");
            final File antScriptInAntHome = new File(findAntScript(Util.isWindows()));
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "d1d6d1c2-506a-4888-ba30-1c588ffde636");
            ValidationHelper.assertTrue(antScriptInAntHome.exists() && antScriptInAntHome.isFile(), "'antHome' must contain an ant execution script. Expected to find " + antScriptInAntHome.getAbsolutePath());
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "f1fcdf85-69e1-4823-a824-82a4a850e30f");
            antScript = antScriptInAntHome.getAbsolutePath();
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "21bd4f65-1301-4191-9752-d8fc38e6a58b");
        if (antScript != null && !args.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "c51467f2-e18e-4e98-93c8-764e818ad5a5");
            LOG.warn("jvmargs will be ignored if you specify anthome or your own antscript!");
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "becda8c7-e563-41dc-87a6-7f0b79988c77");
        wasValidated = true;
    }

    /**
     * build and return the results via xml.  debug status can be determined
     * from log4j category once we get all the logging in place.
     */
    public Element build(final Map<String, String> buildProperties, final Progress progressIn) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "25c87ddc-c0c3-49e3-bd06-f06125ad00d2");
        if (!wasValidated) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "29582505-94d4-42fb-b3e4-52e060a0dbeb");
            throw new IllegalStateException("This builder was never validated." + " The build method should not be getting called.");
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "3075d93d-4995-49d5-8509-42f9d1999bc1");
        validateBuildFileExists();
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "525f698b-b22d-48c2-b4d0-adf85ba2899e");
        final Progress progress = getShowProgress() ? progressIn : null;
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "8fb5c2df-65c4-41f4-bdfd-0cb0966b4f00");
        final OSEnvironment antEnv = new OSEnvironment();
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "2662be44-686f-4618-9acf-3d1302e99125");
        // Merge the environment with the configuration
        mergeEnv(antEnv);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "782373f9-2483-44eb-b3b1-5604dd33886c");
        final AntScript script = new AntScript();
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "ce00b8f3-6446-4d46-9632-d1e0eaf41719");
        script.setBuildProperties(buildProperties);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "c342ac47-c514-4635-b3f4-5840879892bc");
        script.setProperties(properties);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "3c575645-21cb-48ed-be2b-fba53c8b489d");
        script.setLibs(libs);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "1e422ab0-766b-4677-aeee-1c215156347d");
        script.setListeners(listeners);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "3cdcb31c-d386-45fb-afe1-870b35c2bd8e");
        script.setUseLogger(useLogger);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "32e1d2c5-b128-40d1-84f0-31a6f8aa88d6");
        script.setUseScript(antScript != null);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "4fc87b34-85d8-49af-aa3b-bd45ade81311");
        script.setWindows(Util.isWindows());
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "324cd9a1-bf20-4139-8ca3-a88c540acf61");
        script.setAntScript(antScript);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "a9dbca22-b0a3-42d5-9675-6f3b8024d8b3");
        script.setArgs(args);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "6fc95463-5c2e-4a65-8c0b-a224667e0ec4");
        script.setBuildFile(buildFile);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "5a4ddd40-bfd1-4740-a667-a70eef349e3b");
        script.setTarget(target);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "aabf08c9-841d-4f36-bdf3-3cc80897e72e");
        script.setLoggerClassName(loggerClassName);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "69fd7aa8-fb22-4ee4-bcbf-0d2853cbfa2d");
        script.setIsLoggerClassNameSet(isLoggerClassNameSet);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "9dbe2be9-cc51-44ad-9903-35e5f0cf0850");
        script.setShowAntOutput(isLiveOutput());
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "00446391-7b00-4305-8649-986598ab0602");
        script.setTempFileName(tempFileName);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "14e6718b-0d9e-4219-8f19-d97107da3a22");
        script.setUseDebug(useDebug);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "49c9dcc2-ddd2-4f33-825c-101bae9299e0");
        script.setUseQuiet(useQuiet);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "c1ad445a-6bd6-41a1-af41-0bdd8914ae56");
        script.setKeepGoing(keepGoing);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "f700c632-729e-4576-aa91-db1451baa2e9");
        script.setSystemClassPath(getSystemClassPath());
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "6e061e3c-e530-4eb2-b868-531a3778a420");
        script.setPropertyFile(propertyfile);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "f5de236a-6313-4864-bb33-dedf6fda6f5a");
        script.setProgressLoggerLib(progressLoggerLib);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "2c5f2337-6d7f-4473-8a7d-658c9b7b755e");
        script.setProgress(progress);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "37ca5e03-19a6-4377-9913-24fe44d7861c");
        script.setAntEnv(antEnv);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "ee7d125b-8235-4099-82ae-b7284f3abe41");
        final File workingDir = antWorkingDir != null ? new File(antWorkingDir) : null;
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "be71dd90-b52c-495f-b5ed-f25f3a02f846");
        final BuildOutputLogger buildOutputConsumer;
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "9d7b852d-4c5f-4dbc-9e76-a7a2ffb42426");
        if (isLiveOutput()) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "32c2e142-979b-4619-b7c3-2181f8492d5e");
            // TODO: I think there's a bug here when workingDir == null
            buildOutputConsumer = getBuildOutputConsumer(buildProperties.get(Builder.BUILD_PROP_PROJECTNAME), workingDir, AntOutputLogger.DEFAULT_OUTFILE_NAME);
        } else {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "e5756208-8349-4eec-a26c-7259552c930c");
            buildOutputConsumer = null;
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "6acfa38a-8daf-4430-8401-edf786c9e1c0");
        final boolean scriptCompleted = runScript(script, workingDir, buildOutputConsumer);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "cdc66dce-a4d4-493a-9ed2-ed279afaabe8");
        final File logFile = new File(antWorkingDir, tempFileName);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "22901299-8c51-4484-953d-0393ce6d1d9d");
        final Element buildLogElement;
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "b37f7bac-c9b2-4539-8169-9984dff4f913");
        if (!scriptCompleted) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "f7382321-df3e-49a6-ab9e-1cb09e72f229");
            LOG.warn("Build timeout timer of " + timeout + " seconds has expired");
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "6621f9cd-7a85-4559-8641-0db2fc3f0aea");
            buildLogElement = new Element("build");
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "f66c7fcd-f840-488d-8af4-47606b07c328");
            buildLogElement.setAttribute("error", "build timeout");
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "10fdad27-f3e6-476f-b757-f4d80f255be9");
            // somebody should really fix ant's XmlLogger
            if (logFile.exists()) {
                writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "89fa70d6-a5a5-4381-b7b0-39bb9bfd255a");
                try {
                    writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "900e02cf-752b-4171-a203-61e403c785ee");
                    buildLogElement.setText(Util.readFileToString(logFile));
                } catch (IOException likely) {
                // ignored
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "428073aa-88f8-4995-b762-0c9b19c4b367");
            // read in log file as element, return it
            buildLogElement = getAntLogAsElement(logFile);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "ef9ac45d-c93b-4350-9582-07d7c040ef24");
            saveAntLog(logFile);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "cb97d1cf-b879-4610-9f0c-0e3178a0a5f9");
            logFile.delete();
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "fcdf3f71-f439-4806-9c43-1dac1f90915b");
        return buildLogElement;
    }

    boolean runScript(final AntScript script, final File workingDir, final BuildOutputLogger outputLogger) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "e2a42fba-943e-4c80-a42e-f95455283148");
        return new ScriptRunner().runScript(workingDir, script, timeout, outputLogger);
    }

    public Element buildWithTarget(final Map<String, String> properties, final String buildTarget, final Progress progress) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "1e86bac6-c694-477c-9341-751dc4e7d3e2");
        final String origTarget = target;
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "354b188d-3774-453f-ab9a-7e9a85ebb02f");
        try {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "977cef0e-f08c-475f-84fb-d6e2dbbf79db");
            target = buildTarget;
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "26dec65a-fcb2-4f91-95ba-7a5e8d30a8af");
            return build(properties, progress);
        } finally {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "0c9acb0e-beb7-428f-8d5c-1d760b9711ba");
            target = origTarget;
        }
    }

    void validateBuildFileExists() throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "b65a0e7f-7f20-4db1-a0f4-fec34f50587c");
        File build = new File(buildFile);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "7ceca520-1f26-4515-a580-d4a5f7307e3b");
        if (!build.exists() && !build.isAbsolute() && antWorkingDir != null) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "8c8e7712-c569-48f5-be7b-a422c9c17f98");
            build = new File(antWorkingDir, buildFile);
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "18d951a0-2bce-4d5b-a166-f4340ec3adce");
        ValidationHelper.assertExists(build, "buildfile", this.getClass());
    }

    @Description("If supplied, a copy of the ant log will be saved in the specified " + "local directory. Example: saveLogDir=\"/usr/local/dev/projects/cc/logs\".")
    @Optional
    public void setSaveLogDir(String dir) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "ee95ace6-8f88-4445-a0bb-065bdec5c93f");
        saveLogDir = null;
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "bb4ad7cf-dee5-4383-a8d6-0ceb47062b55");
        if (dir != null && !dir.trim().equals("")) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "e010deec-1187-4142-be7d-2cc9d87bea1d");
            saveLogDir = new File(dir.trim());
        }
    }

    void saveAntLog(File logFile) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "e15cf15d-5aa9-4515-972f-f88b0c23dc21");
        if (saveLogDir == null) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "0c4c90ae-bddb-442d-955d-eb7556b3e1a7");
            return;
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "aa0c6730-9080-4847-a22b-0667596db824");
        try {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "140e0598-6dc3-45c1-9aa2-aa258abc479e");
            final File newAntLogFile = new File(saveLogDir, tempFileName);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "aef22a31-0310-47dd-a859-4f68b6ba967d");
            newAntLogFile.createNewFile();
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "9ad98a55-b738-41ed-a739-ce0bf0fc3397");
            final FileInputStream in = new FileInputStream(logFile);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "0cf615a6-ae72-45f8-aae7-8bfa9b8d20ba");
            try {
                writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "1ae521af-5042-460a-90e7-f341e454abfa");
                final FileOutputStream out = new FileOutputStream(newAntLogFile);
                writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "21a5d927-acb6-4fe1-b189-72e4fd9249bc");
                try {
                    writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "b4fbafa1-aded-4788-82a3-80a4fc8f3f5e");
                    byte[] buf = new byte[1024];
                    writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "9974eb19-4d81-431f-be90-5bd72cedd83a");
                    int len;
                    writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "02293641-16b3-4065-9c59-1650d1629629");
                    while ((len = in.read(buf)) > 0) {
                        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "71f9cdf7-4585-469b-9473-7b4d833ca02e");
                        out.write(buf, 0, len);
                    }
                } finally {
                    writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "9bba7890-62f4-4f72-9c55-f6e311864838");
                    out.close();
                }
            } finally {
                writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "282bb2bd-cd4f-44a2-bdb1-dfce8e3c9f90");
                in.close();
            }
        } catch (IOException ioe) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "d6227a04-bcf5-4846-93a8-99ad2b73d23b");
            LOG.error(ioe);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "3bc0b372-4932-4e34-8837-8c234d564c53");
            LOG.error("Unable to create file: " + new File(saveLogDir, tempFileName));
        }
    }

    @Description("Will invoke ANT in the specified directory. This directory can be " + "absolute or relative to the cruisecontrol working directory.")
    @Optional
    public void setAntWorkingDir(String dir) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "6c8a937e-a23e-40ef-a91b-8817d5b516ec");
        antWorkingDir = dir;
    }

    @Description("Absolute filename of script (shell script or bat file) used to start Ant. " + "You can use this to make CruiseControl use your own Ant installation. " + "If this is not specified, the AntBuilder uses the Ant distribution that " + "ships with CruiseControl. See below for <a href=\"#ant-examples\">examples" + "</a>.")
    @Optional("Recommended, however. Cannot be specified if anthome attribute " + "is also specified")
    public void setAntScript(String antScript) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "d522e8da-30e1-455c-987a-6e9b59689fbe");
        this.antScript = antScript;
    }

    @Description("Directory in which Ant is installed. CruiseControl will attempt to use the " + "standard Ant execution scripts (i.e. ant.bat or ant). See below for " + "<a href=\"#ant-examples\">examples</a>.")
    @Optional("Cannot be specified if antscript attribute is also specified.")
    public void setAntHome(String antHome) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "153b5176-0712-4c8f-afc6-dc586b8ef71e");
        this.antHome = antHome;
    }

    /**
     * @param isWindows if true, running under windows
     * @return If the anthome attribute is set, then this method returns the correct shell script
     * to use for a specific environment.
     * @throws CruiseControlException if <code>antHome</code> is not set
     */
    protected String findAntScript(boolean isWindows) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "53cce0d9-757f-4c5e-a5a5-502d8f75d024");
        if (antHome == null) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "f383808a-a54e-44b4-ba26-68e64f2f26a4");
            throw new CruiseControlException("anthome attribute not set.");
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "75e9591c-3ea0-42f9-b7dd-8cfbbac707d7");
        if (isWindows) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "6298467e-1ea5-40f6-ac13-b83d9ef3404e");
            return antHome + "\\bin\\ant.bat";
        } else {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "bb910161-e38d-4dcf-b0ed-0eaee1fa0b0c");
            return antHome + "/bin/ant";
        }
    }

    @Description("Name of temp file used to capture output.")
    @Optional
    @Default("log.xml")
    public void setTempFile(String tempFileName) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "d5f3cc2c-7721-4f4c-9b38-cfebd4543583");
        this.tempFileName = tempFileName;
    }

    @Description("Ant target(s) to run. Default is \"\", or the default target for " + "the build file.")
    @Optional
    public void setTarget(String target) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "52382358-306a-4cd9-8515-7e8a9513dfd7");
        this.target = target;
    }

    @Description("Path to Ant build file.")
    @Optional
    @Default("build.xml")
    public void setBuildFile(String buildFile) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "cd6349e2-4cc9-403e-a6ef-f33eda852826");
        this.buildFile = buildFile;
    }

    @Description("'true' if CruiseControl should call Ant using -logger; 'false' to call Ant " + "using '-listener', thus using the loggerclass as a Listener. uselogger=" + "\"true\" will make Ant log its messages using the class specified by " + "loggerclassname as an Ant Logger, which can make for smaller log files since " + "it doesn't log DEBUG messages (see useDebug and useQuiet attributes below, " + "and the <a href=\"http://ant.apache.org/manual/listeners.html\">Ant manual</a>). " + "Set to false to have Ant echo ant messages to console " + "using its DefaultLogger, which is useful when debugging your ant build. " + "Defaults to 'false' to make initial setup easier but setting it to 'true' is " + "recommended for production situations." + "<br/><br/>" + "RE: liveOutput: If liveOutput=true AND uselogger=true, this builder will write " + "the ant output to a file (antBuilderOutput.log) that can be read by the " + "Dashboard reporting application. The liveOutput setting has no effect if " + "uselogger=false. <a href=\"#antbootstrapper\">AntBootstrapper</a> and " + "<a href=\"#antpublisher\">AntPublisher</a> do not provide access to " + "liveOutput, and operate as if liveOutput=false. NOTE: In order to show ant " + "output while uselogger=true, the AntBuilder uses a custom Build Listener. If " + "this interferes with your Ant build, set liveOutput=false (and please report " + "the problem)")
    @Optional
    public void setUseLogger(boolean useLogger) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "9768aee9-817f-4d37-bfc5-8209526e8199");
        this.useLogger = useLogger;
    }

    /**
     * Sets whether Ant will use the custom AntOutputLogger as a listener in order to show live output.
     * @param showAntOutput if true, add AntOutputLogger as a listener.
     * @deprecated Use {@link #setLiveOutput(boolean)} instead.
     */
    @SkipDoc
    public void setShowAntOutput(final boolean showAntOutput) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "38f23449-cb1b-43ec-b6eb-0053288a1e25");
        setLiveOutput(showAntOutput);
    }

    /**
     * @return true if Ant will use the custom AntOutputLogger as a listener in order to show live output.
     * @deprecated Use {@link #isLiveOutput()} instead.
     */
    boolean getShowAntOutput() {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "3a5a9578-7079-4dc0-a3f3-01268f4e3720");
        return isLiveOutput();
    }

    /**
     * @param showAntOutput if false, disables Dashboard AntOutputLogger
     * @param useLogger if false, disables Dashboard AntOutputLogger
     * @return true if the jar containing the custom Dashboard logger class must be added to the command line used
     * to execute Ant.
     */
    static boolean shouldAddDashboardLoggerJarToCommandLine(final boolean showAntOutput, final boolean useLogger) {
        writelineStatic("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "2a96c028-db40-46d7-8dbd-04a6c60680a6");
        return showAntOutput && useLogger;
    }

    @Description("Pass specified argument to the jvm used to invoke ant." + "Ignored if using anthome or antscript. The element has a single required" + "attribute: \"arg\".<br />" + "<strong>Example:</strong> <code>&lt;jvmarg arg=\"-Xmx120m\"/&gt;</code>")
    @Cardinality(min = 0, max = -1)
    public JVMArg createJVMArg() {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "45d22769-0641-4326-8dc9-868616ab8e06");
        final JVMArg arg = new JVMArg();
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "2268d30e-1ae4-4513-8742-727356bf5980");
        args.add(arg);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "ee010a06-118c-4cdf-8e01-49cb4f5b0c59");
        return arg;
    }

    @Description("Used to define additional <a " + "href=\"http://ant.apache.org/manual/running.html#libs\">library directories</a> " + "for the ant build. The element has one required attribute: \"searchPath\".<br /> " + "<strong>Example:</strong> <code>&lt;lib searchPath=\"/home/me/myantextensions\"/" + "&gt;</code>")
    @Cardinality(min = 0, max = -1)
    public Lib createLib() {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "e0586499-2f97-4fbd-b33e-fd2a60ce5be5");
        final Lib lib = new Lib();
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "bfd8323c-e3c7-43a3-898c-f3165ec20c61");
        libs.add(lib);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "264ae2de-7547-466b-abfe-8c12f9108134");
        return lib;
    }

    @Description("Used to define additional <a " + "href=\"http://ant.apache.org/manual/listeners.html\">listeners</a> for the " + "ant build. The element has one required attribute: \"classname\".<br />" + "<strong>Example:</strong> <code>&lt;listener classname=\"org.apache.tools." + "ant.listener.Log4jListener\"/&gt;</code>")
    @Cardinality(min = 0, max = -1)
    public Listener createListener() {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "310c1b06-fd3f-4bcf-a89f-67a609b440b2");
        final Listener listener = new Listener();
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "97186958-14fe-4a95-a93c-100d4329cc92");
        listeners.add(listener);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "b65e21e4-ba83-434e-aeb5-a74198eda80c");
        return listener;
    }

    @Description("Used to define properties for the ant build. The element has two " + "required attributes: \"name\" and \"value\". These will be passed on the " + "ant command-line as \"-Dname=value\"<br />" + "<strong>Example:</strong> <code>&lt;property name=\"foo\" value=\"bar\"/" + "&gt;</code>")
    @Cardinality(min = 0, max = -1)
    public Property createProperty() {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "b3bee8b7-9cf4-4c33-89b4-73a6a1ed7422");
        final Property property = new Property();
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "7c5a6db2-772e-4a1b-ae5a-25e5e6d49254");
        properties.add(property);
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "2371edca-112c-44cd-84d3-e7a3db4026a4");
        return property;
    }

    protected String getSystemClassPath() {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "32c5a061-0476-47cc-8fef-e9b6d2782644");
        return System.getProperty("java.class.path");
    }

    protected Element getAntLogAsElement(File file) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "f7246004-bfc2-4aee-86bc-fe56548ee64b");
        if (!file.exists()) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "123fc0f5-39d0-455e-ae59-8f1f705c6fc1");
            throw new CruiseControlException("ant logfile " + file.getAbsolutePath() + " does not exist.");
        } else if (file.length() == 0) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "a553119a-3026-43f7-ae72-2383534f3f5c");
            throw new CruiseControlException("ant logfile " + file.getAbsolutePath() + " is empty. Your build probably failed. Check your CruiseControl logs.");
        }
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "c8d8c996-2476-430c-bdc8-bdf600559700");
        try {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "a8c0d364-8d28-4003-a54f-212880077ab2");
            SAXBuilder builder = new SAXBuilder("org.apache.xerces.parsers.SAXParser");
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "5b37b54a-25f9-44b6-ac44-8b4e4d872376");
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
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "f1b4c420-fd40-4948-93dc-c3f6b2473b7f");
            // get rid of empty <task>- and <message>-elements created by Ant's XmlLogger
            XMLFilter emptyTaskFilter = new EmptyElementFilter("task");
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "8a2af539-5bb8-49ad-939f-e7553102adb4");
            emptyTaskFilter.setParent(piFilter);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "882d7b03-8c67-4ebb-a32a-d2a6929f45d7");
            XMLFilter emptyMessageFilter = new EmptyElementFilter("message");
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "eb4b4f70-2c63-464f-a331-69258c8937b3");
            emptyMessageFilter.setParent(emptyTaskFilter);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "b14f02e1-6b57-4dca-b040-cbe706af750b");
            builder.setXMLFilter(emptyMessageFilter);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "c0117adc-1045-477f-8b55-17cc8a66c254");
            return builder.build(file).getRootElement();
        } catch (Exception ee) {
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "440c690b-299f-4350-9d50-9069fc8b34ec");
            if (ee instanceof CruiseControlException) {
                writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "d891aa5e-df2a-43e4-8903-01a305731cd5");
                throw (CruiseControlException) ee;
            }
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "a77dc022-8bd1-4eda-96f1-1629e96664d4");
            File saveFile = new File(file.getParentFile(), System.currentTimeMillis() + file.getName());
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "40209e64-f808-41d7-8289-fc226e8afb28");
            file.renameTo(saveFile);
            writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "6213ce60-b17f-477c-b544-2bee0c1a1940");
            throw new CruiseControlException("Error reading : " + file.getAbsolutePath() + ".  Saved as : " + saveFile.getAbsolutePath(), ee);
        }
    }

    @Description("If true will invoke ant with -debug, which can be useful for debugging your " + "ant build. Defaults to 'false', cannot be set to 'true' if usequiet is " + "also set to 'true'. When used in combination with uselogger=\"true\", " + "this will result in bigger XML log files; otherwise, it will cause more " + "output to be written to the console by Ant's DefaultLogger.")
    @Optional
    public void setUseDebug(boolean debug) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "b6e550dd-11d5-41ea-9a26-a24fbd4df116");
        useDebug = debug;
    }

    @Description("If true will invoke ant with -quiet, which can be useful for creating smaller " + "log files since messages with a priority of INFO will not be logged. Defaults " + "to 'false', cannot be set to 'true' if usedebug is also set to 'true'. " + "Smaller logfiles are only achieved when used in combination with uselogger=" + "\"true\", otherwise there will just be less output echoed to the console by " + "Ant's DefaultLogger." + "<br/><br/>" + "RE: showProgress: useQuiet=\"true\" will prevent any progress messages from " + "being displayed. NOTE: In order to show progress, the AntBuilder uses custom " + "Build Loggers and Listeners. If these interfere with your Ant build, set " + "showProgress=false (and please report the problem).")
    @Optional
    public void setUseQuiet(boolean quiet) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "fe4aca4c-9999-4e34-b7f5-a154dcaedc23");
        useQuiet = quiet;
    }

    @Description("If true will invoke ant with -keep-going, which can be useful for performing " + "build steps after an optional step fails. Defaults to 'false'.")
    @Optional
    public void setKeepGoing(boolean keepGoing) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "2047511a-6293-41d4-912c-1cb772e19747");
        this.keepGoing = keepGoing;
    }

    public String getLoggerClassName() {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "f3f9ab29-3da1-45e6-89c9-17a9aeaac732");
        return loggerClassName;
    }

    @Description("If you want to use another logger (or listener, when uselogger=\"false\") than " + "Ant's XmlLogger, you can specify the classname of the logger here. The logger " + "needs to output compatible XML, and the class needs to be available on the " + "classpath at buildtime.")
    @Optional
    @Default("org.apache.tools.ant.XmlLogger")
    public void setLoggerClassName(String string) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "34f81542-c9df-4f81-8b01-9575d8a6d26b");
        loggerClassName = string;
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "fac72cb7-ed28-489a-9eaa-b1eaef5265ac");
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
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "726341e6-3518-469d-a0dc-6548526e8d3b");
        this.timeout = timeout;
    }

    @Description("Load all properties from file with -D properties (like child <code><a href=\"" + "#antbuilderchildprop\">&lt;property&gt;</a></code> elements) taking " + "precedence. Useful when the propertyfile content can change for every build.")
    @Optional
    public void setPropertyfile(String propertyfile) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "41b3fb60-ff81-457d-a02c-0e680ab60f74");
        this.propertyfile = propertyfile;
    }

    @Description("Overrides the default -lib search path used to add support for showProgress " + "features in the ant builder. This search path ensures customized ant " + "Loggers/Listeners are available on the classpath of the ant builder VM. You " + "should not normally set this value. If you do set this value, you should " + "use the full path (including filename) to cruisecontrol-antprogresslogger.jar. " + "This setting has no effect if showProgress=false.")
    @Optional
    public void setProgressLoggerLib(String progressLoggerLib) {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "85ed2e57-6fc7-4eaf-aefa-8c643d8e9660");
        this.progressLoggerLib = progressLoggerLib;
    }

    /**
     * @return The path (including filename) to the jar file
     * ({@link AntScript#LIBNAME_PROGRESS_LOGGER cruisecontrol-antprogresslogger.jar})
     * containing the AntProgressLogger/Listener classes.
     */
    public String getProgressLoggerLib() {
        writeline("/home/ubuntu/results/coverage/AntBuilder/AntBuilder_8_10.coverage", "e785ba8b-a465-4874-a133-bfcd87a456f5");
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
