/**
 * *****************************************************************************
 * CruiseControl, a Continuous Integration Toolkit
 * Copyright (c) 2003, ThoughtWorks, Inc.
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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;
import java.io.File;
import net.sourceforge.cruisecontrol.CruiseControlException;
import net.sourceforge.cruisecontrol.Progress;
import net.sourceforge.cruisecontrol.util.Commandline;
import net.sourceforge.cruisecontrol.util.OSEnvironment;
import net.sourceforge.cruisecontrol.util.StreamConsumer;
import net.sourceforge.cruisecontrol.util.UtilLocator;
import org.apache.log4j.Logger;
import java.io.*;

/**
 * Ant script class.
 *
 * Contains all the details related to running a Ant based build via
 * either a batch script or inprocess.
 * @author <a href="mailto:epugh@opensourceconnections.com">Eric Pugh</a>
 */
public class AntScript implements Script, StreamConsumer {

    private static final Logger LOG = Logger.getLogger(AntScript.class);

    static final String CLASSNAME_DASHBOARD_LISTENER = "net.sourceforge.cruisecontrol.builders.AntOutputLogger";

    static final String CLASSNAME_ANTPROGRESS_LOGGER = "net.sourceforge.cruisecontrol.builders.AntProgressLogger";

    static final String CLASSNAME_ANTPROGRESS_XML_LOGGER = "net.sourceforge.cruisecontrol.builders.AntProgressXmlLogger";

    static final String CLASSNAME_ANTPROGRESS_XML_LISTENER = "net.sourceforge.cruisecontrol.builders.AntProgressXmlListener";

    public static final String LIBNAME_PROGRESS_LOGGER = "cruisecontrol-antprogresslogger.jar";

    /**
     * Prefix prepended to system out messages to be detected by AntScript as progress messages.
     * NOTE: Must be the exact same string as that defined in AntProgressLog constant, kept separate
     * to avoid dependence on Ant Builder classes in AntScript.
     */
    static final String MSG_PREFIX_ANT_PROGRESS = "ccAntProgress -- ";

    private Map<String, String> buildProperties;

    private boolean isWindows;

    private String antScript;

    private List<AntBuilder.JVMArg> args;

    private List<AntBuilder.Lib> libs;

    private List<AntBuilder.Listener> listeners;

    private String loggerClassName;

    private boolean isLoggerClassNameSet;

    private boolean showAntOutput;

    private String tempFileName = "log.xml";

    private boolean useScript;

    private boolean useLogger;

    private boolean useQuiet;

    private boolean useDebug;

    private boolean keepGoing;

    private String buildFile = "build.xml";

    private List<Property> properties;

    private String target = "";

    private String systemClassPath;

    private int exitCode;

    private String propertyfile;

    private String progressLoggerLib;

    private Progress progress;

    private OSEnvironment env;

    /**
     * construct the command that we're going to execute.
     *
     * @return Commandline holding command to be executed
     * @throws CruiseControlException on unquotable attributes
     */
    public Commandline buildCommandline() throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "8a364b65-bc5a-48d1-b9c2-0ccaad1e1a47");
        final Commandline cmdLine = new Commandline();
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "7d3e27d7-cc3d-4be7-9b9d-0c27aba68c9c");
        if (useScript) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "16453c65-7bd8-4b94-ae99-d22a0a9c5643");
            cmdLine.setExecutable(antScript);
        } else {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "fdfffe49-cfa6-4d53-83dd-51e054547eec");
            if (isWindows) {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "7b1ef3f8-9662-41e8-8103-c5b1d93e4df3");
                cmdLine.setExecutable("java.exe");
            } else {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "1d07944b-f47d-4ce7-b62f-ae6ebc8f6b5d");
                cmdLine.setExecutable("java");
            }
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "73b51f3a-a556-4d5a-b733-9fadd7522e76");
            for (final AntBuilder.JVMArg jvmArg : args) {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "22bebbd8-9c6c-464e-bbc8-49b1686acfef");
                final String arg = jvmArg.getArg();
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "53a60791-ef46-4678-9f49-06f8a46cdc3a");
                // empty args may break the command line
                if (arg != null && arg.length() > 0) {
                    writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "9f51f1eb-733e-4e2b-9770-9c8ab4fb5cc6");
                    cmdLine.createArgument(arg);
                }
            }
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "3a3e5220-8c03-4f93-a053-707a34c54058");
            final List<String> classpathItems = getClasspathItems(systemClassPath, isWindows);
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "8b463e45-085f-40a5-a8bd-8a2864ba3632");
            final String antLauncherJarLocation = getAntLauncherJarLocation(systemClassPath, classpathItems);
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "8bafa97d-91c9-42d3-8295-4e8f861d65fc");
            cmdLine.createArguments("-classpath", antLauncherJarLocation);
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "0e02db55-76fc-4ed5-99c9-c77ef8a0026a");
            cmdLine.createArgument("org.apache.tools.ant.launch.Launcher");
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "92a1e4c8-821b-45c9-909b-7e0b60cec867");
            cmdLine.createArguments("-lib", removeSaxonJars(classpathItems, isWindows));
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "584d7635-d1b4-4daf-aeff-d83ed93b24da");
        if (progress == null) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "18477e58-b2eb-4e9e-a54e-e92af90177df");
            if (useLogger) {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "9254d9aa-c368-4e75-9424-636430582975");
                cmdLine.createArguments("-logger", getLoggerClassName());
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "ab20b0b3-006f-4ae0-9a67-7f819fa9e631");
                cmdLine.createArguments("-logfile", tempFileName);
            } else {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "75f137c1-e1f5-43b1-a9d8-e85598e2ef6a");
                cmdLine.createArguments("-listener", getLoggerClassName());
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "e60cb12e-13bc-4bbb-9a33-16fa95989836");
                cmdLine.createArgument("-DXmlLogger.file=" + tempFileName);
            }
        } else {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "872cbd94-989a-447a-92df-ea03169b7c79");
            // need to showProgress
            // use proper default logger if loggerClassName was not specified by config
            setupResolvedLoggerClassname();
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "764644bf-443f-454c-9faf-deaf7e2bf343");
            cmdLine.createArguments("-logger", getLoggerClassName());
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "ea848898-8166-4dce-b74e-25ffc06f1cc8");
            if (useLogger) {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "cb53a620-9000-403d-b87e-f1bd6a30b0d2");
                // need to use AntProgressXmlLogger as a listener
                cmdLine.createArguments("-listener", CLASSNAME_ANTPROGRESS_XML_LISTENER);
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "709b7e69-ab11-4b5f-b439-d71ae3463a7f");
                cmdLine.createArgument("-DXmlLogger.file=" + tempFileName);
            } else {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "f5671c60-b3d8-4eb8-9352-107a2b6287c0");
                cmdLine.createArguments("-listener", AntBuilder.DEFAULT_LOGGER);
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "5a79e9e3-ee3f-4893-874c-e5e0ed321eee");
                cmdLine.createArgument("-DXmlLogger.file=" + tempFileName);
            }
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "3b3f0cfb-d02b-439d-a58f-d9bb99737911");
        if (AntBuilder.shouldAddDashboardLoggerJarToCommandLine(showAntOutput, useLogger)) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "4ab7f15c-c5a8-42eb-9c8c-731d2e7c383e");
            cmdLine.createArguments("-listener", CLASSNAME_DASHBOARD_LISTENER);
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "277943bb-6efa-4515-b14f-7144e5b7b5cc");
        if ((progress != null) || AntBuilder.shouldAddDashboardLoggerJarToCommandLine(showAntOutput, useLogger)) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "2aca0f5b-18b4-4103-b4bb-168502b043e5");
            // we need to add the custom logger jar {@link #LIBNAME_PROGRESS_LOGGER cruisecontrol-antprogresslogger.jar}
            // to the ant VM class path as a lib
            setupDefaultProgressLoggerLib();
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "7af0c521-3038-4d71-9246-b824db654555");
            // add -lib to progressLogger classes
            cmdLine.createArguments("-lib", progressLoggerLib);
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "74e7dca3-e84d-493c-896d-cc731fc6c9dd");
        // a listener, they will affect the default logger that writes to the console
        if (useDebug) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "5dfdbdd7-ae54-4775-b324-a045988894ee");
            cmdLine.createArgument("-debug");
        } else if (useQuiet) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "6e1a2888-60c7-4764-9011-607aae2e819b");
            cmdLine.createArgument("-quiet");
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "00f41591-01ec-4745-8b65-aa3f1f0f7d04");
        if (keepGoing) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "046a448e-2254-415a-bb1f-aa4f30b92f23");
            cmdLine.createArgument("-keep-going");
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "d98473e6-24b6-440f-b4f6-376308c05046");
        for (final AntBuilder.Lib lib : libs) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "a66462e4-df53-4732-bc27-d89bbd2f15cc");
            cmdLine.createArguments("-lib", lib.getSearchPath());
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "10b1e04a-dab7-4186-8614-8ac3828e93cd");
        for (final AntBuilder.Listener listener : listeners) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "63d20f50-d2e7-470b-8506-d360bdad111e");
            cmdLine.createArguments("-listener", listener.getClassName());
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "30dd4990-db5a-4134-acb3-9388caf38af7");
        for (final Map.Entry property : buildProperties.entrySet()) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "214b45c5-a761-4893-af60-97a2f90d94b2");
            final String value = (String) property.getValue();
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "4f8dc0bf-f458-4a2c-a762-bf845d1fa3c9");
            if (!"".equals(value)) {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "2107cc4a-0a9d-47b7-9563-8cdb73801a17");
                cmdLine.createArgument("-D" + property.getKey() + "=" + value);
            }
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "cea6208a-8980-41f4-803f-ae1e52764de2");
        for (final Property property : properties) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "6c895887-a765-4fac-9312-ddc64b24c945");
            cmdLine.createArgument("-D" + property.getName() + "=" + property.getValue());
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "f0aba7c2-c70d-410d-9daa-0f1734f841cd");
        if (propertyfile != null) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "5d176d2e-c427-4753-8541-f87738fa13c0");
            cmdLine.createArguments("-propertyfile", propertyfile);
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "f58c03f4-aa19-405e-9937-5a7b0dc6a51e");
        cmdLine.createArguments("-buildfile", buildFile);
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "f7ad8b86-6c9d-427b-9060-007f02f5a995");
        cmdLine.setEnv(env);
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "3f502cf0-cd56-4a14-b6bf-6b206a5420f1");
        final StringTokenizer targets = new StringTokenizer(target);
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "2a37a92f-58b7-4992-9fb3-d14114bd8c4b");
        while (targets.hasMoreTokens()) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "d9c76d19-21d5-4cc2-8323-ec67374cc65b");
            cmdLine.createArgument(targets.nextToken());
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "5aa70bc5-0492-45ed-92c4-0193d14a6d60");
        return cmdLine;
    }

    /**
     * @param path the classpath in which to search for the ant-launcher.jar
     * @param isWindows true if running on Windows
     * @return the path to ant-launcher*.jar taken from the given path
     * @throws CruiseControlException if path to ant-launcher.jar could not be found.
     */
    String getAntLauncherJarLocation(final String path, final boolean isWindows) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "2d6580f5-76f0-4230-abd8-8ab2c5fc9a88");
        return getAntLauncherJarLocation(path, getClasspathItems(path, isWindows));
    }

    /**
     * @param path the classpath as a single string, used here only for error message.
     * @param classpathItems the classpath items to search for the ant-launcher.jar
     * @return the path to ant-launcher*.jar taken from the given path
     * @throws CruiseControlException if path to ant-launcher.jar could not be found.
     */
    private String getAntLauncherJarLocation(final String path, final List<String> classpathItems) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "d914f6ba-fd4e-49e6-9819-b42df0b4b635");
        for (final String pathElement : classpathItems) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "e851e121-d931-41ea-bdee-5ae7b8302bd2");
            if (pathElement.indexOf("ant-launcher") != -1 && pathElement.endsWith(".jar")) {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "e65c3186-5d7c-443a-a83d-de83346d16e8");
                return pathElement;
            }
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "8a70d667-910d-4c61-90e7-154b4454e9c9");
        throw new CruiseControlException("Couldn't find path to ant-launcher jar in this classpath: '" + path + "'");
    }

    /**
     * @param path the classpath to split each element into a List
     * @param isWindows true if running on Windows
     * @return a List containing each element in the classpath
     */
    List<String> getClasspathItems(final String path, final boolean isWindows) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "177e573c-5d61-43bb-9ba7-25d25d2574a1");
        final List<String> ret = new ArrayList<String>();
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "e66c1c9b-a4b9-40bb-a5e2-45d1fcf39ca1");
        final String separator = getSeparator(isWindows);
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "e2eb1f70-6124-430d-bcef-473f26ddd50a");
        final StringTokenizer pathTokenizer = new StringTokenizer(path, separator);
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "dd72275d-a3c4-4ed2-950e-35e0c3f94b01");
        while (pathTokenizer.hasMoreTokens()) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "28c0fbe9-38d4-4fd6-adcc-7b5dc39ad920");
            final String pathElement = pathTokenizer.nextToken();
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "37a5b214-7d83-492f-b680-300a2117aa0b");
            ret.add(pathElement);
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "e57aa8c2-f243-4d57-baa4-242d13193ac5");
        return ret;
    }

    /**
     * The Saxon jars cause the Ant junitreport task to fail.
     *
     * @param classpathItems a List containing items in a classpath
     * @param isWindows true if running on Windows
     * @return a String containing all the jars in the classpath minus the Saxon jars
     */
    String removeSaxonJars(final List<String> classpathItems, final boolean isWindows) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "590a1133-44cc-4a2e-a513-64b8b570125b");
        final StringBuilder path = new StringBuilder();
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "bbbe7fc4-6b1d-4074-a3d6-580f92afa8bb");
        final String separator = getSeparator(isWindows);
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "3b81adbc-6a9c-45fc-98b1-19e41f9e9668");
        for (final String pathElement : classpathItems) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "90a735ac-eef5-4ea9-b6d5-ba914d53465e");
            final File elementFile = new File(pathElement);
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "8c07203d-aa79-4a89-92df-ffc38bb99745");
            if (!elementFile.getName().startsWith("saxon")) {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "f49ccd98-6d0e-4561-b187-e0fb741060e0");
                if (path.length() > 0) {
                    writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "981bba15-44f4-4c44-ae8a-76c366b1b7af");
                    path.append(separator);
                }
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "2e24eacd-5406-4f7f-a749-65b04a495cea");
                path.append(pathElement);
            }
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "107f6478-950e-43dc-bafa-af1d64456ab2");
        return path.toString();
    }

    String removeSaxonJars(final String path, final boolean isWindows) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "9687e786-7773-48bd-9068-cf39a213053b");
        return removeSaxonJars(getClasspathItems(path, isWindows), isWindows);
    }

    private String getSeparator(boolean isWindows) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "381aa4fc-4398-46ec-a6ca-8d93b899a0aa");
        return isWindows ? ";" : ":";
    }

    void setupResolvedLoggerClassname() {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "6c17af0d-bc8d-442e-9d42-290f57109503");
        // use proper default logger if loggerClassName was not specified by config
        if ((progress != null) && (!isLoggerClassNameSet)) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "a3664755-76cb-4e62-a90d-d12d75d9e993");
            if (useLogger) {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "d8aa8eb2-3320-4b7d-882b-f5701528de4d");
                // use string to avoid dependence on ant classes
                loggerClassName = CLASSNAME_ANTPROGRESS_XML_LOGGER;
            } else {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "0ad0e839-66fd-4c22-9967-ba5f577136bf");
                // use string to avoid dependence on ant classes
                loggerClassName = CLASSNAME_ANTPROGRESS_LOGGER;
            }
        } else {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "baa37a2f-ecf8-4aeb-bab4-2434cf4f4034");
            if (progress != null) {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "bad78bcf-3ec1-439b-9762-ee2cc031e3ad");
                LOG.warn("Ant Progress support is enabled AND loggerClassname is set. " + "Be sure the loggerClassName: " + loggerClassName + " is compatible with" + " Ant Progress.");
            }
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "f3f56ef1-7c60-44db-a046-83376c683ee5");
        LOG.debug("Using loggerClassName: " + loggerClassName);
    }

    private static final String MSG_RESOLUTION_PROGRESS_LOGGER_LIB = "\n\tTo enable showAntOutput and/or showProgress, do one of the following: " + "\n\t1. Copy " + LIBNAME_PROGRESS_LOGGER + " to a directory, and set the full path (including filename) " + "\n\t\tto " + LIBNAME_PROGRESS_LOGGER + " in config.xml as the value of 'progressLoggerLib' for this " + "<ant> builder. " + "\n\t2. Set showAntOutput=false and/or showProgress=false for this <ant> builder." + "\n\t3. Copy " + LIBNAME_PROGRESS_LOGGER + " into your ant/lib directory." + "\n\tNote: Please report this issue, as not finding this library is most likely a boog.";

    /**
     * Finds the default location of the {@link AntScript#LIBNAME_PROGRESS_LOGGER cruisecontrol-antprogresslogger.jar}
     * by first finding the location of the jar containing the {@link AntScript} class.
     *
     * @return the full path (including jar name) to the jar file
     * ({@link #LIBNAME_PROGRESS_LOGGER cruisecontrol-antprogresslogger.jar})
     * containing the AntProgressLogger/Listener classes.
     * @throws ProgressLibLocatorException if the search class ({@link AntScript}) file can't be found,
     * likely related to running under Java Webstart >= 6, or simply if the jar can't be found
     */
    public static String findDefaultProgressLoggerLib() throws ProgressLibLocatorException {
        writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "f9dc896d-ab6b-4a75-a196-00c4e2eaa1ab");
        // find path (including filename) to progressLoggerLib jar
        final String progressLoggerLib;
        writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "ba87c8d9-8f40-4ebe-8074-7e458b1698f2");
        final File ccMain = UtilLocator.getClassSource(AntScript.class);
        writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "d040a614-d268-47f9-bd57-a5fc8857cdc5");
        if (ccMain == null) {
            writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "593f5e82-f22d-4b98-943a-0c63f7d527a0");
            throw new ProgressLibLocatorException("Could not determine -lib path for progressLoggerLib. (Java 6/Webstart issue?) " + MSG_RESOLUTION_PROGRESS_LOGGER_LIB);
        } else {
            writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "fddfd4b0-f766-4cba-88ee-5caf038ebdfc");
            final String pathToDirContainingCCMainJar;
            writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "3edd743b-5071-457d-b5e8-dba4c5e89d2f");
            if (ccMain.isDirectory()) {
                writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "1e0023d1-2d56-40da-965c-f8b934ab4c2d");
                pathToDirContainingCCMainJar = ccMain.getAbsolutePath();
            } else {
                writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "d2ea7c38-0e0a-49c9-83c2-794725aa082c");
                pathToDirContainingCCMainJar = ccMain.getParentFile().getAbsolutePath();
            }
            writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "43d1ee97-36c8-4d0d-89cd-7a8e013657a7");
            final File expectedProgressLoggerJar = new File(pathToDirContainingCCMainJar, LIBNAME_PROGRESS_LOGGER);
            writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "0a259d5c-0fbe-4332-9ecf-9ae53b31dc19");
            if (expectedProgressLoggerJar.exists()) {
                writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "15710954-75f9-4e5a-8dba-7e3e6af83f2e");
                // Use the specific jar if that jar exists.
                // This is a bit of a hack to load the progress logger jar into
                // ant without loading other jars (such as, ant.jar for instance)
                progressLoggerLib = expectedProgressLoggerJar.getAbsolutePath();
            } else {
                writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "ed83d0b6-4f3f-4c1a-a6ae-8d6e8a9ca7ec");
                // Missing Progress Logger Lib is nasty to debug, so error out here if we can't find it for sure.
                throw new ProgressLibLocatorException("The progressLoggerLib jar file does not exist where expected: " + expectedProgressLoggerJar.getAbsolutePath() + MSG_RESOLUTION_PROGRESS_LOGGER_LIB);
            }
        }
        writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "9968e4cd-32c2-47bf-b6ac-4187a92aced4");
        return progressLoggerLib;
    }

    void setupDefaultProgressLoggerLib() throws ProgressLibLocatorException {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "b4448ffd-2f61-4093-b030-3333bd7858b5");
        if (progressLoggerLib == null) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "648fbd54-4517-499d-a6ec-4f6feb89b635");
            // Use a valid default for progressLoggerLib
            progressLoggerLib = findDefaultProgressLoggerLib();
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "a6ceadd8-73d3-4351-bc0b-d2186c9f34f7");
            LOG.debug("Using default progressLoggerLib: " + progressLoggerLib);
        }
    }

    public static final class ProgressLibLocatorException extends CruiseControlException {

        private ProgressLibLocatorException(final String msg) {
            super(msg);
        }
    }

    /**
     * Analyze the output of ant command, used to detect progress messages.
     */
    public void consumeLine(final String line) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "ebbf0b1f-08dc-4c8c-8df9-f638985561f6");
        if (progress != null && line != null && line.startsWith(MSG_PREFIX_ANT_PROGRESS)) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "d220e376-4aa7-4e6e-9f47-bbb0eb7409e6");
            progress.setValue(line.substring(MSG_PREFIX_ANT_PROGRESS.length()));
        }
    }

    /**
     * @param buildProperties The buildProperties to set.
     */
    public void setBuildProperties(final Map<String, String> buildProperties) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "2fc1e4e0-cb76-4acd-98d8-3fe775f0fc6d");
        this.buildProperties = buildProperties;
    }

    /**
     * @return Returns the loggerClassName.
     */
    public String getLoggerClassName() {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "a4e8ca1a-3adb-4e3c-94ea-6cbb4939abd0");
        return loggerClassName;
    }

    /**
     * @param loggerClassName The loggerClassName to set.
     */
    public void setLoggerClassName(String loggerClassName) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "cf0b3171-6bf5-4239-a9c0-a595b90a0f4d");
        this.loggerClassName = loggerClassName;
    }

    /**
     * @param isLoggerClassNameSet The loggerClassName to set.
     */
    public void setIsLoggerClassNameSet(boolean isLoggerClassNameSet) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "3db2aa72-5c3b-41ff-97bb-0831542def97");
        this.isLoggerClassNameSet = isLoggerClassNameSet;
    }

    /**
     * @param showAntOutput if true use Dashboard AntOutputLogger (CLASSNAME_DASHBOARD_LISTENER) as listener IIF
     * useLogger is also true
     */
    public void setShowAntOutput(final boolean showAntOutput) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "d8d1145f-2949-4358-b38e-0014dc066fba");
        this.showAntOutput = showAntOutput;
    }

    /**
     * @param antScript The antScript to set.
     */
    public void setAntScript(String antScript) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "79013512-4e5b-46fa-a354-57751bce891d");
        this.antScript = antScript;
    }

    /**
     * @param args The args to set.
     */
    public void setArgs(final List<AntBuilder.JVMArg> args) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "958ad2cb-be08-421c-928b-027c63658980");
        this.args = args;
    }

    /**
     * @param isWindows The isWindows to set.
     */
    public void setWindows(boolean isWindows) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "9dd244ff-6f09-48de-b3b0-b070bae38ddd");
        this.isWindows = isWindows;
    }

    /**
     * @param buildFile The buildFile to set.
     */
    public void setBuildFile(String buildFile) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "4381dcc5-43d8-422e-9048-71a591aa98b8");
        this.buildFile = buildFile;
    }

    /**
     * @param tempFileName The tempFileName to set.
     */
    public void setTempFileName(String tempFileName) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "a0540671-942c-40c3-8dd3-9ffb907838a8");
        this.tempFileName = tempFileName;
    }

    /**
     * @param useDebug The useDebug to set.
     */
    public void setUseDebug(boolean useDebug) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "9fa84000-5e37-4149-b81e-ab12b4ffcf50");
        this.useDebug = useDebug;
    }

    /**
     * @param useLogger The useLogger to set.
     */
    public void setUseLogger(boolean useLogger) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "098925f3-9bd5-43c1-b100-0c233d43403d");
        this.useLogger = useLogger;
    }

    /**
     * @param useQuiet The useQuiet to set.
     */
    public void setUseQuiet(boolean useQuiet) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "82197cca-5210-4c3a-b88e-f63cd015857c");
        this.useQuiet = useQuiet;
    }

    public void setKeepGoing(boolean keepGoing) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "881fad62-8d49-4684-8de8-4d4a4d8ef435");
        this.keepGoing = keepGoing;
    }

    /**
     * @param useScript The useScript to set.
     */
    public void setUseScript(boolean useScript) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "b03b1e18-1dce-4244-b4c6-c1e50cd66f64");
        this.useScript = useScript;
    }

    /**
     * @param systemClassPath The systemClassPath to set.
     */
    public void setSystemClassPath(String systemClassPath) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "7f896743-eb7c-4b65-bad1-d9e67147a120");
        this.systemClassPath = systemClassPath;
    }

    /**
     * @param properties The properties to set.
     */
    public void setProperties(final List<Property> properties) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "80578c7a-0144-49b7-8ced-db9fcd84826d");
        this.properties = properties;
    }

    /**
     * @param libs The set of library paths to use.
     */
    public void setLibs(final List<AntBuilder.Lib> libs) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "7a38077a-41b0-474d-83ac-35bc5ac685a5");
        this.libs = libs;
    }

    /**
     * @param listeners The set of listener classes to use.
     */
    public void setListeners(final List<AntBuilder.Listener> listeners) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "18ba1d12-edd0-4d76-907d-7c9aa5e1fc1c");
        this.listeners = listeners;
    }

    /**
     * @param target The target to set.
     */
    public void setTarget(String target) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "3bf18e19-ec36-49e5-ae61-c3451df076ee");
        this.target = target;
    }

    /**
     * @return Returns the exitCode.
     */
    public int getExitCode() {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "932e5ab3-fad2-468e-a313-910dc52b2c1a");
        return exitCode;
    }

    /**
     * @param exitCode The exitCode to set.
     */
    public void setExitCode(int exitCode) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "79ef60a8-aec0-4bf9-9a19-5de79f2438f7");
        this.exitCode = exitCode;
    }

    /**
     * @param propertyFile The properties file to set.
     */
    public void setPropertyFile(String propertyFile) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "fe6475da-6120-4fee-9153-aeae5054fb4e");
        this.propertyfile = propertyFile;
    }

    /**
     * @param progressLoggerLib The directory containing the AntProgressLogger/Listener classes.
     */
    public void setProgressLoggerLib(final String progressLoggerLib) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "bf46a84e-3aef-49b8-aaa2-c11fb76fe32a");
        this.progressLoggerLib = progressLoggerLib;
    }

    /**
     * @param progress The progress callback object to set.
     */
    public void setProgress(final Progress progress) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "e4a7e893-5452-43f2-b8a2-9e72773484ff");
        this.progress = progress;
    }

    /**
     * @param env
     * The environment variables of the ant script, or <code>null</code> if to
     * inherit the environment of the current process.
     */
    public void setAntEnv(final OSEnvironment env) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_1_10.coverage", "c26b6ab1-5f80-4d95-8f57-bbdbbcdc7be4");
        this.env = env;
    }

    // setAntEnv
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
