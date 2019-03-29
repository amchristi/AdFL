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
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "5f8bc04a-04ba-4290-8dd2-a4fd2e3b50db");
        final Commandline cmdLine = new Commandline();
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "3666642c-2581-4cf5-99d1-30d20f3a9fdc");
        if (useScript) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "4583da09-5812-4041-85b1-0e218b9c6406");
            cmdLine.setExecutable(antScript);
        } else {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "59eb5d37-e0d6-4eb6-b5c4-47b08d2303fb");
            if (isWindows) {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "806088cc-a494-49b1-88da-33ab3ad013da");
                cmdLine.setExecutable("java.exe");
            } else {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "2cb87d56-a970-41b2-9ae2-aa2864b09596");
                cmdLine.setExecutable("java");
            }
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "f954d434-f1b1-41e1-992c-4fd519d093cc");
            for (final AntBuilder.JVMArg jvmArg : args) {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "df5912c5-d392-4e75-9e69-0c65b28a47f6");
                final String arg = jvmArg.getArg();
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "7727290c-260a-4c72-8197-e6f21584fdd0");
                // empty args may break the command line
                if (arg != null && arg.length() > 0) {
                    writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "c7134278-60ed-4491-befe-91ec946c1ff1");
                    cmdLine.createArgument(arg);
                }
            }
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "044abcff-f661-4f2d-86e8-5f82b2a637ec");
            final List<String> classpathItems = getClasspathItems(systemClassPath, isWindows);
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "dd14b7dc-c083-4bd0-830d-ac58833cd114");
            final String antLauncherJarLocation = getAntLauncherJarLocation(systemClassPath, classpathItems);
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "93b47c63-f3d6-4e04-9f41-0c7ad1f7a5d2");
            cmdLine.createArguments("-classpath", antLauncherJarLocation);
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "4ed17293-caa9-410a-9b75-908c7a9d61a8");
            cmdLine.createArgument("org.apache.tools.ant.launch.Launcher");
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "e2c08154-8f13-4d56-99bc-a15839d75cc5");
            cmdLine.createArguments("-lib", removeSaxonJars(classpathItems, isWindows));
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "7e500ebe-446f-46e2-aaaf-7ee595fe5d1d");
        if (progress == null) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "6e82605e-ae64-4485-b289-aef25e324e44");
            if (useLogger) {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "e7f089f3-faa4-4690-bc88-47dc12770aed");
                cmdLine.createArguments("-logger", getLoggerClassName());
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "fc0cb444-71d1-4b72-abc0-ff07aad83185");
                cmdLine.createArguments("-logfile", tempFileName);
            } else {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "86174995-0b2b-4fef-94b3-415abb809595");
                cmdLine.createArguments("-listener", getLoggerClassName());
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "35aa7ab6-86f1-4632-ae85-5328b3fb99c5");
                cmdLine.createArgument("-DXmlLogger.file=" + tempFileName);
            }
        } else {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "9fd2564b-454b-4324-a6d3-faaf73682740");
            // need to showProgress
            // use proper default logger if loggerClassName was not specified by config
            setupResolvedLoggerClassname();
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "f8f7b097-90ca-4994-b4b9-4c552448d59d");
            cmdLine.createArguments("-logger", getLoggerClassName());
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "05d748db-8fda-4601-bffe-0fb60406afdb");
            if (useLogger) {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "213aaa95-70a4-4b06-92e7-b507476263d1");
                // need to use AntProgressXmlLogger as a listener
                cmdLine.createArguments("-listener", CLASSNAME_ANTPROGRESS_XML_LISTENER);
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "b9811519-39aa-40ef-992d-b25a01528b40");
                cmdLine.createArgument("-DXmlLogger.file=" + tempFileName);
            } else {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "0dc4e388-7fed-4c48-b4e1-a326dc218e38");
                cmdLine.createArguments("-listener", AntBuilder.DEFAULT_LOGGER);
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "68f0422b-daeb-48cd-acc8-23bc731ab0af");
                cmdLine.createArgument("-DXmlLogger.file=" + tempFileName);
            }
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "0e0038b6-63e8-40fb-afe9-a180d0d308d0");
        if (AntBuilder.shouldAddDashboardLoggerJarToCommandLine(showAntOutput, useLogger)) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "4fbe0928-2b1e-41d5-baaf-32884188336d");
            cmdLine.createArguments("-listener", CLASSNAME_DASHBOARD_LISTENER);
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "e00bd4fb-9b94-4fa8-9546-cc6d2edac04a");
        if ((progress != null) || AntBuilder.shouldAddDashboardLoggerJarToCommandLine(showAntOutput, useLogger)) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "b3771662-28ce-426b-b7fa-961fa24c2ef9");
            // we need to add the custom logger jar {@link #LIBNAME_PROGRESS_LOGGER cruisecontrol-antprogresslogger.jar}
            // to the ant VM class path as a lib
            setupDefaultProgressLoggerLib();
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "b67b23b3-2780-4df6-889e-82bfb3137fcb");
            // add -lib to progressLogger classes
            cmdLine.createArguments("-lib", progressLoggerLib);
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "a9ed8ba5-a3e1-431a-9bbd-d1b7628cfaa1");
        // a listener, they will affect the default logger that writes to the console
        if (useDebug) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "13fe91d8-f7e2-413c-9e59-6019c8f9659c");
            cmdLine.createArgument("-debug");
        } else if (useQuiet) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "ff732f8a-41c2-436a-a083-6f74a393fd87");
            cmdLine.createArgument("-quiet");
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "dd26effb-7b0c-4a2c-91ad-efb69a243138");
        if (keepGoing) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "8f4fbaa5-13bf-444c-a1f5-98ff2f3e29e3");
            cmdLine.createArgument("-keep-going");
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "b18d9a15-10ae-41c1-92b8-aaf5c6e34870");
        for (final AntBuilder.Lib lib : libs) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "7c8de483-de49-4c81-b68d-0d92c4123ea2");
            cmdLine.createArguments("-lib", lib.getSearchPath());
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "426d6ce8-f81c-4d17-9b18-06b21e744fd1");
        for (final AntBuilder.Listener listener : listeners) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "b170fc9f-8145-4212-bb46-f3c15e13daf3");
            cmdLine.createArguments("-listener", listener.getClassName());
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "93611129-0964-402e-81fc-5696362c1150");
        for (final Map.Entry property : buildProperties.entrySet()) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "8f09637d-26ac-4c80-bf63-198792421b57");
            final String value = (String) property.getValue();
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "726cb637-2dd2-4404-909e-ebc5b81e4026");
            if (!"".equals(value)) {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "f7b43c4d-cc18-4901-8499-e65787e0b30a");
                cmdLine.createArgument("-D" + property.getKey() + "=" + value);
            }
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "0e104d8b-2ce4-4c58-935c-a7a3a25b5f2a");
        for (final Property property : properties) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "36f50ae9-5547-4e1c-8715-caff7ec223b0");
            cmdLine.createArgument("-D" + property.getName() + "=" + property.getValue());
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "69a87d16-1068-4c70-8120-871cb923914f");
        if (propertyfile != null) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "ce0e6522-1dc8-4cb1-9ad1-eff4c515046b");
            cmdLine.createArguments("-propertyfile", propertyfile);
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "93f201f2-a987-42a1-9c10-1c2790aa0125");
        cmdLine.createArguments("-buildfile", buildFile);
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "1b5b986f-889d-4838-88d7-fa3b505c50ec");
        cmdLine.setEnv(env);
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "0483dc7b-2cac-4d79-bf2d-620836f834af");
        final StringTokenizer targets = new StringTokenizer(target);
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "17e71b07-7157-495f-b40a-b3f907f3a19c");
        while (targets.hasMoreTokens()) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "8f1e92d2-8f0c-4c2b-9ea3-2180ff69f1ca");
            cmdLine.createArgument(targets.nextToken());
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "87d26307-c304-4862-a1d0-a51736a431e1");
        return cmdLine;
    }

    /**
     * @param path the classpath in which to search for the ant-launcher.jar
     * @param isWindows true if running on Windows
     * @return the path to ant-launcher*.jar taken from the given path
     * @throws CruiseControlException if path to ant-launcher.jar could not be found.
     */
    String getAntLauncherJarLocation(final String path, final boolean isWindows) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "f1cf60e4-21c0-42f9-b6b3-db0f36dd6515");
        return getAntLauncherJarLocation(path, getClasspathItems(path, isWindows));
    }

    /**
     * @param path the classpath as a single string, used here only for error message.
     * @param classpathItems the classpath items to search for the ant-launcher.jar
     * @return the path to ant-launcher*.jar taken from the given path
     * @throws CruiseControlException if path to ant-launcher.jar could not be found.
     */
    private String getAntLauncherJarLocation(final String path, final List<String> classpathItems) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "04c376f3-ce0b-4ead-ac21-6621de0c1180");
        for (final String pathElement : classpathItems) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "4ae4d726-881b-448e-b83b-98215af318e0");
            if (pathElement.indexOf("ant-launcher") != -1 && pathElement.endsWith(".jar")) {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "ed5731a2-04c3-4589-be5a-4cde32e8c225");
                return pathElement;
            }
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "06c654c0-5a4f-4586-90a1-19dd5657b96c");
        throw new CruiseControlException("Couldn't find path to ant-launcher jar in this classpath: '" + path + "'");
    }

    /**
     * @param path the classpath to split each element into a List
     * @param isWindows true if running on Windows
     * @return a List containing each element in the classpath
     */
    List<String> getClasspathItems(final String path, final boolean isWindows) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "689505dd-234c-42c6-8cd4-4aebcf2f838d");
        final List<String> ret = new ArrayList<String>();
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "414d9f99-56d5-4e55-b820-d7857c493ffd");
        final String separator = getSeparator(isWindows);
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "fff1c8f2-8995-452d-9c16-5f30252264d0");
        final StringTokenizer pathTokenizer = new StringTokenizer(path, separator);
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "05ee75eb-1458-493c-884e-3ac7333c90b1");
        while (pathTokenizer.hasMoreTokens()) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "0139a0aa-cc59-41d5-a576-8ef6af7bd7fa");
            final String pathElement = pathTokenizer.nextToken();
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "17580f48-aac2-4b56-9338-cbfe653feaae");
            ret.add(pathElement);
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "8f7218b5-3963-4edf-8220-0ccde8b6fa84");
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
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "1b3005f2-b56a-4e0d-b254-117d5b2db15e");
        final StringBuilder path = new StringBuilder();
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "f704d09b-502d-4dbc-912e-717dd8d8cdaf");
        final String separator = getSeparator(isWindows);
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "3906b9ea-d30f-4c70-8be5-e9e2bac7a935");
        for (final String pathElement : classpathItems) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "aa2fae03-a866-4428-a5d5-94a7a55eff6b");
            final File elementFile = new File(pathElement);
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "be5806ac-9d81-49c1-b36e-ea11eecf2940");
            if (!elementFile.getName().startsWith("saxon")) {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "3baa1b68-fb04-4a87-a463-f24a9951e9fe");
                if (path.length() > 0) {
                    writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "6d9b464c-afcf-4e7c-aefa-042304e0156f");
                    path.append(separator);
                }
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "a7fcb943-47da-4a7c-9443-4caae07fd824");
                path.append(pathElement);
            }
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "75cc7799-3b8e-48a5-88ea-1e05caaef4bb");
        return path.toString();
    }

    String removeSaxonJars(final String path, final boolean isWindows) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "14bac21b-757f-4a0b-b65c-df94dd1f4054");
        return removeSaxonJars(getClasspathItems(path, isWindows), isWindows);
    }

    private String getSeparator(boolean isWindows) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "a3e6827d-c920-4e01-8a0b-df131e0c7bff");
        return isWindows ? ";" : ":";
    }

    void setupResolvedLoggerClassname() {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "5c928100-f531-4893-865a-e637fdb6d777");
        // use proper default logger if loggerClassName was not specified by config
        if ((progress != null) && (!isLoggerClassNameSet)) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "6a7d3bb8-0cdb-40dd-bf27-773b6ba99dd8");
            if (useLogger) {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "3ccf741c-94f4-40c0-a2e6-bc8da7e504cb");
                // use string to avoid dependence on ant classes
                loggerClassName = CLASSNAME_ANTPROGRESS_XML_LOGGER;
            } else {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "7e546417-7a72-49fa-b6d8-d84015f06d37");
                // use string to avoid dependence on ant classes
                loggerClassName = CLASSNAME_ANTPROGRESS_LOGGER;
            }
        } else {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "fc79d3b4-e6ac-42f3-8431-d997cee77360");
            if (progress != null) {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "8bc5c58d-a483-4669-838f-02621b452d4e");
                LOG.warn("Ant Progress support is enabled AND loggerClassname is set. " + "Be sure the loggerClassName: " + loggerClassName + " is compatible with" + " Ant Progress.");
            }
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "e0ddf904-3e97-418c-86de-ce3f2ce68074");
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
        writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "a91507a7-cd7f-4f77-81a4-d6427c0dcbd2");
        // find path (including filename) to progressLoggerLib jar
        final String progressLoggerLib;
        writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "55b6feb4-352e-4b6e-9e54-d9cdd426d516");
        final File ccMain = UtilLocator.getClassSource(AntScript.class);
        writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "42d70f03-965d-4807-8558-50d942d42f90");
        if (ccMain == null) {
            writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "0e070134-560b-435e-a7f3-b0689cfa81ef");
            throw new ProgressLibLocatorException("Could not determine -lib path for progressLoggerLib. (Java 6/Webstart issue?) " + MSG_RESOLUTION_PROGRESS_LOGGER_LIB);
        } else {
            writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "cd5791f7-a2d1-42c8-ae6c-cf6a95bceb63");
            final String pathToDirContainingCCMainJar;
            writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "97b2348b-83d6-4e88-9bb0-3ddaf70b2215");
            if (ccMain.isDirectory()) {
                writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "79426d04-18ee-457d-bb4a-c4c926f09415");
                pathToDirContainingCCMainJar = ccMain.getAbsolutePath();
            } else {
                writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "e74980f6-7cbf-47cb-88b1-cc571cffdcac");
                pathToDirContainingCCMainJar = ccMain.getParentFile().getAbsolutePath();
            }
            writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "a8357d3b-1f14-4a45-87f8-79ea8219e5c6");
            final File expectedProgressLoggerJar = new File(pathToDirContainingCCMainJar, LIBNAME_PROGRESS_LOGGER);
            writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "0a00712a-6001-4037-af58-23c1fa16dc19");
            if (expectedProgressLoggerJar.exists()) {
                writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "9496fab2-c952-47f8-a95f-26b9d47a4509");
                // Use the specific jar if that jar exists.
                // This is a bit of a hack to load the progress logger jar into
                // ant without loading other jars (such as, ant.jar for instance)
                progressLoggerLib = expectedProgressLoggerJar.getAbsolutePath();
            } else {
                writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "ee75063f-5fc3-4ab7-97bc-f172aa4c723e");
                // Missing Progress Logger Lib is nasty to debug, so error out here if we can't find it for sure.
                throw new ProgressLibLocatorException("The progressLoggerLib jar file does not exist where expected: " + expectedProgressLoggerJar.getAbsolutePath() + MSG_RESOLUTION_PROGRESS_LOGGER_LIB);
            }
        }
        writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "a78eaba6-2113-490f-a8b2-ba038343ff0e");
        return progressLoggerLib;
    }

    void setupDefaultProgressLoggerLib() throws ProgressLibLocatorException {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "a9190ece-0aa1-4ecd-a92d-c59b3c71bce1");
        if (progressLoggerLib == null) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "c3d5f812-c1c7-45e1-9a29-ba6ff9d2f147");
            // Use a valid default for progressLoggerLib
            progressLoggerLib = findDefaultProgressLoggerLib();
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "bdc8195c-81d0-46bb-983d-12611a9e381d");
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
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "9f017e1d-020b-4d1f-b2a1-e4d9ae13faa3");
        if (progress != null && line != null && line.startsWith(MSG_PREFIX_ANT_PROGRESS)) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "f94a5363-2a25-4277-8512-e022515548fa");
            progress.setValue(line.substring(MSG_PREFIX_ANT_PROGRESS.length()));
        }
    }

    /**
     * @param buildProperties The buildProperties to set.
     */
    public void setBuildProperties(final Map<String, String> buildProperties) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "6f20f0f4-584a-47ec-b87e-3a231c0c8be2");
        this.buildProperties = buildProperties;
    }

    /**
     * @return Returns the loggerClassName.
     */
    public String getLoggerClassName() {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "b3df48c7-14ed-4f5a-90a6-0f25d96ddb77");
        return loggerClassName;
    }

    /**
     * @param loggerClassName The loggerClassName to set.
     */
    public void setLoggerClassName(String loggerClassName) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "16c33a98-0688-4879-b66c-809aae0d4c54");
        this.loggerClassName = loggerClassName;
    }

    /**
     * @param isLoggerClassNameSet The loggerClassName to set.
     */
    public void setIsLoggerClassNameSet(boolean isLoggerClassNameSet) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "806a9f48-7143-4706-b40d-511b843509d9");
        this.isLoggerClassNameSet = isLoggerClassNameSet;
    }

    /**
     * @param showAntOutput if true use Dashboard AntOutputLogger (CLASSNAME_DASHBOARD_LISTENER) as listener IIF
     * useLogger is also true
     */
    public void setShowAntOutput(final boolean showAntOutput) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "cf280482-6ff6-45ac-97b9-e880253b45a6");
        this.showAntOutput = showAntOutput;
    }

    /**
     * @param antScript The antScript to set.
     */
    public void setAntScript(String antScript) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "d22f3bb6-aa77-48e6-b6b6-897ab687864d");
        this.antScript = antScript;
    }

    /**
     * @param args The args to set.
     */
    public void setArgs(final List<AntBuilder.JVMArg> args) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "76c264dc-44d9-49b0-9086-2729a8572632");
        this.args = args;
    }

    /**
     * @param isWindows The isWindows to set.
     */
    public void setWindows(boolean isWindows) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "496051cc-9524-4e5f-bf92-fda5c4a77045");
        this.isWindows = isWindows;
    }

    /**
     * @param buildFile The buildFile to set.
     */
    public void setBuildFile(String buildFile) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "e7845a88-45d2-4eb0-b053-22ec58d24d11");
        this.buildFile = buildFile;
    }

    /**
     * @param tempFileName The tempFileName to set.
     */
    public void setTempFileName(String tempFileName) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "e8304fd8-9885-4d84-9bfb-add84d2b0285");
        this.tempFileName = tempFileName;
    }

    /**
     * @param useDebug The useDebug to set.
     */
    public void setUseDebug(boolean useDebug) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "479dbc64-7261-4f27-ac64-74c5291c876d");
        this.useDebug = useDebug;
    }

    /**
     * @param useLogger The useLogger to set.
     */
    public void setUseLogger(boolean useLogger) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "8f611e25-eb77-4f0f-801f-d0f4ed02b6b5");
        this.useLogger = useLogger;
    }

    /**
     * @param useQuiet The useQuiet to set.
     */
    public void setUseQuiet(boolean useQuiet) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "a3266db6-6345-4199-ac29-e14219d11d55");
        this.useQuiet = useQuiet;
    }

    public void setKeepGoing(boolean keepGoing) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "a7da33f9-17d7-46ff-b425-5f0b03d3f0ab");
        this.keepGoing = keepGoing;
    }

    /**
     * @param useScript The useScript to set.
     */
    public void setUseScript(boolean useScript) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "07469691-eacd-45ac-92d2-6aaf83e4b339");
        this.useScript = useScript;
    }

    /**
     * @param systemClassPath The systemClassPath to set.
     */
    public void setSystemClassPath(String systemClassPath) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "6603939d-c4e8-47a3-9ed6-e1b8e9274807");
        this.systemClassPath = systemClassPath;
    }

    /**
     * @param properties The properties to set.
     */
    public void setProperties(final List<Property> properties) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "a8f3c91d-62f4-4efa-bbe7-b9fd81d7d19c");
        this.properties = properties;
    }

    /**
     * @param libs The set of library paths to use.
     */
    public void setLibs(final List<AntBuilder.Lib> libs) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "2c9d20e2-682b-4cd9-80f5-6fdaab29520c");
        this.libs = libs;
    }

    /**
     * @param listeners The set of listener classes to use.
     */
    public void setListeners(final List<AntBuilder.Listener> listeners) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "90368746-5301-45cd-8120-a904da73ee44");
        this.listeners = listeners;
    }

    /**
     * @param target The target to set.
     */
    public void setTarget(String target) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "9a99b062-3f97-46f2-ae31-9039c436c729");
        this.target = target;
    }

    /**
     * @return Returns the exitCode.
     */
    public int getExitCode() {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "41662b5e-8b81-48e5-9147-b9bf1a8e863e");
        return exitCode;
    }

    /**
     * @param exitCode The exitCode to set.
     */
    public void setExitCode(int exitCode) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "db6ae006-74c0-4988-b11d-878512ebbb21");
        this.exitCode = exitCode;
    }

    /**
     * @param propertyFile The properties file to set.
     */
    public void setPropertyFile(String propertyFile) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "1412ada9-4229-44b8-992d-64816f384551");
        this.propertyfile = propertyFile;
    }

    /**
     * @param progressLoggerLib The directory containing the AntProgressLogger/Listener classes.
     */
    public void setProgressLoggerLib(final String progressLoggerLib) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "31b6dfaf-629e-43ee-b2be-611b9e391145");
        this.progressLoggerLib = progressLoggerLib;
    }

    /**
     * @param progress The progress callback object to set.
     */
    public void setProgress(final Progress progress) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "a399376f-c61c-4526-b21d-7a7476f36fdf");
        this.progress = progress;
    }

    /**
     * @param env
     * The environment variables of the ant script, or <code>null</code> if to
     * inherit the environment of the current process.
     */
    public void setAntEnv(final OSEnvironment env) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_3_10.coverage", "b7985e2c-8141-410e-823f-58dbbbdc6d12");
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
