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
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "d1d66fdb-73a6-457f-89e1-3a0497c216ae");
        final Commandline cmdLine = new Commandline();
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "79416b39-4ec6-4a35-aae1-8b92e1e05b1e");
        if (useScript) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "b1e826de-cc16-40fa-8f2c-2512a741b023");
            cmdLine.setExecutable(antScript);
        } else {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "f34d07be-15d2-4d71-89e7-f2dcdf5a6231");
            if (isWindows) {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "27e621cb-fcf7-4b8a-bb0c-e6680ce71309");
                cmdLine.setExecutable("java.exe");
            } else {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "06623c39-f1d6-46e4-beec-054ea706cfa4");
                cmdLine.setExecutable("java");
            }
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "c41ac452-ba77-4b9f-9764-fed97eea4990");
            for (final AntBuilder.JVMArg jvmArg : args) {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "98ea797f-fb3a-49fe-a814-59667ee4ec98");
                final String arg = jvmArg.getArg();
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "5053c532-7772-4e4f-a50d-1b782c0477de");
                // empty args may break the command line
                if (arg != null && arg.length() > 0) {
                    writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "2d65caa5-c13c-41f8-90b2-bfdca0b4d337");
                    cmdLine.createArgument(arg);
                }
            }
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "e0f58992-9877-4dc8-87aa-51683f7e52d2");
            final List<String> classpathItems = getClasspathItems(systemClassPath, isWindows);
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "358f312f-92c8-4b78-8efc-928c3f5f0d94");
            final String antLauncherJarLocation = getAntLauncherJarLocation(systemClassPath, classpathItems);
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "05780da6-75db-42fa-9a94-8d05981aa5ec");
            cmdLine.createArguments("-classpath", antLauncherJarLocation);
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "a8ab8531-741d-4464-b05a-4c90591b7f62");
            cmdLine.createArgument("org.apache.tools.ant.launch.Launcher");
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "4be1e6df-3788-47d1-892e-bafeae350633");
            cmdLine.createArguments("-lib", removeSaxonJars(classpathItems, isWindows));
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "260f7d04-c9d0-4921-9e2b-d330b14d1393");
        if (progress == null) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "5fd1cd48-4e45-494a-82f6-4ab32ba9809c");
            if (useLogger) {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "9bc82552-9804-4137-8916-c901f27a8ed7");
                cmdLine.createArguments("-logger", getLoggerClassName());
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "2105c27b-591d-4363-8bf7-ed148756f43c");
                cmdLine.createArguments("-logfile", tempFileName);
            } else {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "57a9892b-68a7-4877-867a-2680130c12ac");
                cmdLine.createArguments("-listener", getLoggerClassName());
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "0dccfa35-d23d-472d-ad00-15fb53e5ad4f");
                cmdLine.createArgument("-DXmlLogger.file=" + tempFileName);
            }
        } else {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "bc28418f-7a9e-41e6-b6a6-bdbc2ce7f5d6");
            // need to showProgress
            // use proper default logger if loggerClassName was not specified by config
            setupResolvedLoggerClassname();
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "1a54c7e2-9be4-4379-8342-3fd9d4912758");
            cmdLine.createArguments("-logger", getLoggerClassName());
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "107f5e30-104e-4e25-a62d-fb75b5908d21");
            if (useLogger) {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "0b5eb71c-9042-42c9-922c-0bfaee6ae5a6");
                // need to use AntProgressXmlLogger as a listener
                cmdLine.createArguments("-listener", CLASSNAME_ANTPROGRESS_XML_LISTENER);
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "ad66174d-a077-420c-8094-a733ff501964");
                cmdLine.createArgument("-DXmlLogger.file=" + tempFileName);
            } else {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "5f74333b-c9fc-4046-9ebd-e136b1472973");
                cmdLine.createArguments("-listener", AntBuilder.DEFAULT_LOGGER);
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "cce23808-dc12-4b2e-b9a7-80b1201a81be");
                cmdLine.createArgument("-DXmlLogger.file=" + tempFileName);
            }
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "646ff950-e492-4e99-90aa-2f242a375e24");
        if (AntBuilder.shouldAddDashboardLoggerJarToCommandLine(showAntOutput, useLogger)) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "1c3c1140-6485-4056-ae33-c500ff9674af");
            cmdLine.createArguments("-listener", CLASSNAME_DASHBOARD_LISTENER);
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "338cd9ac-7982-48af-b18d-865e10b51aa7");
        if ((progress != null) || AntBuilder.shouldAddDashboardLoggerJarToCommandLine(showAntOutput, useLogger)) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "39ce9dd3-4e12-4248-b33c-3cecc2d345c8");
            // we need to add the custom logger jar {@link #LIBNAME_PROGRESS_LOGGER cruisecontrol-antprogresslogger.jar}
            // to the ant VM class path as a lib
            setupDefaultProgressLoggerLib();
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "cfa4a2e0-8b3f-4258-9880-e3ff48e4355d");
            // add -lib to progressLogger classes
            cmdLine.createArguments("-lib", progressLoggerLib);
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "958b07e1-0cea-4d92-af01-e09d3984fe7e");
        // a listener, they will affect the default logger that writes to the console
        if (useDebug) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "e696442b-eb04-4ce6-b18e-6dc9312bf613");
            cmdLine.createArgument("-debug");
        } else if (useQuiet) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "8b172a0a-62f1-47fe-a1c5-1b4bd75eab7a");
            cmdLine.createArgument("-quiet");
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "3cf4f5b1-9e21-4277-8a98-84b43720177f");
        if (keepGoing) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "2ac77776-5ca9-4f6d-a62b-a5b94452b556");
            cmdLine.createArgument("-keep-going");
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "f2cb1ec4-eb3d-4998-913a-83161090a46f");
        for (final AntBuilder.Lib lib : libs) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "29a40473-2162-47db-904c-6ce1fef2b04a");
            cmdLine.createArguments("-lib", lib.getSearchPath());
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "c5e52862-26e6-464a-83b7-e14f00bc1968");
        for (final AntBuilder.Listener listener : listeners) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "daf91d38-0363-45c1-a419-3251261ac7c5");
            cmdLine.createArguments("-listener", listener.getClassName());
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "271808fc-e6ec-4d1f-bba7-59c16021791e");
        for (final Map.Entry property : buildProperties.entrySet()) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "ddc5810a-cff5-4d9e-ba22-6b401ae1786b");
            final String value = (String) property.getValue();
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "29b44542-526f-4010-994f-aff28342ea8f");
            if (!"".equals(value)) {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "d04738bc-1ca2-4831-808e-72571cc8d8ca");
                cmdLine.createArgument("-D" + property.getKey() + "=" + value);
            }
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "1e9983b6-6411-4dff-9d1b-0a2ce38cf6c2");
        for (final Property property : properties) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "8cff9c44-58c7-441e-83c3-41677b017b2f");
            cmdLine.createArgument("-D" + property.getName() + "=" + property.getValue());
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "196582cb-1ee9-4118-9a15-4ac7c49cd746");
        if (propertyfile != null) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "4249212a-15a6-4a8d-89c5-2c1d79046f9a");
            cmdLine.createArguments("-propertyfile", propertyfile);
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "4cab9c24-a4b3-45a0-9039-b5a364e7e0ec");
        cmdLine.createArguments("-buildfile", buildFile);
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "97ba6ad6-8563-4507-b005-863606415215");
        cmdLine.setEnv(env);
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "26ce923f-da92-425f-9c8a-09e6a8ca395b");
        final StringTokenizer targets = new StringTokenizer(target);
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "886d54d7-9d2a-428c-a5fa-cc3153b4dcfc");
        while (targets.hasMoreTokens()) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "af57a18b-8b4f-4cb2-a199-bbf4ba27bcfd");
            cmdLine.createArgument(targets.nextToken());
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "b7067767-8aaa-4745-b99b-9e7f0b86a19e");
        return cmdLine;
    }

    /**
     * @param path the classpath in which to search for the ant-launcher.jar
     * @param isWindows true if running on Windows
     * @return the path to ant-launcher*.jar taken from the given path
     * @throws CruiseControlException if path to ant-launcher.jar could not be found.
     */
    String getAntLauncherJarLocation(final String path, final boolean isWindows) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "fd3e11a5-5757-440c-a2a7-ffec359d4bae");
        return getAntLauncherJarLocation(path, getClasspathItems(path, isWindows));
    }

    /**
     * @param path the classpath as a single string, used here only for error message.
     * @param classpathItems the classpath items to search for the ant-launcher.jar
     * @return the path to ant-launcher*.jar taken from the given path
     * @throws CruiseControlException if path to ant-launcher.jar could not be found.
     */
    private String getAntLauncherJarLocation(final String path, final List<String> classpathItems) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "0eea5f11-eb9f-471f-8630-6fdfdfbd552a");
        for (final String pathElement : classpathItems) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "4e78ccbf-2933-4ba3-bc60-518772a76c9b");
            if (pathElement.indexOf("ant-launcher") != -1 && pathElement.endsWith(".jar")) {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "d4c659c3-aa99-440c-9469-cce976487c30");
                return pathElement;
            }
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "83a34727-4017-4707-b484-52cf92fa2f4c");
        throw new CruiseControlException("Couldn't find path to ant-launcher jar in this classpath: '" + path + "'");
    }

    /**
     * @param path the classpath to split each element into a List
     * @param isWindows true if running on Windows
     * @return a List containing each element in the classpath
     */
    List<String> getClasspathItems(final String path, final boolean isWindows) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "16c4c525-af4e-448a-917d-84b37ecfb8be");
        final List<String> ret = new ArrayList<String>();
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "8f7ef1e8-344c-45cf-b022-b19c98fc3aa9");
        final String separator = getSeparator(isWindows);
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "e94b2bd1-c14a-4c7a-98d3-5206d200704f");
        final StringTokenizer pathTokenizer = new StringTokenizer(path, separator);
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "c5cc82e6-3721-4a30-a4cc-e5747a652f35");
        while (pathTokenizer.hasMoreTokens()) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "7bc36f9b-6c1f-4cf3-b6bf-b500ce450316");
            final String pathElement = pathTokenizer.nextToken();
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "cdcc0430-5dea-4012-bc2c-1cf8bcae3faa");
            ret.add(pathElement);
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "1be7ca10-706e-44f2-ad95-ee36f1c70dac");
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
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "1086accb-d0d2-40fb-baf0-c0155c985f99");
        final StringBuilder path = new StringBuilder();
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "51cc1e25-b5e9-4469-82b6-e6823a345e44");
        final String separator = getSeparator(isWindows);
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "d59c5489-57ce-4663-8801-972b38eb803a");
        for (final String pathElement : classpathItems) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "dc690b27-086c-4616-ae11-7b6c54931774");
            final File elementFile = new File(pathElement);
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "7aba0327-6fe2-4396-8efd-438010ae7cc3");
            if (!elementFile.getName().startsWith("saxon")) {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "7e585094-c8de-4cb5-b2d0-945f13c2f7b0");
                if (path.length() > 0) {
                    writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "cdd0d287-a70e-44c0-9676-c63729f1a93a");
                    path.append(separator);
                }
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "36979ffc-a74d-4a5a-a073-529671aa4916");
                path.append(pathElement);
            }
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "d0826def-d0d2-48d9-a0a2-15329365ff00");
        return path.toString();
    }

    String removeSaxonJars(final String path, final boolean isWindows) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "175a716e-8a31-4ea2-a60f-e1305440cf42");
        return removeSaxonJars(getClasspathItems(path, isWindows), isWindows);
    }

    private String getSeparator(boolean isWindows) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "37b6059c-cfcf-4079-9dbc-6c506c8e2495");
        return isWindows ? ";" : ":";
    }

    void setupResolvedLoggerClassname() {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "e465ed0a-e660-496b-a469-8b7b6074b76f");
        // use proper default logger if loggerClassName was not specified by config
        if ((progress != null) && (!isLoggerClassNameSet)) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "310b3a36-6ce2-4e75-81ff-0b3051048a86");
            if (useLogger) {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "88f3df06-ce3e-4fa7-94f2-6bf5a5b05883");
                // use string to avoid dependence on ant classes
                loggerClassName = CLASSNAME_ANTPROGRESS_XML_LOGGER;
            } else {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "63b3bd69-afc0-45a4-8569-55851244f56b");
                // use string to avoid dependence on ant classes
                loggerClassName = CLASSNAME_ANTPROGRESS_LOGGER;
            }
        } else {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "9a2f05f7-1d37-4dcb-956f-79eff1773981");
            if (progress != null) {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "3506e426-905b-4a47-ac84-7f5208faeb66");
                LOG.warn("Ant Progress support is enabled AND loggerClassname is set. " + "Be sure the loggerClassName: " + loggerClassName + " is compatible with" + " Ant Progress.");
            }
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "9c8bc654-c7be-445d-ac71-31fd608f523f");
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
        writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "9b7eb205-e92c-4cde-b3ea-8130a49898f9");
        // find path (including filename) to progressLoggerLib jar
        final String progressLoggerLib;
        writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "a3bd23b5-d74d-42d5-9036-20f9bb99b6c9");
        final File ccMain = UtilLocator.getClassSource(AntScript.class);
        writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "8527b90e-8e38-41b9-9758-e74a5226b7f7");
        if (ccMain == null) {
            writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "b4c9f70b-029a-458e-a61d-2ff65b921398");
            throw new ProgressLibLocatorException("Could not determine -lib path for progressLoggerLib. (Java 6/Webstart issue?) " + MSG_RESOLUTION_PROGRESS_LOGGER_LIB);
        } else {
            writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "45f48379-44d3-44e6-b5b2-10c6df07aa4f");
            final String pathToDirContainingCCMainJar;
            writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "8fdf81c8-eecc-4771-beec-2ae54c4ac1d5");
            if (ccMain.isDirectory()) {
                writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "a0059de3-b1c5-4578-be0b-62829e2a92a0");
                pathToDirContainingCCMainJar = ccMain.getAbsolutePath();
            } else {
                writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "1d64296d-9e96-4e8d-80f1-2393dfdc3d33");
                pathToDirContainingCCMainJar = ccMain.getParentFile().getAbsolutePath();
            }
            writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "6a2fd902-18fe-4b9e-8dbc-21db6ae04ab7");
            final File expectedProgressLoggerJar = new File(pathToDirContainingCCMainJar, LIBNAME_PROGRESS_LOGGER);
            writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "0b1ecb6d-5d3d-4731-8285-dd90445f0187");
            if (expectedProgressLoggerJar.exists()) {
                writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "59114d50-beb5-44ac-91b2-5abdc68d2f0f");
                // Use the specific jar if that jar exists.
                // This is a bit of a hack to load the progress logger jar into
                // ant without loading other jars (such as, ant.jar for instance)
                progressLoggerLib = expectedProgressLoggerJar.getAbsolutePath();
            } else {
                writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "85f97544-22b7-4f1a-bb65-bdc1196d8b75");
                // Missing Progress Logger Lib is nasty to debug, so error out here if we can't find it for sure.
                throw new ProgressLibLocatorException("The progressLoggerLib jar file does not exist where expected: " + expectedProgressLoggerJar.getAbsolutePath() + MSG_RESOLUTION_PROGRESS_LOGGER_LIB);
            }
        }
        writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "0feccba6-764f-403c-8802-bf6d9697b026");
        return progressLoggerLib;
    }

    void setupDefaultProgressLoggerLib() throws ProgressLibLocatorException {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "e389434d-40e4-488a-a50a-64b4fb1bda12");
        if (progressLoggerLib == null) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "8b3f2260-f627-4ccf-8ba9-ddee2b1eb6a7");
            // Use a valid default for progressLoggerLib
            progressLoggerLib = findDefaultProgressLoggerLib();
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "41c9ad8d-e1a4-4370-b684-dd080e6501f9");
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
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "da7ac6ce-eaa1-4e46-8975-6da95229c5ae");
        if (progress != null && line != null && line.startsWith(MSG_PREFIX_ANT_PROGRESS)) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "93af3c56-04d3-4119-8c11-dcc8e2a9eb01");
            progress.setValue(line.substring(MSG_PREFIX_ANT_PROGRESS.length()));
        }
    }

    /**
     * @param buildProperties The buildProperties to set.
     */
    public void setBuildProperties(final Map<String, String> buildProperties) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "12b548c4-568d-4486-b812-b844bb806afd");
        this.buildProperties = buildProperties;
    }

    /**
     * @return Returns the loggerClassName.
     */
    public String getLoggerClassName() {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "99f90db0-0467-4d2d-88e1-fbcd1a72b957");
        return loggerClassName;
    }

    /**
     * @param loggerClassName The loggerClassName to set.
     */
    public void setLoggerClassName(String loggerClassName) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "fa6157b4-2b93-486a-837c-90006a395f1d");
        this.loggerClassName = loggerClassName;
    }

    /**
     * @param isLoggerClassNameSet The loggerClassName to set.
     */
    public void setIsLoggerClassNameSet(boolean isLoggerClassNameSet) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "5c19be18-613c-4f9a-81bd-6a7b5fc2e1cb");
        this.isLoggerClassNameSet = isLoggerClassNameSet;
    }

    /**
     * @param showAntOutput if true use Dashboard AntOutputLogger (CLASSNAME_DASHBOARD_LISTENER) as listener IIF
     * useLogger is also true
     */
    public void setShowAntOutput(final boolean showAntOutput) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "148150a7-35df-4d23-9fce-f7c528f4da27");
        this.showAntOutput = showAntOutput;
    }

    /**
     * @param antScript The antScript to set.
     */
    public void setAntScript(String antScript) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "c66aee28-1c66-4a6f-ad28-c599933657cd");
        this.antScript = antScript;
    }

    /**
     * @param args The args to set.
     */
    public void setArgs(final List<AntBuilder.JVMArg> args) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "be22b29f-47cf-4667-be90-87e470da10bf");
        this.args = args;
    }

    /**
     * @param isWindows The isWindows to set.
     */
    public void setWindows(boolean isWindows) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "378538a2-2cf8-4884-8eac-40835dd90568");
        this.isWindows = isWindows;
    }

    /**
     * @param buildFile The buildFile to set.
     */
    public void setBuildFile(String buildFile) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "b91d2d90-dd7a-4fdf-a59f-dffc962be196");
        this.buildFile = buildFile;
    }

    /**
     * @param tempFileName The tempFileName to set.
     */
    public void setTempFileName(String tempFileName) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "c498f5bd-9359-401f-b5d9-c1e79a364097");
        this.tempFileName = tempFileName;
    }

    /**
     * @param useDebug The useDebug to set.
     */
    public void setUseDebug(boolean useDebug) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "02fb184e-1bab-4572-a4c1-509f08aa3705");
        this.useDebug = useDebug;
    }

    /**
     * @param useLogger The useLogger to set.
     */
    public void setUseLogger(boolean useLogger) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "b4020bd0-e439-41be-9625-f0ca82df35a9");
        this.useLogger = useLogger;
    }

    /**
     * @param useQuiet The useQuiet to set.
     */
    public void setUseQuiet(boolean useQuiet) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "e71f2e23-1c8c-4b25-aca4-40d0165ccad1");
        this.useQuiet = useQuiet;
    }

    public void setKeepGoing(boolean keepGoing) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "34b79384-ed0d-48da-9659-dff409d99dfc");
        this.keepGoing = keepGoing;
    }

    /**
     * @param useScript The useScript to set.
     */
    public void setUseScript(boolean useScript) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "3fbfbd14-61a6-4d93-b000-3bf18649d2fd");
        this.useScript = useScript;
    }

    /**
     * @param systemClassPath The systemClassPath to set.
     */
    public void setSystemClassPath(String systemClassPath) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "c67ab63e-207c-45d6-9b86-b70a56eb05d9");
        this.systemClassPath = systemClassPath;
    }

    /**
     * @param properties The properties to set.
     */
    public void setProperties(final List<Property> properties) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "7f811d87-90c7-4eb1-b9a1-8d20ef2e104f");
        this.properties = properties;
    }

    /**
     * @param libs The set of library paths to use.
     */
    public void setLibs(final List<AntBuilder.Lib> libs) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "6287ae64-8eb0-461d-936e-0a10623f5e21");
        this.libs = libs;
    }

    /**
     * @param listeners The set of listener classes to use.
     */
    public void setListeners(final List<AntBuilder.Listener> listeners) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "643b2e1f-f84a-4ded-a938-e2616d030f3d");
        this.listeners = listeners;
    }

    /**
     * @param target The target to set.
     */
    public void setTarget(String target) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "5ab734de-6a6d-43e7-b96d-2087a48eb63b");
        this.target = target;
    }

    /**
     * @return Returns the exitCode.
     */
    public int getExitCode() {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "a5a4bef5-91e2-4755-b6aa-83f37533e68e");
        return exitCode;
    }

    /**
     * @param exitCode The exitCode to set.
     */
    public void setExitCode(int exitCode) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "8353e291-4df6-40b1-ae57-75cd1a7cfcc9");
        this.exitCode = exitCode;
    }

    /**
     * @param propertyFile The properties file to set.
     */
    public void setPropertyFile(String propertyFile) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "aa97c871-f179-4423-ad11-46135c0d1b94");
        this.propertyfile = propertyFile;
    }

    /**
     * @param progressLoggerLib The directory containing the AntProgressLogger/Listener classes.
     */
    public void setProgressLoggerLib(final String progressLoggerLib) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "aa027d7d-300c-4403-b5af-29b2fe54151f");
        this.progressLoggerLib = progressLoggerLib;
    }

    /**
     * @param progress The progress callback object to set.
     */
    public void setProgress(final Progress progress) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "abb5d3f7-4d7e-4269-bf4d-a78c5b6d3c68");
        this.progress = progress;
    }

    /**
     * @param env
     * The environment variables of the ant script, or <code>null</code> if to
     * inherit the environment of the current process.
     */
    public void setAntEnv(final OSEnvironment env) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_2_10.coverage", "d2084878-4842-422d-bed1-7c4157105f47");
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
