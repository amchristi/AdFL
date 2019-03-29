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
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "a6e866e9-19be-49b5-8717-6546c33ff242");
        final Commandline cmdLine = new Commandline();
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "3417ec13-7561-4a0d-84aa-73d9d292033f");
        if (useScript) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "f04eb1b6-87f8-4e77-bcd5-960acdfacfef");
            cmdLine.setExecutable(antScript);
        } else {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "e7388166-cdf0-4300-919c-7b87bce7a761");
            if (isWindows) {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "395a3dda-8ea4-433d-bdbc-2394021930e4");
                cmdLine.setExecutable("java.exe");
            } else {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "ba6dede0-077b-4ec9-88a3-b07a7ee68ef4");
                cmdLine.setExecutable("java");
            }
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "47102949-6f9f-4dcd-85a4-39cd10591fb3");
            for (final AntBuilder.JVMArg jvmArg : args) {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "ee59e866-3cbe-4c7d-a95a-e0e7052ade69");
                final String arg = jvmArg.getArg();
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "24015840-d6e6-4cde-a176-95afbe39ad2c");
                // empty args may break the command line
                if (arg != null && arg.length() > 0) {
                    writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "2e4415f1-9604-4c53-9283-b627c06e8cdf");
                    cmdLine.createArgument(arg);
                }
            }
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "f39384c3-5b20-46b8-975b-6ee743e00baa");
            final List<String> classpathItems = getClasspathItems(systemClassPath, isWindows);
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "f4714cc1-9f40-435f-9c5b-81c279035475");
            final String antLauncherJarLocation = getAntLauncherJarLocation(systemClassPath, classpathItems);
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "4772479c-4634-4f3d-a294-a7a70f473ba5");
            cmdLine.createArguments("-classpath", antLauncherJarLocation);
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "71c6e3da-1645-4a15-82e4-95665c8a0f6f");
            cmdLine.createArgument("org.apache.tools.ant.launch.Launcher");
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "0a6c4848-e85f-42ce-9c82-181cd9b9654f");
            cmdLine.createArguments("-lib", removeSaxonJars(classpathItems, isWindows));
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "85e5c2ce-9aa2-438f-895f-02aa347883b7");
        if (progress == null) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "6fb74956-91d4-408c-88c4-414719f8225d");
            if (useLogger) {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "cd883886-c27e-46d2-be53-7501c889d8f3");
                cmdLine.createArguments("-logger", getLoggerClassName());
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "f86f5506-3960-4d0b-beed-87c90fad0827");
                cmdLine.createArguments("-logfile", tempFileName);
            } else {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "6b4d7938-7aab-464c-a8e3-7588e2eb6676");
                cmdLine.createArguments("-listener", getLoggerClassName());
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "53bc1e2c-a53e-481d-8c73-c2722a2a17d8");
                cmdLine.createArgument("-DXmlLogger.file=" + tempFileName);
            }
        } else {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "04327b9b-35b0-4854-96d5-c81106c50af0");
            // need to showProgress
            // use proper default logger if loggerClassName was not specified by config
            setupResolvedLoggerClassname();
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "9f9b1ba1-64e9-4958-8284-b778ed98782e");
            cmdLine.createArguments("-logger", getLoggerClassName());
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "f2bc1b76-0965-4354-939e-d6f191f8ca85");
            if (useLogger) {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "acbae0e5-ab89-449d-b132-6391204576c5");
                // need to use AntProgressXmlLogger as a listener
                cmdLine.createArguments("-listener", CLASSNAME_ANTPROGRESS_XML_LISTENER);
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "4017767f-f7d1-4e94-9ae7-a234ed1c25d7");
                cmdLine.createArgument("-DXmlLogger.file=" + tempFileName);
            } else {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "2fadbd42-3219-43ec-bf2e-4c60753a087d");
                cmdLine.createArguments("-listener", AntBuilder.DEFAULT_LOGGER);
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "1c4c6cfb-2d34-4e33-a1e1-bfd5d704569e");
                cmdLine.createArgument("-DXmlLogger.file=" + tempFileName);
            }
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "72f749b5-c9a7-4880-8bcd-b83e11749c1c");
        if (AntBuilder.shouldAddDashboardLoggerJarToCommandLine(showAntOutput, useLogger)) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "a0bf811d-084f-4553-adf9-36823a11bec6");
            cmdLine.createArguments("-listener", CLASSNAME_DASHBOARD_LISTENER);
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "f0debfe2-c792-4f43-81db-588b93b13778");
        if ((progress != null) || AntBuilder.shouldAddDashboardLoggerJarToCommandLine(showAntOutput, useLogger)) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "14c9071d-9635-432b-8839-eab346fde887");
            // we need to add the custom logger jar {@link #LIBNAME_PROGRESS_LOGGER cruisecontrol-antprogresslogger.jar}
            // to the ant VM class path as a lib
            setupDefaultProgressLoggerLib();
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "e43691cc-e5c0-4222-9019-cde90d3db586");
            // add -lib to progressLogger classes
            cmdLine.createArguments("-lib", progressLoggerLib);
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "de3d5d00-a24b-4048-bbc3-d58daa2fdb2b");
        // a listener, they will affect the default logger that writes to the console
        if (useDebug) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "0c7365a7-e4ef-4c84-bd1b-a7b547bb1da8");
            cmdLine.createArgument("-debug");
        } else if (useQuiet) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "c7ed0cf4-de7a-417c-98eb-9927cf6059a1");
            cmdLine.createArgument("-quiet");
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "d122b5ec-5c01-4ba3-9633-2fd895272aa7");
        if (keepGoing) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "f895c67b-69f6-4247-8ed3-e63182094c82");
            cmdLine.createArgument("-keep-going");
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "eebebc0b-8ca9-4945-988c-948f8a47be09");
        for (final AntBuilder.Lib lib : libs) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "f2985721-e71b-485b-b2f2-7d39a7e805e8");
            cmdLine.createArguments("-lib", lib.getSearchPath());
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "e4b67505-b69b-4755-8b2a-72c903d75141");
        for (final AntBuilder.Listener listener : listeners) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "2ffdec55-4905-4447-b4f2-c9685b33fe4e");
            cmdLine.createArguments("-listener", listener.getClassName());
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "34d17fdf-decb-4b78-8a29-953bbbc67cb4");
        for (final Map.Entry property : buildProperties.entrySet()) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "e236c3fa-48ed-47c6-ab24-9105d299f209");
            final String value = (String) property.getValue();
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "586044e2-8ad3-4799-afb5-d0602dd0d823");
            if (!"".equals(value)) {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "0119c9b3-d120-4653-be83-379a6066fb25");
                cmdLine.createArgument("-D" + property.getKey() + "=" + value);
            }
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "5e4c8404-09fa-406c-a667-329d0f4ed98e");
        for (final Property property : properties) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "c4beb5de-9795-4002-aff2-ed4d8e233f0a");
            cmdLine.createArgument("-D" + property.getName() + "=" + property.getValue());
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "e83d3e1d-8f2a-4950-8182-38a915678b37");
        if (propertyfile != null) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "40308bf4-7a7b-4cd2-9f68-c56000eb2d44");
            cmdLine.createArguments("-propertyfile", propertyfile);
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "b35eaa24-c50b-4d2e-a2b0-443ba650eb9a");
        cmdLine.createArguments("-buildfile", buildFile);
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "bc18cd82-aa16-4ce5-b4cb-29fcb72e9e36");
        cmdLine.setEnv(env);
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "d6579cb8-bb33-4ef7-a97c-0a39d7b81735");
        final StringTokenizer targets = new StringTokenizer(target);
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "335b0fa9-31e1-44e9-89ff-5780d0b898c3");
        while (targets.hasMoreTokens()) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "35bd49c3-ec06-4dcf-87ea-b3a873cbeb5c");
            cmdLine.createArgument(targets.nextToken());
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "d7efaeef-7797-4bf2-9832-0323be51d5a9");
        return cmdLine;
    }

    /**
     * @param path the classpath in which to search for the ant-launcher.jar
     * @param isWindows true if running on Windows
     * @return the path to ant-launcher*.jar taken from the given path
     * @throws CruiseControlException if path to ant-launcher.jar could not be found.
     */
    String getAntLauncherJarLocation(final String path, final boolean isWindows) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "735dce6c-275f-45ce-b227-b8e2bec8d4cf");
        return getAntLauncherJarLocation(path, getClasspathItems(path, isWindows));
    }

    /**
     * @param path the classpath as a single string, used here only for error message.
     * @param classpathItems the classpath items to search for the ant-launcher.jar
     * @return the path to ant-launcher*.jar taken from the given path
     * @throws CruiseControlException if path to ant-launcher.jar could not be found.
     */
    private String getAntLauncherJarLocation(final String path, final List<String> classpathItems) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "660a30e9-45e2-48e9-bae0-79669e44d7fd");
        for (final String pathElement : classpathItems) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "7da8dd33-0f8f-440a-9d53-51c1022965cd");
            if (pathElement.indexOf("ant-launcher") != -1 && pathElement.endsWith(".jar")) {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "adc318a5-b813-46f6-9657-69becebf72c9");
                return pathElement;
            }
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "fcfe1957-6b05-494c-9d79-94f73e4c3714");
        throw new CruiseControlException("Couldn't find path to ant-launcher jar in this classpath: '" + path + "'");
    }

    /**
     * @param path the classpath to split each element into a List
     * @param isWindows true if running on Windows
     * @return a List containing each element in the classpath
     */
    List<String> getClasspathItems(final String path, final boolean isWindows) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "f2bfa280-127a-48e4-9bf5-236eb5238885");
        final List<String> ret = new ArrayList<String>();
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "7c967e76-10ae-4516-91f1-599ed8a4ea12");
        final String separator = getSeparator(isWindows);
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "a45c3e9b-cba0-4e7b-83a5-2e50f105eb83");
        final StringTokenizer pathTokenizer = new StringTokenizer(path, separator);
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "27a85729-d70c-4bc1-82f7-e59d77eab113");
        while (pathTokenizer.hasMoreTokens()) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "c2263be5-906d-4f38-860e-537c8d4924d3");
            final String pathElement = pathTokenizer.nextToken();
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "380ec929-a86b-46e6-9236-eb6d845a4314");
            ret.add(pathElement);
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "bc00ea59-f90d-453f-ab06-196de86a7f84");
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
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "84f9efc8-97fd-47d9-9615-5e04536cd2aa");
        final StringBuilder path = new StringBuilder();
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "063d4f64-494b-4ea8-806d-92f4fb73c740");
        final String separator = getSeparator(isWindows);
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "7afc0053-f337-4105-a747-5b454266205a");
        for (final String pathElement : classpathItems) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "53c73058-caf3-4a13-a86d-4662189a9dbb");
            final File elementFile = new File(pathElement);
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "d4e38574-89cd-40e0-ad87-f98784a052fd");
            if (!elementFile.getName().startsWith("saxon")) {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "6eb75809-8501-4fd4-9baa-a4f03be42845");
                if (path.length() > 0) {
                    writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "9d1ef175-8416-45fd-9a35-89ced2c56aa6");
                    path.append(separator);
                }
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "ad15c301-07a8-4b58-b7cc-de6ec29bb8dd");
                path.append(pathElement);
            }
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "89a479d7-a6a4-4253-816a-0b3b9367a513");
        return path.toString();
    }

    String removeSaxonJars(final String path, final boolean isWindows) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "ed39f012-8d3b-469e-82a6-42f087714495");
        return removeSaxonJars(getClasspathItems(path, isWindows), isWindows);
    }

    private String getSeparator(boolean isWindows) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "e0bcac96-254a-4f00-bb0b-3288b55bbc93");
        return isWindows ? ";" : ":";
    }

    void setupResolvedLoggerClassname() {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "33158c0b-2238-4834-95b3-1eb922216e68");
        // use proper default logger if loggerClassName was not specified by config
        if ((progress != null) && (!isLoggerClassNameSet)) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "aeccdde4-459b-4179-9a0e-490f8d6b8730");
            if (useLogger) {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "0f5fcd68-856e-4be0-a545-507280b1c2e2");
                // use string to avoid dependence on ant classes
                loggerClassName = CLASSNAME_ANTPROGRESS_XML_LOGGER;
            } else {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "36f1d9ec-933a-4071-b048-b805e53969db");
                // use string to avoid dependence on ant classes
                loggerClassName = CLASSNAME_ANTPROGRESS_LOGGER;
            }
        } else {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "3b556ea1-fcce-4d94-a0da-aec1c7f1e2cc");
            if (progress != null) {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "8dd0e0a3-c792-4201-8795-ee048f40efd6");
                LOG.warn("Ant Progress support is enabled AND loggerClassname is set. " + "Be sure the loggerClassName: " + loggerClassName + " is compatible with" + " Ant Progress.");
            }
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "174f1288-174f-4717-b909-1badd12638a3");
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
        writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "b803dfbd-6a6a-4e24-9124-3408b9383cf8");
        // find path (including filename) to progressLoggerLib jar
        final String progressLoggerLib;
        writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "d18b9c40-4bbf-44c0-b0d7-d431d59271d1");
        final File ccMain = UtilLocator.getClassSource(AntScript.class);
        writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "6ac5192e-0f25-4a96-9350-9a75d3bd1c35");
        if (ccMain == null) {
            writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "4f0684ca-2d48-4111-aa0b-014a17f39e98");
            throw new ProgressLibLocatorException("Could not determine -lib path for progressLoggerLib. (Java 6/Webstart issue?) " + MSG_RESOLUTION_PROGRESS_LOGGER_LIB);
        } else {
            writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "c122ff86-eaa5-4a1e-8d68-d6534de7f74f");
            final String pathToDirContainingCCMainJar;
            writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "bf2765ee-e701-41fd-9934-8171673693d9");
            if (ccMain.isDirectory()) {
                writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "f7b8dc96-ef14-4c43-a031-4d7e22b969bf");
                pathToDirContainingCCMainJar = ccMain.getAbsolutePath();
            } else {
                writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "406aeebf-2c21-48ba-84ba-7b142c74c700");
                pathToDirContainingCCMainJar = ccMain.getParentFile().getAbsolutePath();
            }
            writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "4cd48d72-b8ec-4731-a136-91bd04b7a7af");
            final File expectedProgressLoggerJar = new File(pathToDirContainingCCMainJar, LIBNAME_PROGRESS_LOGGER);
            writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "bfa713dd-2091-421a-b6fb-d9430139db8a");
            if (expectedProgressLoggerJar.exists()) {
                writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "71345c1d-09e5-43ba-bce4-41746ce2733e");
                // Use the specific jar if that jar exists.
                // This is a bit of a hack to load the progress logger jar into
                // ant without loading other jars (such as, ant.jar for instance)
                progressLoggerLib = expectedProgressLoggerJar.getAbsolutePath();
            } else {
                writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "5f98a0ec-e1b6-46a7-9f91-0a86a3e55e35");
                // Missing Progress Logger Lib is nasty to debug, so error out here if we can't find it for sure.
                throw new ProgressLibLocatorException("The progressLoggerLib jar file does not exist where expected: " + expectedProgressLoggerJar.getAbsolutePath() + MSG_RESOLUTION_PROGRESS_LOGGER_LIB);
            }
        }
        writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "f7c5258c-a862-4377-a72e-c7995d674530");
        return progressLoggerLib;
    }

    void setupDefaultProgressLoggerLib() throws ProgressLibLocatorException {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "a6917b5c-27f1-472f-89eb-d260f820e406");
        if (progressLoggerLib == null) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "b6d4d00b-4920-40a4-8827-665b1beb9f5e");
            // Use a valid default for progressLoggerLib
            progressLoggerLib = findDefaultProgressLoggerLib();
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "afdc6790-9ccb-45db-bda8-21269da8b12a");
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
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "9a10fd5d-1051-4c78-95c5-b567d835bf99");
        if (progress != null && line != null && line.startsWith(MSG_PREFIX_ANT_PROGRESS)) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "05259442-b609-4d19-a21d-365d9257c192");
            progress.setValue(line.substring(MSG_PREFIX_ANT_PROGRESS.length()));
        }
    }

    /**
     * @param buildProperties The buildProperties to set.
     */
    public void setBuildProperties(final Map<String, String> buildProperties) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "c36568fb-2948-445d-865d-38d1a23311e7");
        this.buildProperties = buildProperties;
    }

    /**
     * @return Returns the loggerClassName.
     */
    public String getLoggerClassName() {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "a9cb6849-68a6-4d97-9629-e5c9bdf62d6a");
        return loggerClassName;
    }

    /**
     * @param loggerClassName The loggerClassName to set.
     */
    public void setLoggerClassName(String loggerClassName) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "8ed3f925-6761-46fd-8f14-06d26cda4c70");
        this.loggerClassName = loggerClassName;
    }

    /**
     * @param isLoggerClassNameSet The loggerClassName to set.
     */
    public void setIsLoggerClassNameSet(boolean isLoggerClassNameSet) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "07853983-d477-4dd4-8861-9e8781f32513");
        this.isLoggerClassNameSet = isLoggerClassNameSet;
    }

    /**
     * @param showAntOutput if true use Dashboard AntOutputLogger (CLASSNAME_DASHBOARD_LISTENER) as listener IIF
     * useLogger is also true
     */
    public void setShowAntOutput(final boolean showAntOutput) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "c8a99994-3f93-48f0-8aa2-10cad3467576");
        this.showAntOutput = showAntOutput;
    }

    /**
     * @param antScript The antScript to set.
     */
    public void setAntScript(String antScript) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "7a46dc58-b5e2-4f4a-ad56-724ec626e718");
        this.antScript = antScript;
    }

    /**
     * @param args The args to set.
     */
    public void setArgs(final List<AntBuilder.JVMArg> args) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "071b578b-de3b-472e-a4bb-8d125164e583");
        this.args = args;
    }

    /**
     * @param isWindows The isWindows to set.
     */
    public void setWindows(boolean isWindows) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "79eeaab1-7eec-46d9-bd04-513163170642");
        this.isWindows = isWindows;
    }

    /**
     * @param buildFile The buildFile to set.
     */
    public void setBuildFile(String buildFile) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "ad89b370-6ebc-4154-ba9b-a3eca2f2f5f6");
        this.buildFile = buildFile;
    }

    /**
     * @param tempFileName The tempFileName to set.
     */
    public void setTempFileName(String tempFileName) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "a5111e9e-54b2-4f3b-b489-9fbabb825e50");
        this.tempFileName = tempFileName;
    }

    /**
     * @param useDebug The useDebug to set.
     */
    public void setUseDebug(boolean useDebug) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "a7aaaabe-141a-425b-af15-3fcbfa3d78c1");
        this.useDebug = useDebug;
    }

    /**
     * @param useLogger The useLogger to set.
     */
    public void setUseLogger(boolean useLogger) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "8286be8e-9d33-4fc8-a387-7099ff979fc9");
        this.useLogger = useLogger;
    }

    /**
     * @param useQuiet The useQuiet to set.
     */
    public void setUseQuiet(boolean useQuiet) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "975afef5-6e10-461a-af0a-ece06c089b9a");
        this.useQuiet = useQuiet;
    }

    public void setKeepGoing(boolean keepGoing) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "c9c707d9-fa9c-4b5e-8afb-6d592bb94d2c");
        this.keepGoing = keepGoing;
    }

    /**
     * @param useScript The useScript to set.
     */
    public void setUseScript(boolean useScript) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "4d13d847-fe91-4c25-8fab-ef677ba59e5d");
        this.useScript = useScript;
    }

    /**
     * @param systemClassPath The systemClassPath to set.
     */
    public void setSystemClassPath(String systemClassPath) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "d7bd25d3-e3ce-4e7f-9077-de013162e4fd");
        this.systemClassPath = systemClassPath;
    }

    /**
     * @param properties The properties to set.
     */
    public void setProperties(final List<Property> properties) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "0f7b3678-dbc6-4e11-a191-302b1f87a80f");
        this.properties = properties;
    }

    /**
     * @param libs The set of library paths to use.
     */
    public void setLibs(final List<AntBuilder.Lib> libs) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "159da354-a9c8-4d0b-b3fb-57484b5a9d36");
        this.libs = libs;
    }

    /**
     * @param listeners The set of listener classes to use.
     */
    public void setListeners(final List<AntBuilder.Listener> listeners) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "48541551-4fc6-42da-bcb0-6a19a43f986f");
        this.listeners = listeners;
    }

    /**
     * @param target The target to set.
     */
    public void setTarget(String target) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "085a359c-f3ea-47b2-803f-f43984349eda");
        this.target = target;
    }

    /**
     * @return Returns the exitCode.
     */
    public int getExitCode() {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "cc24b0ed-acf8-49da-892d-a59baeffc0ad");
        return exitCode;
    }

    /**
     * @param exitCode The exitCode to set.
     */
    public void setExitCode(int exitCode) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "be7dd1f1-17fa-4557-92f6-a1ea481b016d");
        this.exitCode = exitCode;
    }

    /**
     * @param propertyFile The properties file to set.
     */
    public void setPropertyFile(String propertyFile) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "19c30488-683e-4c87-bff9-50de607fcd19");
        this.propertyfile = propertyFile;
    }

    /**
     * @param progressLoggerLib The directory containing the AntProgressLogger/Listener classes.
     */
    public void setProgressLoggerLib(final String progressLoggerLib) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "24d1dfcc-973c-445e-933b-c954fec71aa7");
        this.progressLoggerLib = progressLoggerLib;
    }

    /**
     * @param progress The progress callback object to set.
     */
    public void setProgress(final Progress progress) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "7f0d95c7-6fab-4237-9216-a9663a7ceae8");
        this.progress = progress;
    }

    /**
     * @param env
     * The environment variables of the ant script, or <code>null</code> if to
     * inherit the environment of the current process.
     */
    public void setAntEnv(final OSEnvironment env) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_4_10.coverage", "ab9e44e0-7b13-485f-afeb-b2ad464fc374");
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
