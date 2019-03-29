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
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "95cc886b-38f1-469b-a1c5-c3995e792377");
        final Commandline cmdLine = new Commandline();
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "1d656cdf-ed91-477e-a2ec-ac620fa2ce4f");
        if (useScript) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "abea2fad-7d09-49a0-933b-99bace8e84f6");
            cmdLine.setExecutable(antScript);
        } else {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "560bc7cb-cd12-4242-8aae-483f7203044f");
            if (isWindows) {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "137ffff0-2085-4f93-8299-09f04466514b");
                cmdLine.setExecutable("java.exe");
            } else {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "68916239-a717-4d29-8988-d397719c0dd4");
                cmdLine.setExecutable("java");
            }
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "742c573c-fa4f-417d-9916-3bb8c0e984d2");
            for (final AntBuilder.JVMArg jvmArg : args) {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "a9004020-99c0-4529-8421-8c5d3799cb2e");
                final String arg = jvmArg.getArg();
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "e52020cb-b59b-4cab-9536-5e9c2186b0ec");
                // empty args may break the command line
                if (arg != null && arg.length() > 0) {
                    writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "19bca404-0e18-4e49-8b46-56e7dbec1b77");
                    cmdLine.createArgument(arg);
                }
            }
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "0b8cc87c-658d-4878-aee0-334d8807a746");
            final List<String> classpathItems = getClasspathItems(systemClassPath, isWindows);
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "9f7d1430-215a-4e60-be04-c46e5f06ce06");
            final String antLauncherJarLocation = getAntLauncherJarLocation(systemClassPath, classpathItems);
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "8698b68f-a0a7-4956-9b79-31af40693848");
            cmdLine.createArguments("-classpath", antLauncherJarLocation);
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "7be57a53-bcda-4494-b427-9e35e62ec6e0");
            cmdLine.createArgument("org.apache.tools.ant.launch.Launcher");
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "396340e0-e76d-4d06-872e-28e0a71dcb5c");
            cmdLine.createArguments("-lib", removeSaxonJars(classpathItems, isWindows));
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "24349826-6b6d-4456-8cff-687fbadfb8bb");
        if (progress == null) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "115581e6-32b8-40e1-8740-fef8372ef419");
            if (useLogger) {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "7ae21b44-8962-4fd4-a598-1317ba68ac1c");
                cmdLine.createArguments("-logger", getLoggerClassName());
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "2db76860-8311-4907-a3bc-d7591f054e33");
                cmdLine.createArguments("-logfile", tempFileName);
            } else {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "fe98c9c8-9615-4ac1-86d9-4898b3bf3bcd");
                cmdLine.createArguments("-listener", getLoggerClassName());
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "608586a5-df0d-4f60-97ab-137ac39cbd40");
                cmdLine.createArgument("-DXmlLogger.file=" + tempFileName);
            }
        } else {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "cd16c899-42c8-4802-866c-3b10c38a9423");
            // need to showProgress
            // use proper default logger if loggerClassName was not specified by config
            setupResolvedLoggerClassname();
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "605a908e-3de0-4eff-92bd-4c4312007ea1");
            cmdLine.createArguments("-logger", getLoggerClassName());
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "7efddfc2-b58c-41a1-8aea-84b89a86bef2");
            if (useLogger) {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "08992865-cf41-4cc4-95d0-e4906381e425");
                // need to use AntProgressXmlLogger as a listener
                cmdLine.createArguments("-listener", CLASSNAME_ANTPROGRESS_XML_LISTENER);
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "0d134e14-fb99-4f4c-83bf-f1a42f795c02");
                cmdLine.createArgument("-DXmlLogger.file=" + tempFileName);
            } else {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "99cfaf0c-3e60-460f-b4ed-2ebb09612ecb");
                cmdLine.createArguments("-listener", AntBuilder.DEFAULT_LOGGER);
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "1445bc65-9a3f-484f-b1a0-89d112b41e56");
                cmdLine.createArgument("-DXmlLogger.file=" + tempFileName);
            }
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "cb52cc21-a4f8-432e-a79d-871702935cbd");
        if (AntBuilder.shouldAddDashboardLoggerJarToCommandLine(showAntOutput, useLogger)) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "80568d16-5197-41b9-9e81-63e5087fb530");
            cmdLine.createArguments("-listener", CLASSNAME_DASHBOARD_LISTENER);
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "aadcaefd-a02c-4e34-b8db-0035370c9115");
        if ((progress != null) || AntBuilder.shouldAddDashboardLoggerJarToCommandLine(showAntOutput, useLogger)) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "03b1c738-21c3-473e-8360-cf30fc93bdc9");
            // we need to add the custom logger jar {@link #LIBNAME_PROGRESS_LOGGER cruisecontrol-antprogresslogger.jar}
            // to the ant VM class path as a lib
            setupDefaultProgressLoggerLib();
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "08fa582f-71e2-41f2-b40b-4c0d82b50e18");
            // add -lib to progressLogger classes
            cmdLine.createArguments("-lib", progressLoggerLib);
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "9a2fdacd-7781-44a3-9e98-b9fb37a7a03e");
        // a listener, they will affect the default logger that writes to the console
        if (useDebug) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "d9b69142-cac5-4766-97b7-75b4a64c7cb1");
            cmdLine.createArgument("-debug");
        } else if (useQuiet) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "fad8a37b-10cb-4b30-b4af-73e977200c0a");
            cmdLine.createArgument("-quiet");
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "a0455354-b8fd-4a2d-857d-38a52db3420c");
        if (keepGoing) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "98b8e8e7-3e06-40ef-ac5f-a5cbbb6b8105");
            cmdLine.createArgument("-keep-going");
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "ea127031-1498-4c64-ae60-ea64d9878b95");
        for (final AntBuilder.Lib lib : libs) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "7f222963-72d6-4610-b606-2e5231bd46e3");
            cmdLine.createArguments("-lib", lib.getSearchPath());
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "358594a9-1532-4bd9-8ad1-8e0fa85be019");
        for (final AntBuilder.Listener listener : listeners) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "9bc705a6-40eb-43f3-829c-ff4c83036950");
            cmdLine.createArguments("-listener", listener.getClassName());
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "b33af26e-1e96-4c4d-9c59-57da79d0011c");
        for (final Map.Entry property : buildProperties.entrySet()) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "191f4fb8-1b7d-428e-9d43-2295b261588a");
            final String value = (String) property.getValue();
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "2d7cab8b-d3a9-46e0-9090-367d7a85af40");
            if (!"".equals(value)) {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "5a2314dd-70be-4f25-8681-6dfc7e625d8b");
                cmdLine.createArgument("-D" + property.getKey() + "=" + value);
            }
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "880ee713-3a17-4e77-92ed-ce81d7ac9829");
        for (final Property property : properties) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "9e3894fb-8821-48eb-9fdd-a7bad021dd40");
            cmdLine.createArgument("-D" + property.getName() + "=" + property.getValue());
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "b5d5679e-3e2c-4a39-82fc-55e0f1043826");
        if (propertyfile != null) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "6e5ddc98-2c58-47b5-8325-bc145e74db15");
            cmdLine.createArguments("-propertyfile", propertyfile);
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "5ba5e3fc-9d4e-4a4b-a155-5d7c47b9078b");
        cmdLine.createArguments("-buildfile", buildFile);
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "cb885682-162f-43f8-9691-ded3abec10f5");
        cmdLine.setEnv(env);
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "cd8ff13b-525a-4ead-8f6c-dee3717d0ac2");
        final StringTokenizer targets = new StringTokenizer(target);
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "c5ae7ced-90c9-48f6-8ace-00afcf55869d");
        while (targets.hasMoreTokens()) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "b06a19fb-f8ef-4dfd-aac3-928664c5d7aa");
            cmdLine.createArgument(targets.nextToken());
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "e0734ce2-75ed-4311-8761-88d8c0f29da9");
        return cmdLine;
    }

    /**
     * @param path the classpath in which to search for the ant-launcher.jar
     * @param isWindows true if running on Windows
     * @return the path to ant-launcher*.jar taken from the given path
     * @throws CruiseControlException if path to ant-launcher.jar could not be found.
     */
    String getAntLauncherJarLocation(final String path, final boolean isWindows) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "a1b46a34-4234-43a2-86c5-118b85519005");
        return getAntLauncherJarLocation(path, getClasspathItems(path, isWindows));
    }

    /**
     * @param path the classpath as a single string, used here only for error message.
     * @param classpathItems the classpath items to search for the ant-launcher.jar
     * @return the path to ant-launcher*.jar taken from the given path
     * @throws CruiseControlException if path to ant-launcher.jar could not be found.
     */
    private String getAntLauncherJarLocation(final String path, final List<String> classpathItems) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "3ed34b64-59ac-48d2-a91e-f03e12286fd8");
        for (final String pathElement : classpathItems) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "fdcdc63a-2206-448d-88cd-5fee6dea496b");
            if (pathElement.indexOf("ant-launcher") != -1 && pathElement.endsWith(".jar")) {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "03ac075b-38ac-46a6-a721-49e0cce98a0f");
                return pathElement;
            }
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "cef5b51f-62c3-4a65-9e14-e32987ff96e2");
        throw new CruiseControlException("Couldn't find path to ant-launcher jar in this classpath: '" + path + "'");
    }

    /**
     * @param path the classpath to split each element into a List
     * @param isWindows true if running on Windows
     * @return a List containing each element in the classpath
     */
    List<String> getClasspathItems(final String path, final boolean isWindows) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "4ed4743c-1df3-4eb4-be3e-4e9fd8baf5da");
        final List<String> ret = new ArrayList<String>();
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "c4d49954-ec5d-43ea-bdf5-9c68eef1ea0d");
        final String separator = getSeparator(isWindows);
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "fefb779a-2fec-4fc5-9d1b-ae0390fce5d3");
        final StringTokenizer pathTokenizer = new StringTokenizer(path, separator);
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "17abe3a3-2bab-4686-8fef-42ae84e05fbd");
        while (pathTokenizer.hasMoreTokens()) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "068fcdf7-9e05-4774-8c60-d9e96084aa61");
            final String pathElement = pathTokenizer.nextToken();
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "28fa94e7-ee5a-4b30-b429-248e20c1dabf");
            ret.add(pathElement);
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "8c7547a3-523d-4434-8274-08e7226e4f82");
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
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "c661e6ce-3eca-4b67-a9e2-6594d9102695");
        final StringBuilder path = new StringBuilder();
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "205bea0f-b1a0-4480-b4ec-a50572defab7");
        final String separator = getSeparator(isWindows);
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "ddf16a48-3f0f-4511-9ed2-937c4e0045a9");
        for (final String pathElement : classpathItems) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "fc2ca346-4af5-48e4-896b-d33be765441c");
            final File elementFile = new File(pathElement);
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "ed7ba155-977b-4b81-99df-e848aaefa1cb");
            if (!elementFile.getName().startsWith("saxon")) {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "8bae5628-747f-43bb-ab08-32c664bc732c");
                if (path.length() > 0) {
                    writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "380ad4a4-f9b8-4fd6-8bf8-ff8a6232c809");
                    path.append(separator);
                }
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "c253aa71-4693-4406-bc0e-f2d832a99aab");
                path.append(pathElement);
            }
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "f86ab892-1dd0-4bb9-b8c6-068efbb930ea");
        return path.toString();
    }

    String removeSaxonJars(final String path, final boolean isWindows) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "79d30902-cfb4-46e7-99be-d18b7f2e3600");
        return removeSaxonJars(getClasspathItems(path, isWindows), isWindows);
    }

    private String getSeparator(boolean isWindows) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "756dc676-2749-414a-b8b5-22f9831c6dc3");
        return isWindows ? ";" : ":";
    }

    void setupResolvedLoggerClassname() {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "9f87eb80-691e-4b97-a738-6aedabd7dc30");
        // use proper default logger if loggerClassName was not specified by config
        if ((progress != null) && (!isLoggerClassNameSet)) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "8bbaa279-6f92-4ba0-a6e6-27bfdf8374d9");
            if (useLogger) {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "32d2a895-3ffb-442a-ac42-5dd4f273402b");
                // use string to avoid dependence on ant classes
                loggerClassName = CLASSNAME_ANTPROGRESS_XML_LOGGER;
            } else {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "9dec9a4a-b08e-46a4-b364-771e5f69e0c2");
                // use string to avoid dependence on ant classes
                loggerClassName = CLASSNAME_ANTPROGRESS_LOGGER;
            }
        } else {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "bd7bab05-919b-4736-b69d-abcf6ef3fac7");
            if (progress != null) {
                writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "875f3988-c8b0-4171-a82e-f5e5ef3492bb");
                LOG.warn("Ant Progress support is enabled AND loggerClassname is set. " + "Be sure the loggerClassName: " + loggerClassName + " is compatible with" + " Ant Progress.");
            }
        }
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "a0d87b6e-a719-4176-ac63-b35ddc255a68");
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
        writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "c327a0c8-c07e-4967-8bc8-7fd42274291d");
        // find path (including filename) to progressLoggerLib jar
        final String progressLoggerLib;
        writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "bb10f758-4b40-4fb4-8349-b41d9b998be5");
        final File ccMain = UtilLocator.getClassSource(AntScript.class);
        writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "61f0d480-3cdd-4542-8eab-26356d4b36ce");
        if (ccMain == null) {
            writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "e8b3f909-36d5-442f-bdfb-8e376cb7c31c");
            throw new ProgressLibLocatorException("Could not determine -lib path for progressLoggerLib. (Java 6/Webstart issue?) " + MSG_RESOLUTION_PROGRESS_LOGGER_LIB);
        } else {
            writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "afb88e41-a049-4b7f-89fe-edf24909dc7b");
            final String pathToDirContainingCCMainJar;
            writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "6dc5d9d6-3a73-4424-9761-4cf0e91bb20b");
            if (ccMain.isDirectory()) {
                writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "bda4b002-9a02-4fe3-ade7-6d4a0e61b95f");
                pathToDirContainingCCMainJar = ccMain.getAbsolutePath();
            } else {
                writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "371c2859-2b88-41ec-a249-a22dce84bbbc");
                pathToDirContainingCCMainJar = ccMain.getParentFile().getAbsolutePath();
            }
            writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "ca43ded6-94a9-4734-a7b6-57a76b7e49de");
            final File expectedProgressLoggerJar = new File(pathToDirContainingCCMainJar, LIBNAME_PROGRESS_LOGGER);
            writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "67c75dd7-8be5-487a-987d-c51ac189ccdc");
            if (expectedProgressLoggerJar.exists()) {
                writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "e4703d8a-1f95-4769-b571-24c715066697");
                // Use the specific jar if that jar exists.
                // This is a bit of a hack to load the progress logger jar into
                // ant without loading other jars (such as, ant.jar for instance)
                progressLoggerLib = expectedProgressLoggerJar.getAbsolutePath();
            } else {
                writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "c3c15468-d543-4e0c-8afe-0f83f32aada6");
                // Missing Progress Logger Lib is nasty to debug, so error out here if we can't find it for sure.
                throw new ProgressLibLocatorException("The progressLoggerLib jar file does not exist where expected: " + expectedProgressLoggerJar.getAbsolutePath() + MSG_RESOLUTION_PROGRESS_LOGGER_LIB);
            }
        }
        writelineStatic("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "143aad85-f630-44ea-9a30-51de244df0fe");
        return progressLoggerLib;
    }

    void setupDefaultProgressLoggerLib() throws ProgressLibLocatorException {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "701811d6-c7be-4174-8a8f-bf5de5dc1f9c");
        if (progressLoggerLib == null) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "df93ac4e-7830-4df3-8dba-9192f9a3e24c");
            // Use a valid default for progressLoggerLib
            progressLoggerLib = findDefaultProgressLoggerLib();
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "ca530e8c-393b-407b-ac9b-4ad864e5b91e");
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
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "1f62e87c-d7e2-43f5-8dea-03a93532f363");
        if (progress != null && line != null && line.startsWith(MSG_PREFIX_ANT_PROGRESS)) {
            writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "a9453d13-c962-46df-b60b-13848a595fe6");
            progress.setValue(line.substring(MSG_PREFIX_ANT_PROGRESS.length()));
        }
    }

    /**
     * @param buildProperties The buildProperties to set.
     */
    public void setBuildProperties(final Map<String, String> buildProperties) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "fc140497-6ec8-4015-8197-99745f36f47c");
        this.buildProperties = buildProperties;
    }

    /**
     * @return Returns the loggerClassName.
     */
    public String getLoggerClassName() {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "a4551432-313d-400b-ae49-5cde474b97f0");
        return loggerClassName;
    }

    /**
     * @param loggerClassName The loggerClassName to set.
     */
    public void setLoggerClassName(String loggerClassName) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "75b067c3-1525-4d1f-8184-aac6a75311ee");
        this.loggerClassName = loggerClassName;
    }

    /**
     * @param isLoggerClassNameSet The loggerClassName to set.
     */
    public void setIsLoggerClassNameSet(boolean isLoggerClassNameSet) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "3f8fb99f-b0fe-4866-be96-6eaa67f3df68");
        this.isLoggerClassNameSet = isLoggerClassNameSet;
    }

    /**
     * @param showAntOutput if true use Dashboard AntOutputLogger (CLASSNAME_DASHBOARD_LISTENER) as listener IIF
     * useLogger is also true
     */
    public void setShowAntOutput(final boolean showAntOutput) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "f1d2a1dd-977f-489f-ae43-6c678dba6ae8");
        this.showAntOutput = showAntOutput;
    }

    /**
     * @param antScript The antScript to set.
     */
    public void setAntScript(String antScript) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "957f51db-7d39-4e00-aafd-a2c8036df56b");
        this.antScript = antScript;
    }

    /**
     * @param args The args to set.
     */
    public void setArgs(final List<AntBuilder.JVMArg> args) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "e4fe6562-684b-4882-8c7e-42005dd07e54");
        this.args = args;
    }

    /**
     * @param isWindows The isWindows to set.
     */
    public void setWindows(boolean isWindows) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "736cc4c4-a9ad-40d8-90b5-c663cf15a1d3");
        this.isWindows = isWindows;
    }

    /**
     * @param buildFile The buildFile to set.
     */
    public void setBuildFile(String buildFile) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "d53467d9-a50d-4d40-b876-1d81ef901cc6");
        this.buildFile = buildFile;
    }

    /**
     * @param tempFileName The tempFileName to set.
     */
    public void setTempFileName(String tempFileName) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "c9525bdb-77d3-474f-be09-9556da2b9dd3");
        this.tempFileName = tempFileName;
    }

    /**
     * @param useDebug The useDebug to set.
     */
    public void setUseDebug(boolean useDebug) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "5d78855e-8b69-4f2f-a9ac-740fbe5a39db");
        this.useDebug = useDebug;
    }

    /**
     * @param useLogger The useLogger to set.
     */
    public void setUseLogger(boolean useLogger) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "1d95cd4d-f82d-4da5-8421-b23d443252c7");
        this.useLogger = useLogger;
    }

    /**
     * @param useQuiet The useQuiet to set.
     */
    public void setUseQuiet(boolean useQuiet) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "d343d410-0e4a-4069-be71-800434e22e28");
        this.useQuiet = useQuiet;
    }

    public void setKeepGoing(boolean keepGoing) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "592e4271-efc0-4af9-b464-26e93893b264");
        this.keepGoing = keepGoing;
    }

    /**
     * @param useScript The useScript to set.
     */
    public void setUseScript(boolean useScript) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "125e2443-81b2-4a93-b082-ca4b93ac7f43");
        this.useScript = useScript;
    }

    /**
     * @param systemClassPath The systemClassPath to set.
     */
    public void setSystemClassPath(String systemClassPath) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "646d935b-df22-4e26-b96c-698efbca9201");
        this.systemClassPath = systemClassPath;
    }

    /**
     * @param properties The properties to set.
     */
    public void setProperties(final List<Property> properties) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "c5b0fc7a-bcfd-4af2-83b3-ebff5ffa54bd");
        this.properties = properties;
    }

    /**
     * @param libs The set of library paths to use.
     */
    public void setLibs(final List<AntBuilder.Lib> libs) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "ad3adf7f-c769-4522-90f9-25e29fb82f51");
        this.libs = libs;
    }

    /**
     * @param listeners The set of listener classes to use.
     */
    public void setListeners(final List<AntBuilder.Listener> listeners) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "69f12615-d470-4c6f-a09b-ec491c72f4a2");
        this.listeners = listeners;
    }

    /**
     * @param target The target to set.
     */
    public void setTarget(String target) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "d4946c75-0971-44be-8ff6-60263be28a2e");
        this.target = target;
    }

    /**
     * @return Returns the exitCode.
     */
    public int getExitCode() {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "558ebe68-c17a-4c4f-9c19-6f9df89a9864");
        return exitCode;
    }

    /**
     * @param exitCode The exitCode to set.
     */
    public void setExitCode(int exitCode) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "945302cb-bf37-4624-a3ec-ccddda70fa19");
        this.exitCode = exitCode;
    }

    /**
     * @param propertyFile The properties file to set.
     */
    public void setPropertyFile(String propertyFile) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "d3724a57-bd8b-4169-a203-599ab54f5223");
        this.propertyfile = propertyFile;
    }

    /**
     * @param progressLoggerLib The directory containing the AntProgressLogger/Listener classes.
     */
    public void setProgressLoggerLib(final String progressLoggerLib) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "c535c0eb-ceb7-40fb-b9bd-c8b1641c09a1");
        this.progressLoggerLib = progressLoggerLib;
    }

    /**
     * @param progress The progress callback object to set.
     */
    public void setProgress(final Progress progress) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "ff5292d8-0e24-4aeb-b885-b2fb9821e89a");
        this.progress = progress;
    }

    /**
     * @param env
     * The environment variables of the ant script, or <code>null</code> if to
     * inherit the environment of the current process.
     */
    public void setAntEnv(final OSEnvironment env) {
        writeline("/home/ubuntu/results/coverage/AntScript/AntScript_5_10.coverage", "a8400906-d348-4e69-8451-d659c330d5a1");
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
