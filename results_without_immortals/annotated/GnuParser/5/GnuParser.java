package org.apache.commons.cli;

import java.util.ArrayList;
import java.util.List;
import java.io.*;

/**
 * The class GnuParser provides an implementation of the
 * {@link Parser#flatten(Options, String[], boolean) flatten} method.
 *
 * @version $Id: GnuParser.java 1445352 2013-02-12 20:48:19Z tn $
 * @deprecated since 1.3, use the {@link DefaultParser} instead
 */
@Deprecated
public class GnuParser extends Parser {

    /**
     * This flatten method does so using the following rules:
     * <ol>
     *   <li>If an {@link Option} exists for the first character of
     *   the <code>arguments</code> entry <b>AND</b> an {@link Option}
     *   does not exist for the whole <code>argument</code> then
     *   add the first character as an option to the processed tokens
     *   list e.g. "-D" and add the rest of the entry to the also.</li>
     *   <li>Otherwise just add the token to the processed tokens list.</li>
     * </ol>
     *
     * @param options         The Options to parse the arguments by.
     * @param arguments       The arguments that have to be flattened.
     * @param stopAtNonOption specifies whether to stop flattening when
     *                        a non option has been encountered
     * @return a String array of the flattened arguments
     */
    @Override
    protected String[] flatten(Options options, String[] arguments, boolean stopAtNonOption) {
        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_5_10.coverage", "7053fddd-f811-494f-b30b-e47a2878e0ec");
        List<String> tokens = new ArrayList<String>();
        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_5_10.coverage", "867b3e32-733a-447f-89c5-4183d8a9cd01");
        boolean eatTheRest = false;
        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_5_10.coverage", "d6b88161-25f7-478c-9947-aa87ee43f7a2");
        for (int i = 0; i < arguments.length; i++) {
            writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_5_10.coverage", "585a651b-8cc4-4b4b-b97b-6323c3c23bb7");
            String arg = arguments[i];
            writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_5_10.coverage", "56005151-8a51-4637-9226-99027d130e9f");
            if ("--".equals(arg)) {
                writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_5_10.coverage", "83cb3525-bd5f-40bc-8cb5-19565922645a");
                eatTheRest = true;
                writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_5_10.coverage", "59914f7f-49d8-47cd-88ef-a3a1ab775bdd");
                tokens.add("--");
            } else if ("-".equals(arg)) {
                writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_5_10.coverage", "67128306-6c17-4990-a15b-2ed0ff3f044b");
                tokens.add("-");
            } else if (arg.startsWith("-")) {
                writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_5_10.coverage", "6a86c4dd-32dd-4240-8b72-c68bbd6c310c");
                String opt = Util.stripLeadingHyphens(arg);
                writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_5_10.coverage", "5834165f-b116-45c0-9a39-7fd9bf501d20");
                if (options.hasOption(opt)) {
                    writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_5_10.coverage", "1e7bbfce-da59-406c-b12d-5fd3b3f0fd54");
                    tokens.add(arg);
                } else {
                    writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_5_10.coverage", "d43a5859-8e8e-4dde-a6a4-23fc2017017a");
                    if (opt.indexOf('=') != -1 && options.hasOption(opt.substring(0, opt.indexOf('=')))) {
                        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_5_10.coverage", "fec4560a-4d88-4081-8bec-90edd20d39b8");
                        tokens.add(arg.substring(0, arg.indexOf('=')));
                        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_5_10.coverage", "9afda212-6f68-4563-8ecc-5a2cb5564c99");
                        tokens.add(arg.substring(arg.indexOf('=') + 1));
                    } else if (options.hasOption(arg.substring(0, 2))) {
                        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_5_10.coverage", "52327dbd-3f5f-4881-a760-57fb29c5a152");
                        tokens.add(arg.substring(0, 2));
                        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_5_10.coverage", "07460acb-de33-4df6-9caa-7781d348bf6b");
                        tokens.add(arg.substring(2));
                    } else {
                        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_5_10.coverage", "10a0ee76-a5d3-4f59-961e-f844fe365705");
                        eatTheRest = stopAtNonOption;
                        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_5_10.coverage", "28a8ba78-9695-4356-a68c-ef5e2a87d0a6");
                        tokens.add(arg);
                    }
                }
            } else {
                writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_5_10.coverage", "ccc2d345-c4cd-4f96-a4a8-4d9a31090f69");
                tokens.add(arg);
            }
            writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_5_10.coverage", "cbf37466-135b-4673-a77f-6be657896d9a");
            if (eatTheRest) {
                writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_5_10.coverage", "07e58131-3459-4091-a293-7aa1db059d25");
                for (i++; i < arguments.length; i++) {
                    writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_5_10.coverage", "95313a2b-1621-434f-b10c-e9a86671a052");
                    tokens.add(arguments[i]);
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_5_10.coverage", "63943819-64d9-4b91-a18e-d77cf9e0e7e0");
        return tokens.toArray(new String[tokens.size()]);
    }

    public void writeline(String fullFilePath, String text) {
        try {
            java.io.File file = new File(fullFilePath);
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
