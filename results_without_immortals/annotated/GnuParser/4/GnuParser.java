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
        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_4_10.coverage", "f1c7ecf7-2f15-477f-b6af-02ea51cd0014");
        List<String> tokens = new ArrayList<String>();
        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_4_10.coverage", "3e5b5e40-6319-4eef-a97c-05e33b2ccbc4");
        boolean eatTheRest = false;
        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_4_10.coverage", "94d1ea47-8a09-4906-b91c-bf03601c25db");
        for (int i = 0; i < arguments.length; i++) {
            writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_4_10.coverage", "8b5331ae-8f9d-4981-9a39-eb4023360c09");
            String arg = arguments[i];
            writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_4_10.coverage", "6f2caa10-71c0-485d-ab41-504de219750d");
            if ("--".equals(arg)) {
                writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_4_10.coverage", "0811e1bb-9921-48b2-b890-bd1ba5fda7c7");
                eatTheRest = true;
                writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_4_10.coverage", "fc0707ad-c8b6-41aa-8329-285a414df21c");
                tokens.add("--");
            } else if ("-".equals(arg)) {
                writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_4_10.coverage", "fa16bab8-bccf-4f0d-8317-93687b9417bd");
                tokens.add("-");
            } else if (arg.startsWith("-")) {
                writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_4_10.coverage", "0d068f89-632b-4a94-a29a-7dca62f12adb");
                String opt = Util.stripLeadingHyphens(arg);
                writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_4_10.coverage", "137bdfef-8a6c-44d5-aa1e-6a1f1ce95732");
                if (options.hasOption(opt)) {
                    writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_4_10.coverage", "031d8f59-408e-46dc-986c-eaa8add00416");
                    tokens.add(arg);
                } else {
                    writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_4_10.coverage", "47ad21f7-a83c-449b-8203-1c7b321a75d5");
                    if (opt.indexOf('=') != -1 && options.hasOption(opt.substring(0, opt.indexOf('=')))) {
                        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_4_10.coverage", "800a251d-23ee-4c3f-9589-d4a33fd0852c");
                        tokens.add(arg.substring(0, arg.indexOf('=')));
                        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_4_10.coverage", "23ab5be3-a11c-4df5-ad3f-f2b8fd52a11a");
                        tokens.add(arg.substring(arg.indexOf('=') + 1));
                    } else if (options.hasOption(arg.substring(0, 2))) {
                        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_4_10.coverage", "29c9a7be-7599-4b72-91bf-a2cd1b10f0cb");
                        tokens.add(arg.substring(0, 2));
                        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_4_10.coverage", "6eb5be05-e346-4d32-8920-7b13b873b555");
                        tokens.add(arg.substring(2));
                    } else {
                        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_4_10.coverage", "3d346c2c-360e-4e7e-a0bf-f23b23532d96");
                        eatTheRest = stopAtNonOption;
                        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_4_10.coverage", "315b9e39-e826-4dc9-a507-2827e4511bba");
                        tokens.add(arg);
                    }
                }
            } else {
                writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_4_10.coverage", "3546872d-29fe-45d6-a6cd-2b4b3f0d0ad2");
                tokens.add(arg);
            }
            writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_4_10.coverage", "2a024bdd-0e78-4490-a4ee-6396be7b03ef");
            if (eatTheRest) {
                writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_4_10.coverage", "b76bf275-db72-42b8-bb82-bbe6f3bcd669");
                for (i++; i < arguments.length; i++) {
                    writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_4_10.coverage", "68440c6f-4600-40c9-a9b6-3713e2cde5ff");
                    tokens.add(arguments[i]);
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_4_10.coverage", "ad6327af-3280-477d-a793-8040cff1d34d");
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
