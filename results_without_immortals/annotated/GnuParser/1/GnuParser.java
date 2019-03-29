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
        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_1_10.coverage", "93fec767-a480-4b7c-8189-74439af18b2f");
        List<String> tokens = new ArrayList<String>();
        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_1_10.coverage", "ab16bef6-7b92-4483-aaaa-668f9bea8c43");
        boolean eatTheRest = false;
        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_1_10.coverage", "004af11a-9646-4966-a483-1a3322c80dda");
        for (int i = 0; i < arguments.length; i++) {
            writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_1_10.coverage", "f1228b22-92bb-46a4-b72f-c124e50288fe");
            String arg = arguments[i];
            writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_1_10.coverage", "93cd7a03-04e2-4dda-8104-1c2086fc5c3d");
            if ("--".equals(arg)) {
                writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_1_10.coverage", "338e0c10-2452-4117-bd13-a18f95652b8d");
                eatTheRest = true;
                writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_1_10.coverage", "e9bebb27-72bd-4821-8317-7ec16c3af30d");
                tokens.add("--");
            } else if ("-".equals(arg)) {
                writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_1_10.coverage", "27202e58-d03e-4524-90fe-540dba20686b");
                tokens.add("-");
            } else if (arg.startsWith("-")) {
                writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_1_10.coverage", "a697fe65-abf5-494b-9e5e-79925d89bb4f");
                String opt = Util.stripLeadingHyphens(arg);
                writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_1_10.coverage", "d0dc248a-85c4-4528-afa2-6a88e1bafa47");
                if (options.hasOption(opt)) {
                    writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_1_10.coverage", "c4084bab-dfef-469d-977d-276525482fc2");
                    tokens.add(arg);
                } else {
                    writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_1_10.coverage", "78c966c8-d4c8-4410-be99-cef4c4e0239e");
                    if (opt.indexOf('=') != -1 && options.hasOption(opt.substring(0, opt.indexOf('=')))) {
                        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_1_10.coverage", "3a1c6464-39d9-4b9e-a26a-c2860110d8ae");
                        tokens.add(arg.substring(0, arg.indexOf('=')));
                        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_1_10.coverage", "14b24b05-de92-4e83-829f-55d7931428cd");
                        tokens.add(arg.substring(arg.indexOf('=') + 1));
                    } else if (options.hasOption(arg.substring(0, 2))) {
                        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_1_10.coverage", "11be51e1-2e7b-4b1b-853d-b19b157cc194");
                        tokens.add(arg.substring(0, 2));
                        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_1_10.coverage", "fdef1933-8cd4-4ff3-aa7e-7cda5a358b23");
                        tokens.add(arg.substring(2));
                    } else {
                        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_1_10.coverage", "4e39a1de-e3fd-4598-aa83-e858c5da6118");
                        eatTheRest = stopAtNonOption;
                        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_1_10.coverage", "632e3812-49ab-45e3-bb2b-a249e529f4fd");
                        tokens.add(arg);
                    }
                }
            } else {
                writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_1_10.coverage", "604733d7-165a-43f9-af47-4b25d281aad1");
                tokens.add(arg);
            }
            writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_1_10.coverage", "f2546cab-2c4d-4598-8889-9c66ff90f939");
            if (eatTheRest) {
                writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_1_10.coverage", "3a2c85ff-62bc-468a-936e-7e81edca97ce");
                for (i++; i < arguments.length; i++) {
                    writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_1_10.coverage", "0b49e947-c98d-4c04-8bd5-317e51eb8e6f");
                    tokens.add(arguments[i]);
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_1_10.coverage", "d96b46fd-0c0c-4b1b-9a00-2b1a2f48e1c3");
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
