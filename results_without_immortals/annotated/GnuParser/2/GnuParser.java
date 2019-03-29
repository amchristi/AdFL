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
        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_2_10.coverage", "c847f345-64d2-4caa-be0b-03b0e0cf46a8");
        List<String> tokens = new ArrayList<String>();
        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_2_10.coverage", "45ff2402-9236-45ef-ad6e-e4d3a2cfd6d3");
        boolean eatTheRest = false;
        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_2_10.coverage", "d0d97706-16aa-4058-b9b5-d34261d24e91");
        for (int i = 0; i < arguments.length; i++) {
            writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_2_10.coverage", "e18cebdf-d310-46f7-aa11-a840758902a7");
            String arg = arguments[i];
            writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_2_10.coverage", "b2c514b4-d098-41b3-9f3a-1e9f946b16cc");
            if ("--".equals(arg)) {
                writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_2_10.coverage", "346bb1ff-43bb-447b-9cba-bc13d5b973f7");
                eatTheRest = true;
                writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_2_10.coverage", "93aaade1-e5fd-425a-8dd6-8ebdf4231546");
                tokens.add("--");
            } else if ("-".equals(arg)) {
                writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_2_10.coverage", "5aadc3a2-a207-4b9d-ac25-c37668c0cda3");
                tokens.add("-");
            } else if (arg.startsWith("-")) {
                writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_2_10.coverage", "a7a6bc2e-1fba-47c5-a2be-38c939ded86d");
                String opt = Util.stripLeadingHyphens(arg);
                writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_2_10.coverage", "1bfc0a5a-bcfb-427f-88ee-2d6b665afbec");
                if (options.hasOption(opt)) {
                    writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_2_10.coverage", "f3fb432f-65dd-4b8b-872c-0e4254c5da9f");
                    tokens.add(arg);
                } else {
                    writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_2_10.coverage", "821aa12e-100a-4437-96f0-8d538961467f");
                    if (opt.indexOf('=') != -1 && options.hasOption(opt.substring(0, opt.indexOf('=')))) {
                        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_2_10.coverage", "6fcd8f21-d80c-4bf6-b3be-ef1ebb6e3569");
                        tokens.add(arg.substring(0, arg.indexOf('=')));
                        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_2_10.coverage", "0656c2b5-b43f-4734-96fd-7590bf579a4c");
                        tokens.add(arg.substring(arg.indexOf('=') + 1));
                    } else if (options.hasOption(arg.substring(0, 2))) {
                        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_2_10.coverage", "ec0f1923-c5ac-4be4-aa0b-090da46281b0");
                        tokens.add(arg.substring(0, 2));
                        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_2_10.coverage", "f2cc78a1-ff78-4563-b85f-5c27b94793d1");
                        tokens.add(arg.substring(2));
                    } else {
                        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_2_10.coverage", "df304054-3771-4720-a88f-d79cba6f8f12");
                        eatTheRest = stopAtNonOption;
                        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_2_10.coverage", "3e42cd9c-3c03-444c-8894-83af72bb5e65");
                        tokens.add(arg);
                    }
                }
            } else {
                writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_2_10.coverage", "83d2e184-6580-451f-b598-9b88e9aaf4bf");
                tokens.add(arg);
            }
            writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_2_10.coverage", "94423c1a-2c66-458c-95f7-82412781dbc2");
            if (eatTheRest) {
                writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_2_10.coverage", "f510a7d0-d1a7-40ca-a602-9bcbd5db1ee7");
                for (i++; i < arguments.length; i++) {
                    writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_2_10.coverage", "f7d5b8a0-499a-4cbb-95fa-603b94f919fc");
                    tokens.add(arguments[i]);
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_2_10.coverage", "328efb4c-c970-42da-b647-3f7eb55bc42d");
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
