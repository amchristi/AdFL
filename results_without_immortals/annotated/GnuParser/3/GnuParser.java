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
        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_3_10.coverage", "b6dd1e2f-7b30-4303-ae5b-da3591813281");
        List<String> tokens = new ArrayList<String>();
        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_3_10.coverage", "c1c17e43-b50c-41b6-9904-277e83fa4171");
        boolean eatTheRest = false;
        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_3_10.coverage", "e454101b-7e4f-404f-aa83-fd33e789a802");
        for (int i = 0; i < arguments.length; i++) {
            writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_3_10.coverage", "c3c8a581-bff9-4590-b9d1-bcc4cc90cdb6");
            String arg = arguments[i];
            writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_3_10.coverage", "9ecc5b89-a91f-49a5-b4d8-d6f52e16460f");
            if ("--".equals(arg)) {
                writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_3_10.coverage", "2e359c10-926d-4411-95cb-02bb8770cf46");
                eatTheRest = true;
                writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_3_10.coverage", "71a8dba7-d58f-4239-86a1-cf7ca39764b9");
                tokens.add("--");
            } else if ("-".equals(arg)) {
                writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_3_10.coverage", "710f7c1a-d6ab-46fb-9293-c1999511fe98");
                tokens.add("-");
            } else if (arg.startsWith("-")) {
                writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_3_10.coverage", "4adcb5ad-69bc-4799-87d2-e3771fcd7aea");
                String opt = Util.stripLeadingHyphens(arg);
                writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_3_10.coverage", "b2b78405-0d0f-4c23-8db8-5d5e8ac9d5b2");
                if (options.hasOption(opt)) {
                    writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_3_10.coverage", "49b532de-6e49-4e17-a332-616b5e65759d");
                    tokens.add(arg);
                } else {
                    writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_3_10.coverage", "e768c5f5-ed7e-435d-b00e-72c9e08fc469");
                    if (opt.indexOf('=') != -1 && options.hasOption(opt.substring(0, opt.indexOf('=')))) {
                        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_3_10.coverage", "b1510972-8a02-4292-8fd1-f5f5c7172bfd");
                        tokens.add(arg.substring(0, arg.indexOf('=')));
                        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_3_10.coverage", "6bad5659-4dab-49d1-a3ac-e9eca2314258");
                        tokens.add(arg.substring(arg.indexOf('=') + 1));
                    } else if (options.hasOption(arg.substring(0, 2))) {
                        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_3_10.coverage", "01f4f3d2-285c-49b6-9aeb-3fc342e3faca");
                        tokens.add(arg.substring(0, 2));
                        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_3_10.coverage", "411dbdb4-1362-4a53-bb4a-1cb011135435");
                        tokens.add(arg.substring(2));
                    } else {
                        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_3_10.coverage", "3d39955f-0249-4a16-8ee7-bac1c671158a");
                        eatTheRest = stopAtNonOption;
                        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_3_10.coverage", "e43f15a8-b159-4c73-9ad7-0eb822654d59");
                        tokens.add(arg);
                    }
                }
            } else {
                writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_3_10.coverage", "193c2a86-f0bf-4d36-bae3-979807dbbc40");
                tokens.add(arg);
            }
            writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_3_10.coverage", "51a1f9fa-3cd2-48ce-af5a-3df5387f644a");
            if (eatTheRest) {
                writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_3_10.coverage", "3b60f0a1-48cd-4397-b96b-23440dba5c18");
                for (i++; i < arguments.length; i++) {
                    writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_3_10.coverage", "4ab7e75b-628b-4616-9d7a-65175dc05e53");
                    tokens.add(arguments[i]);
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/GnuParser/GnuParser_3_10.coverage", "c730e338-d57f-4f08-af2c-1ee24675bc14");
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
