/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.validator.routines;

import java.io.Serializable;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.io.*;

/**
 * <p>Perform email validations.</p>
 * <p>
 * Based on a script by <a href="mailto:stamhankar@hotmail.com">Sandeep V. Tamhankar</a>
 * http://javascript.internet.com
 * </p>
 * <p>
 * This implementation is not guaranteed to catch all possible errors in an email address.
 * </p>.
 *
 * @version $Revision: 1715080 $
 * @since Validator 1.4
 */
public class EmailValidator implements Serializable {

    private static final long serialVersionUID = 1705927040799295880L;

    private static final String SPECIAL_CHARS = "\\p{Cntrl}\\(\\)<>@,;:'\\\\\\\"\\.\\[\\]";

    private static final String VALID_CHARS = "(\\\\.)|[^\\s" + SPECIAL_CHARS + "]";

    private static final String QUOTED_USER = "(\"[^\"]*\")";

    private static final String WORD = "((" + VALID_CHARS + "|')+|" + QUOTED_USER + ")";

    private static final String EMAIL_REGEX = "^\\s*?(.+)@(.+?)\\s*$";

    private static final String IP_DOMAIN_REGEX = "^\\[(.*)\\]$";

    private static final String USER_REGEX = "^\\s*" + WORD + "(\\." + WORD + ")*$";

    private static final Pattern EMAIL_PATTERN = Pattern.compile(EMAIL_REGEX);

    private static final Pattern IP_DOMAIN_PATTERN = Pattern.compile(IP_DOMAIN_REGEX);

    private static final Pattern USER_PATTERN = Pattern.compile(USER_REGEX);

    private final boolean allowLocal;

    private final boolean allowTld;

    /**
     * Singleton instance of this class, which
     * doesn't consider local addresses as valid.
     */
    private static final EmailValidator EMAIL_VALIDATOR = new EmailValidator(false, false);

    /**
     * Singleton instance of this class, which
     * doesn't consider local addresses as valid.
     */
    private static final EmailValidator EMAIL_VALIDATOR_WITH_TLD = new EmailValidator(false, true);

    /**
     * Singleton instance of this class, which does
     * consider local addresses valid.
     */
    private static final EmailValidator EMAIL_VALIDATOR_WITH_LOCAL = new EmailValidator(true, false);

    /**
     * Singleton instance of this class, which does
     * consider local addresses valid.
     */
    private static final EmailValidator EMAIL_VALIDATOR_WITH_LOCAL_WITH_TLD = new EmailValidator(true, true);

    /**
     * Returns the Singleton instance of this validator.
     *
     * @return singleton instance of this validator.
     */
    public static EmailValidator getInstance() {
        writelineStatic("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_6_10.coverage", "c90fe98b-0947-4629-a7dc-28862349ba8c");
        return EMAIL_VALIDATOR;
    }

    /**
     * Returns the Singleton instance of this validator,
     * with local validation as required.
     *
     * @param allowLocal Should local addresses be considered valid?
     * @return singleton instance of this validator
     */
    public static EmailValidator getInstance(boolean allowLocal, boolean allowTld) {
        writelineStatic("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_6_10.coverage", "6d46828c-55fd-486a-885b-3de6a185c085");
        if (allowLocal) {
            writelineStatic("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_6_10.coverage", "22d86084-aa66-460a-9f19-7d809e9e2d30");
            if (allowTld) {
                writelineStatic("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_6_10.coverage", "86587b0f-59be-4f8d-a2db-0ec9cb2388c8");
                return EMAIL_VALIDATOR_WITH_LOCAL_WITH_TLD;
            } else {
                writelineStatic("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_6_10.coverage", "ec96da0d-8212-4d80-b1b4-20b979c21158");
                return EMAIL_VALIDATOR_WITH_LOCAL;
            }
        } else {
            writelineStatic("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_6_10.coverage", "4390b323-f34d-43d3-9b90-95888a47da8a");
            if (allowTld) {
                writelineStatic("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_6_10.coverage", "fff163c2-f18d-4cad-99a9-7ded082b0be1");
                return EMAIL_VALIDATOR_WITH_TLD;
            } else {
                writelineStatic("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_6_10.coverage", "3785154e-c400-4e0a-b865-d0d9f1760fa7");
                return EMAIL_VALIDATOR;
            }
        }
    }

    /**
     * Returns the Singleton instance of this validator,
     * with local validation as required.
     *
     * @param allowLocal Should local addresses be considered valid?
     * @return singleton instance of this validator
     */
    public static EmailValidator getInstance(boolean allowLocal) {
        writelineStatic("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_6_10.coverage", "96ed89d1-1dd3-493b-a743-a51906ccf5ad");
        return getInstance(allowLocal, false);
    }

    /**
     * Protected constructor for subclasses to use.
     *
     * @param allowLocal Should local addresses be considered valid?
     */
    protected EmailValidator(boolean allowLocal, boolean allowTld) {
        super();
        this.allowLocal = allowLocal;
        this.allowTld = allowTld;
    }

    /**
     * Protected constructor for subclasses to use.
     *
     * @param allowLocal Should local addresses be considered valid?
     */
    protected EmailValidator(boolean allowLocal) {
        super();
        this.allowLocal = allowLocal;
        this.allowTld = false;
    }

    /**
     * <p>Checks if a field has a valid e-mail address.</p>
     *
     * @param email The value validation is being performed on.  A <code>null</code>
     * value is considered invalid.
     * @return true if the email address is valid.
     */
    public boolean isValid(String email) {
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_6_10.coverage", "ef3e8c29-59e2-4031-8aad-0a7008fd9526");
        if (email == null) {
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_6_10.coverage", "3a5159fa-994a-4d05-b875-36a837dd9cd5");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_6_10.coverage", "e5efc9e9-e79f-4fb4-9710-204b0229e364");
        if (email.endsWith(".")) {
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_6_10.coverage", "4c0cf97f-5f27-4564-9b5f-28839fa7de3d");
            // check this first - it's cheap!
            return false;
        }
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_6_10.coverage", "9c941734-fb76-49f8-8aac-b2a11c5022fd");
        // Check the whole email address structure
        Matcher emailMatcher = EMAIL_PATTERN.matcher(email);
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_6_10.coverage", "a19d8a58-c523-4002-a655-57fb639461d8");
        if (!emailMatcher.matches()) {
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_6_10.coverage", "85eb913f-6f62-4afe-8c0d-dd3e0c0f907d");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_6_10.coverage", "6107a537-4837-4c6d-85ee-eeb1b5e6e06a");
        if (!isValidUser(emailMatcher.group(1))) {
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_6_10.coverage", "f623aa91-2d62-4dbf-bf44-719b63a4df45");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_6_10.coverage", "7b23e44f-1f85-4976-aa4e-5c78c7625fc2");
        if (!isValidDomain(emailMatcher.group(2))) {
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_6_10.coverage", "e20a62c8-cf6b-4b42-83ab-c413ce66da42");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_6_10.coverage", "776da551-e1e5-496d-b266-e4ab89af5c25");
        return true;
    }

    /**
     * Returns true if the domain component of an email address is valid.
     *
     * @param domain being validated, may be in IDN format
     * @return true if the email address's domain is valid.
     */
    protected boolean isValidDomain(String domain) {
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_6_10.coverage", "fa112dad-0bd9-4f44-881f-ee977d4b6c39");
        // see if domain is an IP address in brackets
        Matcher ipDomainMatcher = IP_DOMAIN_PATTERN.matcher(domain);
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_6_10.coverage", "6cf6b504-7d77-40d4-9bd9-301be2fed69e");
        if (ipDomainMatcher.matches()) {
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_6_10.coverage", "c41adeb5-d9b0-4d83-924a-48162c4b7820");
            InetAddressValidator inetAddressValidator = InetAddressValidator.getInstance();
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_6_10.coverage", "32897830-dae5-481f-a8c0-03098260868a");
            return inetAddressValidator.isValid(ipDomainMatcher.group(1));
        }
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_6_10.coverage", "cdea09f2-90a3-4a1e-8dfa-92f2c7022e31");
        // Domain is symbolic name
        DomainValidator domainValidator = DomainValidator.getInstance(allowLocal);
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_6_10.coverage", "09db108a-1deb-40b4-ba74-dba71b6d9d7b");
        if (allowTld) {
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_6_10.coverage", "3291d876-ca29-4d91-8538-808fdd0acf64");
            return domainValidator.isValid(domain) || domainValidator.isValidTld(domain);
        } else {
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_6_10.coverage", "996633a6-a9bd-4dfe-a7b7-81f8556ca712");
            return domainValidator.isValid(domain);
        }
    }

    /**
     * Returns true if the user component of an email address is valid.
     *
     * @param user being validated
     * @return true if the user name is valid.
     */
    protected boolean isValidUser(String user) {
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_6_10.coverage", "300f542c-b1e0-45d3-8c38-e10d095b1381");
        if (user == null || user.length() > 64) {
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_6_10.coverage", "a9c326a0-b6f1-44b2-9c2c-a93cbad50a07");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_6_10.coverage", "7b9677d2-478b-448f-8e33-2fedfe89ef28");
        return USER_PATTERN.matcher(user).matches();
    }

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
