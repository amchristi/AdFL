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
        writelineStatic("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_8_10.coverage", "d0fb9281-0363-43ee-9f79-4f9b2f54d062");
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
        writelineStatic("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_8_10.coverage", "8f0120ef-6fd7-49f4-a90c-ff80b5eebfb3");
        if (allowLocal) {
            writelineStatic("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_8_10.coverage", "d2fe3e0e-f211-4a66-b0e9-8ba0675a1f5a");
            if (allowTld) {
                writelineStatic("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_8_10.coverage", "8dced4eb-ca2e-4319-a583-08098aee9c90");
                return EMAIL_VALIDATOR_WITH_LOCAL_WITH_TLD;
            } else {
                writelineStatic("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_8_10.coverage", "2b5ed21d-8b46-47f6-bec7-7ae945f5588d");
                return EMAIL_VALIDATOR_WITH_LOCAL;
            }
        } else {
            writelineStatic("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_8_10.coverage", "846dfdc4-6daa-4722-afcd-805df4d0dd26");
            if (allowTld) {
                writelineStatic("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_8_10.coverage", "5213fcc6-adab-4764-8f91-adc303c00d14");
                return EMAIL_VALIDATOR_WITH_TLD;
            } else {
                writelineStatic("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_8_10.coverage", "82fd996a-06c3-4b37-b0b8-27d4cb4d87f6");
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
        writelineStatic("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_8_10.coverage", "72a9db6a-4de0-44ec-b814-ee5b4bfb368e");
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
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_8_10.coverage", "6bef4854-2b63-45d3-b355-22c55ecb6c83");
        if (email == null) {
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_8_10.coverage", "df5c69a2-1141-464c-b656-014d1bb972c0");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_8_10.coverage", "7fcdab73-0ee5-48b2-90f5-4fb0bff8891c");
        if (email.endsWith(".")) {
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_8_10.coverage", "25bc098d-ee15-4cd5-af31-21e773a51e31");
            // check this first - it's cheap!
            return false;
        }
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_8_10.coverage", "185ec8a9-6f4e-4e83-977e-d90aec1e4642");
        // Check the whole email address structure
        Matcher emailMatcher = EMAIL_PATTERN.matcher(email);
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_8_10.coverage", "d0b33b1c-1e3f-452d-b009-40a85c95420d");
        if (!emailMatcher.matches()) {
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_8_10.coverage", "6f78f20a-5c8a-4306-b1de-99b37c13a82a");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_8_10.coverage", "edaf251a-15c8-4357-9c74-b19b85b49d9d");
        if (!isValidUser(emailMatcher.group(1))) {
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_8_10.coverage", "75f6c21b-916e-4b19-bac3-de69386b92c1");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_8_10.coverage", "6ba29430-7d2e-47be-9e19-0b6fa0f3291b");
        if (!isValidDomain(emailMatcher.group(2))) {
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_8_10.coverage", "d86282ef-2356-4e8a-8d92-ac0958225f68");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_8_10.coverage", "e176aad6-94fd-4af6-b0ad-32c82e439238");
        return true;
    }

    /**
     * Returns true if the domain component of an email address is valid.
     *
     * @param domain being validated, may be in IDN format
     * @return true if the email address's domain is valid.
     */
    protected boolean isValidDomain(String domain) {
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_8_10.coverage", "c91b95fc-fa12-4dfb-b271-f945ee9896e0");
        // see if domain is an IP address in brackets
        Matcher ipDomainMatcher = IP_DOMAIN_PATTERN.matcher(domain);
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_8_10.coverage", "81545964-97af-4c42-a5cd-b666e56f9b66");
        if (ipDomainMatcher.matches()) {
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_8_10.coverage", "fa90ff70-afb3-4ee2-bc86-309f200f15ed");
            InetAddressValidator inetAddressValidator = InetAddressValidator.getInstance();
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_8_10.coverage", "28740850-75a7-4071-adcb-429e4bc9d532");
            return inetAddressValidator.isValid(ipDomainMatcher.group(1));
        }
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_8_10.coverage", "04e9ebe9-6c1c-4b57-bf47-03751f77c026");
        // Domain is symbolic name
        DomainValidator domainValidator = DomainValidator.getInstance(allowLocal);
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_8_10.coverage", "4b147114-e283-4b4c-989e-c09774d2c1c6");
        if (allowTld) {
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_8_10.coverage", "f6701e21-5608-4e08-8b12-a39258708631");
            return domainValidator.isValid(domain) || domainValidator.isValidTld(domain);
        } else {
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_8_10.coverage", "97005cdb-6530-41e0-9d1e-c0a4166b1a13");
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
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_8_10.coverage", "7905878b-e474-44db-8f9c-73ec838b6e22");
        if (user == null || user.length() > 64) {
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_8_10.coverage", "37ea9691-7d7b-44b8-836b-253e42afd99c");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_8_10.coverage", "55c20267-a217-46af-8c89-ec9427bb07e5");
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
