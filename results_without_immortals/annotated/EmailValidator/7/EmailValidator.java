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
        writelineStatic("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_7_10.coverage", "dc612ad0-4d39-4378-9ac4-70d3995144aa");
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
        writelineStatic("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_7_10.coverage", "a1554f36-0043-43a3-bd9b-5457f7aad977");
        if (allowLocal) {
            writelineStatic("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_7_10.coverage", "0053db4d-91fb-4f28-b1c0-aee7b9010c65");
            if (allowTld) {
                writelineStatic("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_7_10.coverage", "b52f7486-5153-40e1-bddb-76cdb363558b");
                return EMAIL_VALIDATOR_WITH_LOCAL_WITH_TLD;
            } else {
                writelineStatic("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_7_10.coverage", "6a19cb8f-f567-49dd-95ef-d683af37eaae");
                return EMAIL_VALIDATOR_WITH_LOCAL;
            }
        } else {
            writelineStatic("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_7_10.coverage", "df946147-46cd-4a8e-83b0-35347a0d3743");
            if (allowTld) {
                writelineStatic("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_7_10.coverage", "fa1cbd88-5314-4d36-8b43-28246db0d38e");
                return EMAIL_VALIDATOR_WITH_TLD;
            } else {
                writelineStatic("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_7_10.coverage", "8c8bfae8-027a-4ae7-aae1-679dd160c09c");
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
        writelineStatic("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_7_10.coverage", "b8be8f8e-1d08-448f-a019-79d2d3812996");
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
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_7_10.coverage", "0f1b3ee4-0e8d-4bea-a745-1823562b58bd");
        if (email == null) {
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_7_10.coverage", "a8e8bb52-0565-4360-be57-f7e63f52577c");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_7_10.coverage", "4d528b70-20e7-4079-be4f-2098031d3266");
        if (email.endsWith(".")) {
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_7_10.coverage", "42b2ea04-981e-4f6e-8f0c-a6fca5447dc5");
            // check this first - it's cheap!
            return false;
        }
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_7_10.coverage", "925b771d-2ecc-4d00-916f-1cdd7e9c9d60");
        // Check the whole email address structure
        Matcher emailMatcher = EMAIL_PATTERN.matcher(email);
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_7_10.coverage", "4dd70679-7c93-4a66-a57a-24ab3dd26f22");
        if (!emailMatcher.matches()) {
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_7_10.coverage", "ab0c0775-e93b-4ca0-8706-a03f4d7e8d17");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_7_10.coverage", "7c4aa3ea-74df-445b-984d-c881d96c4b2a");
        if (!isValidUser(emailMatcher.group(1))) {
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_7_10.coverage", "9895fe50-9f6d-476e-b2be-d95b85e5fcc0");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_7_10.coverage", "b9896d7d-1a37-4263-bff5-55cbbc492812");
        if (!isValidDomain(emailMatcher.group(2))) {
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_7_10.coverage", "babd8923-f184-447f-97f9-62abc69b48a7");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_7_10.coverage", "4e7914dd-860b-49bc-ab17-f6eb208ab947");
        return true;
    }

    /**
     * Returns true if the domain component of an email address is valid.
     *
     * @param domain being validated, may be in IDN format
     * @return true if the email address's domain is valid.
     */
    protected boolean isValidDomain(String domain) {
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_7_10.coverage", "0e0c24c1-2ed2-47f9-ac92-d26432844af5");
        // see if domain is an IP address in brackets
        Matcher ipDomainMatcher = IP_DOMAIN_PATTERN.matcher(domain);
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_7_10.coverage", "4dba0142-7561-4c82-aefa-f2433a51fce6");
        if (ipDomainMatcher.matches()) {
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_7_10.coverage", "233d2195-a63d-4440-a533-31ef16e660f4");
            InetAddressValidator inetAddressValidator = InetAddressValidator.getInstance();
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_7_10.coverage", "9ef4c617-708e-44e1-a25a-3ebd43c41240");
            return inetAddressValidator.isValid(ipDomainMatcher.group(1));
        }
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_7_10.coverage", "dc27b259-375d-4a84-abab-db6e2dca6bcc");
        // Domain is symbolic name
        DomainValidator domainValidator = DomainValidator.getInstance(allowLocal);
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_7_10.coverage", "1717d905-3dd2-4148-8953-7e6fda736ca3");
        if (allowTld) {
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_7_10.coverage", "f51490a3-f7f1-475c-adf9-4431207fbdf7");
            return domainValidator.isValid(domain) || domainValidator.isValidTld(domain);
        } else {
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_7_10.coverage", "db6584fd-a97b-4dd6-a04e-dd59f31b7dc8");
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
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_7_10.coverage", "d6b53e8e-ed2c-4cc6-829e-87b6f737d23a");
        if (user == null || user.length() > 64) {
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_7_10.coverage", "90e022ad-e1c1-4df6-a6f8-f70fa52f1f7c");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_7_10.coverage", "86b123fc-8790-4589-82aa-ab3c450856b2");
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
