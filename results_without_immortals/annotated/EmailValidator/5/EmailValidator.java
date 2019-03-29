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
        writelineStatic("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_5_10.coverage", "c38a337b-0741-4fa1-a918-ba45d84920e4");
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
        writelineStatic("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_5_10.coverage", "b787a0ba-33fd-4eeb-bf24-109969288064");
        if (allowLocal) {
            writelineStatic("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_5_10.coverage", "087b6abd-5809-46fa-a5dc-7950abec8688");
            if (allowTld) {
                writelineStatic("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_5_10.coverage", "42a7a1b3-e843-401b-8bef-4cd894458115");
                return EMAIL_VALIDATOR_WITH_LOCAL_WITH_TLD;
            } else {
                writelineStatic("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_5_10.coverage", "3af9764a-6908-4250-93bf-1318d28b9521");
                return EMAIL_VALIDATOR_WITH_LOCAL;
            }
        } else {
            writelineStatic("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_5_10.coverage", "22b6ae6c-77f7-4f52-853a-d88c50209d02");
            if (allowTld) {
                writelineStatic("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_5_10.coverage", "9652d074-3e0a-4cc0-a6b1-84885af30221");
                return EMAIL_VALIDATOR_WITH_TLD;
            } else {
                writelineStatic("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_5_10.coverage", "e601bfb9-9777-478b-9ec4-1b2af98c1b20");
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
        writelineStatic("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_5_10.coverage", "7338cbd4-850b-47c4-8446-c857e6e5b14b");
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
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_5_10.coverage", "647a15d2-bd96-4ef3-aa3a-6ca0610a0915");
        if (email == null) {
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_5_10.coverage", "586064b6-330a-4270-a48a-58ce13df781b");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_5_10.coverage", "540bcb91-d76b-4d98-b3b9-7b449597df3e");
        if (email.endsWith(".")) {
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_5_10.coverage", "f99532e1-a067-4d6b-b662-a8da8bc2f734");
            // check this first - it's cheap!
            return false;
        }
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_5_10.coverage", "03f2e053-2300-4c9f-ad51-2b168ff6591f");
        // Check the whole email address structure
        Matcher emailMatcher = EMAIL_PATTERN.matcher(email);
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_5_10.coverage", "1948c96b-35e6-42a8-a283-9398dfa901b4");
        if (!emailMatcher.matches()) {
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_5_10.coverage", "f11b3092-78c4-4c70-8356-60068f987430");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_5_10.coverage", "db202655-472d-4545-ac01-913d9cdcf11b");
        if (!isValidUser(emailMatcher.group(1))) {
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_5_10.coverage", "477f5110-1e6b-4608-ba0c-09f36e3bf992");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_5_10.coverage", "5cbcc385-c1aa-4e1f-9057-cb95bacc6aa8");
        if (!isValidDomain(emailMatcher.group(2))) {
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_5_10.coverage", "ddd9cb83-e99a-44b0-9a64-d0782c0ad7a2");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_5_10.coverage", "c0acb920-a1ef-4136-8474-f3f68c78790c");
        return true;
    }

    /**
     * Returns true if the domain component of an email address is valid.
     *
     * @param domain being validated, may be in IDN format
     * @return true if the email address's domain is valid.
     */
    protected boolean isValidDomain(String domain) {
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_5_10.coverage", "af223145-cb01-4c41-9a4d-a88736e3cae8");
        // see if domain is an IP address in brackets
        Matcher ipDomainMatcher = IP_DOMAIN_PATTERN.matcher(domain);
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_5_10.coverage", "184a0064-5103-4426-8ab2-e4f99d5d9764");
        if (ipDomainMatcher.matches()) {
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_5_10.coverage", "5c662fb0-4a63-4351-9318-dc949d563e72");
            InetAddressValidator inetAddressValidator = InetAddressValidator.getInstance();
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_5_10.coverage", "d00df54c-4620-433d-9ccf-6ea88d1de93d");
            return inetAddressValidator.isValid(ipDomainMatcher.group(1));
        }
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_5_10.coverage", "f93c846a-b831-420c-b4ff-ea0dd24f0caa");
        // Domain is symbolic name
        DomainValidator domainValidator = DomainValidator.getInstance(allowLocal);
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_5_10.coverage", "c1facd0d-c4a7-4e38-a8df-dfbb9efb41d1");
        if (allowTld) {
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_5_10.coverage", "a4b65ab9-5adb-4ee6-857a-f9d9fb0cb10f");
            return domainValidator.isValid(domain) || domainValidator.isValidTld(domain);
        } else {
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_5_10.coverage", "08799b30-d5b7-47fc-99c5-43276c7150a2");
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
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_5_10.coverage", "006d5602-73fb-4712-8d68-1fce8169ee7e");
        if (user == null || user.length() > 64) {
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_5_10.coverage", "923c8a4b-13c2-4b47-9539-8def7ae1633a");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_5_10.coverage", "63be9a94-1f50-4829-b235-7984582ef366");
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
