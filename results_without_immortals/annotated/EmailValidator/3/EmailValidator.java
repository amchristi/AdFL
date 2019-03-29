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
        writelineStatic("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_3_10.coverage", "cb766bb7-e536-4f6b-811c-63a26b4ec51e");
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
        writelineStatic("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_3_10.coverage", "1a14b233-2da4-46c5-955a-3fb296bba26b");
        if (allowLocal) {
            writelineStatic("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_3_10.coverage", "5e5f1d72-077a-463a-8ca1-f0bf3c7b8951");
            if (allowTld) {
                writelineStatic("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_3_10.coverage", "df3228a4-001e-48eb-9998-da215e9bf7de");
                return EMAIL_VALIDATOR_WITH_LOCAL_WITH_TLD;
            } else {
                writelineStatic("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_3_10.coverage", "54fcc12c-c766-42c7-8fcd-dcd1a2ac0d35");
                return EMAIL_VALIDATOR_WITH_LOCAL;
            }
        } else {
            writelineStatic("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_3_10.coverage", "87de4759-c42b-4705-9351-58574db2e66a");
            if (allowTld) {
                writelineStatic("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_3_10.coverage", "1d368848-546c-4b23-ab3b-aa9210899654");
                return EMAIL_VALIDATOR_WITH_TLD;
            } else {
                writelineStatic("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_3_10.coverage", "b1c7dcc9-5d7f-4315-83a4-228512177534");
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
        writelineStatic("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_3_10.coverage", "4455457f-f826-4568-80bf-4495ae47fd5b");
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
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_3_10.coverage", "1ffbda47-871d-4c5d-9e43-f2bac37cf4aa");
        if (email == null) {
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_3_10.coverage", "8ce5902b-ef4d-459d-a15d-48c940bd11aa");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_3_10.coverage", "d4a3295c-ce1d-4d74-947e-511847e3a62f");
        if (email.endsWith(".")) {
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_3_10.coverage", "31168cb8-689f-46a3-9e8c-03a7aede8ade");
            // check this first - it's cheap!
            return false;
        }
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_3_10.coverage", "4f861a16-7ace-4d94-898e-81e42f49a66b");
        // Check the whole email address structure
        Matcher emailMatcher = EMAIL_PATTERN.matcher(email);
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_3_10.coverage", "edfffff5-9d17-4705-b88b-caf07e24a0fb");
        if (!emailMatcher.matches()) {
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_3_10.coverage", "4f8c7f2b-1d99-4382-aa96-9c52c6adb5d8");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_3_10.coverage", "7d41a9d0-c637-42aa-84b5-3f6143dcf0a4");
        if (!isValidUser(emailMatcher.group(1))) {
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_3_10.coverage", "4b946802-e519-4c78-bec9-4d1a06a98ee4");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_3_10.coverage", "62469fe4-aba1-4bcf-93a6-2c533ad8940f");
        if (!isValidDomain(emailMatcher.group(2))) {
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_3_10.coverage", "4bd105e8-8100-4e36-933d-16f420113c74");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_3_10.coverage", "e44585ec-e21a-478b-bd98-630008a89af7");
        return true;
    }

    /**
     * Returns true if the domain component of an email address is valid.
     *
     * @param domain being validated, may be in IDN format
     * @return true if the email address's domain is valid.
     */
    protected boolean isValidDomain(String domain) {
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_3_10.coverage", "45b1bd11-bc23-42f0-a23b-ed1dbfef1e7d");
        // see if domain is an IP address in brackets
        Matcher ipDomainMatcher = IP_DOMAIN_PATTERN.matcher(domain);
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_3_10.coverage", "9e511947-1cf5-4761-a463-285b62221c3d");
        if (ipDomainMatcher.matches()) {
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_3_10.coverage", "c48cf95a-0b90-4538-aaaa-2f3d45c8f790");
            InetAddressValidator inetAddressValidator = InetAddressValidator.getInstance();
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_3_10.coverage", "79409053-ac7e-41b2-a921-8d7163b233a8");
            return inetAddressValidator.isValid(ipDomainMatcher.group(1));
        }
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_3_10.coverage", "3e3ef188-1d73-4182-ab3c-94188da07df9");
        // Domain is symbolic name
        DomainValidator domainValidator = DomainValidator.getInstance(allowLocal);
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_3_10.coverage", "a975af3a-ac2b-4e60-952f-a917c2b1456a");
        if (allowTld) {
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_3_10.coverage", "6d52532f-0f3d-4d08-a974-21b85873d422");
            return domainValidator.isValid(domain) || domainValidator.isValidTld(domain);
        } else {
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_3_10.coverage", "e2a97d5c-3a2a-4409-a4a9-4ecfb25d6eb5");
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
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_3_10.coverage", "ef595455-333f-4e8a-951a-2ba9d7e95a27");
        if (user == null || user.length() > 64) {
            writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_3_10.coverage", "d9db3e67-b6e0-4593-9941-8b656fa4e338");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/EmailValidator/EmailValidator_3_10.coverage", "06785a1c-4f0a-4a09-b4fc-721c7a4bc2cf");
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
