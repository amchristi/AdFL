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
import java.util.regex.Pattern;
import java.util.regex.Matcher;
import java.io.*;

/**
 * <b>Regular Expression</b> validation (using JDK 1.4+ regex support).
 * <p>
 * Construct the validator either for a single regular expression or a set (array) of
 * regular expressions. By default validation is <i>case sensitive</i> but constructors
 * are provided to allow  <i>case in-sensitive</i> validation. For example to create
 * a validator which does <i>case in-sensitive</i> validation for a set of regular
 * expressions:
 * </p>
 * <pre>
 * <code>
 * String[] regexs = new String[] {...};
 * RegexValidator validator = new RegexValidator(regexs, false);
 * </code>
 * </pre>
 *
 * <ul>
 * <li>Validate <code>true</code> or <code>false</code>:</li>
 * <li>
 * <ul>
 * <li><code>boolean valid = validator.isValid(value);</code></li>
 * </ul>
 * </li>
 * <li>Validate returning an aggregated String of the matched groups:</li>
 * <li>
 * <ul>
 * <li><code>String result = validator.validate(value);</code></li>
 * </ul>
 * </li>
 * <li>Validate returning the matched groups:</li>
 * <li>
 * <ul>
 * <li><code>String[] result = validator.match(value);</code></li>
 * </ul>
 * </li>
 * </ul>
 *
 * <b>Note that patterns are matched against the entire input.</b>
 *
 * <p>
 * Cached instances pre-compile and re-use {@link Pattern}(s) - which according
 * to the {@link Pattern} API are safe to use in a multi-threaded environment.
 * </p>
 *
 * @version $Revision: 1713331 $
 * @since Validator 1.4
 */
public class RegexValidator implements Serializable {

    private static final long serialVersionUID = -8832409930574867162L;

    private final Pattern[] patterns;

    /**
     * Construct a <i>case sensitive</i> validator for a single
     * regular expression.
     *
     * @param regex The regular expression this validator will
     * validate against
     */
    public RegexValidator(String regex) {
        this(regex, true);
    }

    /**
     * Construct a validator for a single regular expression
     * with the specified case sensitivity.
     *
     * @param regex The regular expression this validator will
     * validate against
     * @param caseSensitive when <code>true</code> matching is <i>case
     * sensitive</i>, otherwise matching is <i>case in-sensitive</i>
     */
    public RegexValidator(String regex, boolean caseSensitive) {
        this(new String[] { regex }, caseSensitive);
    }

    /**
     * Construct a <i>case sensitive</i> validator that matches any one
     * of the set of regular expressions.
     *
     * @param regexs The set of regular expressions this validator will
     * validate against
     */
    public RegexValidator(String[] regexs) {
        this(regexs, true);
    }

    /**
     * Construct a validator that matches any one of the set of regular
     * expressions with the specified case sensitivity.
     *
     * @param regexs The set of regular expressions this validator will
     * validate against
     * @param caseSensitive when <code>true</code> matching is <i>case
     * sensitive</i>, otherwise matching is <i>case in-sensitive</i>
     */
    public RegexValidator(String[] regexs, boolean caseSensitive) {
        if (regexs == null || regexs.length == 0) {
            throw new IllegalArgumentException("Regular expressions are missing");
        }
        patterns = new Pattern[regexs.length];
        int flags = (caseSensitive ? 0 : Pattern.CASE_INSENSITIVE);
        for (int i = 0; i < regexs.length; i++) {
            if (regexs[i] == null || regexs[i].length() == 0) {
                throw new IllegalArgumentException("Regular expression[" + i + "] is missing");
            }
            patterns[i] = Pattern.compile(regexs[i], flags);
        }
    }

    /**
     * Validate a value against the set of regular expressions.
     *
     * @param value The value to validate.
     * @return <code>true</code> if the value is valid
     * otherwise <code>false</code>.
     */
    public boolean isValid(String value) {
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_4_10.coverage", "233e8bbf-7dfa-47ae-a545-f11a6ca142cc");
        if (value == null) {
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_4_10.coverage", "fd467a9b-3562-482e-96e4-837c2471d27c");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_4_10.coverage", "7e014688-7bbf-49ef-902b-ff67f81ade6a");
        for (int i = 0; i < patterns.length; i++) {
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_4_10.coverage", "a4d3df4b-0c13-434d-a40f-46ec31ae26f0");
            if (patterns[i].matcher(value).matches()) {
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_4_10.coverage", "2653fe61-b4a2-44bc-bf61-7a51b4becd4c");
                return true;
            }
        }
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_4_10.coverage", "5204320f-e03a-428c-97bc-01e3a721d430");
        return false;
    }

    /**
     * Validate a value against the set of regular expressions
     * returning the array of matched groups.
     *
     * @param value The value to validate.
     * @return String array of the <i>groups</i> matched if
     * valid or <code>null</code> if invalid
     */
    public String[] match(String value) {
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_4_10.coverage", "fc67c9e5-1bd9-4357-9c0b-3703d9e3b091");
        if (value == null) {
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_4_10.coverage", "2c2027b3-554c-4f39-a00a-284c75bbba95");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_4_10.coverage", "d3482c1c-2303-465c-ad8d-dc6ec5822255");
        for (int i = 0; i < patterns.length; i++) {
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_4_10.coverage", "b6fd71b9-7ae7-49a0-ba48-7b42200efc61");
            Matcher matcher = patterns[i].matcher(value);
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_4_10.coverage", "95790198-b4a8-4833-b996-3bbf7a3ff3c1");
            if (matcher.matches()) {
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_4_10.coverage", "88bf75ea-2621-4fce-b97c-edb3f9a770ad");
                int count = matcher.groupCount();
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_4_10.coverage", "63b271b2-26ec-4d16-a5fb-b5761f7256a8");
                String[] groups = new String[count];
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_4_10.coverage", "95baa9ba-05e6-4726-b231-82dc376b9f43");
                for (int j = 0; j < count; j++) {
                    writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_4_10.coverage", "74221831-e496-4df4-8fb5-48600f2ee597");
                    groups[j] = matcher.group(j + 1);
                }
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_4_10.coverage", "225ab564-23f4-48ca-a246-451fb44fabbe");
                return groups;
            }
        }
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_4_10.coverage", "2fed89a4-981e-4427-bd1b-ad1c587b7328");
        return null;
    }

    /**
     * Validate a value against the set of regular expressions
     * returning a String value of the aggregated groups.
     *
     * @param value The value to validate.
     * @return Aggregated String value comprised of the
     * <i>groups</i> matched if valid or <code>null</code> if invalid
     */
    public String validate(String value) {
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_4_10.coverage", "3123e26a-aff2-4e6a-ab81-89b004624a91");
        if (value == null) {
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_4_10.coverage", "9f5b7f08-b020-4396-be50-0f64144557af");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_4_10.coverage", "b5c6a75b-e91c-4fca-bad5-ca664edc5ada");
        for (int i = 0; i < patterns.length; i++) {
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_4_10.coverage", "a1d0b700-1fd4-4169-b29d-56c987e91f2f");
            Matcher matcher = patterns[i].matcher(value);
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_4_10.coverage", "1c39a48d-3b08-4590-a877-0dfc8fc3c11a");
            if (matcher.matches()) {
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_4_10.coverage", "12595cf2-7dfc-4bb2-ae5b-6454be4d01d9");
                int count = matcher.groupCount();
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_4_10.coverage", "be20682a-eb5b-43f5-9f9f-0f2cdf525276");
                if (count == 1) {
                    writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_4_10.coverage", "a4eb63fb-f714-4bff-9b22-0e519925d23f");
                    return matcher.group(1);
                }
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_4_10.coverage", "e4fcda85-6786-44b7-bbde-17a1ebad1d75");
                StringBuilder buffer = new StringBuilder();
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_4_10.coverage", "93345b42-07a1-4e9b-8140-f089ff2e1a0b");
                for (int j = 0; j < count; j++) {
                    writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_4_10.coverage", "cc0b3f41-18de-4f9c-abd1-fe039fa98a8a");
                    String component = matcher.group(j + 1);
                    writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_4_10.coverage", "90aa8806-5da6-4bd7-87d2-7cb79d286d41");
                    if (component != null) {
                        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_4_10.coverage", "9f387982-8640-4833-a567-4d8234f1a9c6");
                        buffer.append(component);
                    }
                }
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_4_10.coverage", "31690f09-79dd-45d9-9ff6-bfa51b1c7c64");
                return buffer.toString();
            }
        }
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_4_10.coverage", "144cbdd7-57e4-4a9e-9b6b-797aff7235d0");
        return null;
    }

    /**
     * Provide a String representation of this validator.
     * @return A String representation of this validator
     */
    public String toString() {
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_4_10.coverage", "71f4a15b-9ff6-4d10-a78b-3bf08dcf574c");
        StringBuilder buffer = new StringBuilder();
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_4_10.coverage", "c9111b54-9cdc-4c8d-859b-a6845df121ed");
        buffer.append("RegexValidator{");
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_4_10.coverage", "b7fa94a0-35e4-4df1-a646-4b80bd6c65a6");
        for (int i = 0; i < patterns.length; i++) {
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_4_10.coverage", "d6140a02-36e3-48de-ab17-f6e83ad3532d");
            if (i > 0) {
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_4_10.coverage", "41576600-5e6c-4926-8bd2-95254699e3ad");
                buffer.append(",");
            }
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_4_10.coverage", "86b242b9-c36b-45e6-a41f-482747543cb3");
            buffer.append(patterns[i].pattern());
        }
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_4_10.coverage", "1115174e-ec9a-4286-bda2-e5b94ca26b55");
        buffer.append("}");
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_4_10.coverage", "8e9976b1-0cbf-4a36-8be6-fdf317a131a3");
        return buffer.toString();
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
