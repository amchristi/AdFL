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
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_8_10.coverage", "d91eefb0-a11f-467a-bde5-059eec3b099e");
        if (value == null) {
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_8_10.coverage", "4b1a5f11-5c56-47f6-984c-7d19de2fb219");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_8_10.coverage", "cf58b624-9cf8-4cab-9383-8ef8e7916509");
        for (int i = 0; i < patterns.length; i++) {
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_8_10.coverage", "e1aa21db-485f-4289-8a54-992cce4afdd9");
            if (patterns[i].matcher(value).matches()) {
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_8_10.coverage", "88e2faf5-d576-4102-8105-2efb235bf9c3");
                return true;
            }
        }
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_8_10.coverage", "07e04d2f-f86b-4e91-a26a-3ac99c95b5d0");
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
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_8_10.coverage", "dc8675fe-0445-4294-829b-efe873bd7e31");
        if (value == null) {
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_8_10.coverage", "248f4102-66a0-4117-a6b9-628185cd2276");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_8_10.coverage", "b1d9bcce-6b6d-4456-9c51-4affc38cb4d3");
        for (int i = 0; i < patterns.length; i++) {
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_8_10.coverage", "89ead71c-1feb-40dd-9c7f-a49118c146cf");
            Matcher matcher = patterns[i].matcher(value);
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_8_10.coverage", "631a4461-dee9-481e-bfee-28228a118da5");
            if (matcher.matches()) {
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_8_10.coverage", "4dc974d1-66ef-4a8c-9139-711a50647b84");
                int count = matcher.groupCount();
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_8_10.coverage", "8bc6d90a-19a9-481f-922e-dbd75ab3e73d");
                String[] groups = new String[count];
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_8_10.coverage", "52ae4110-1f5e-4e37-b800-ebbd532e8cd1");
                for (int j = 0; j < count; j++) {
                    writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_8_10.coverage", "adb61e0b-7e70-4c9d-8873-482c809a2857");
                    groups[j] = matcher.group(j + 1);
                }
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_8_10.coverage", "cb899c40-b0b6-40ba-8f44-9fdf6c29b93d");
                return groups;
            }
        }
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_8_10.coverage", "66a71a80-55c5-4d5d-b608-5d9c5b1c9265");
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
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_8_10.coverage", "66ea4fb3-44ea-4765-ab1f-96db5a4af9cd");
        if (value == null) {
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_8_10.coverage", "d8eb9eac-caf7-497a-9131-f632750d5558");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_8_10.coverage", "9baba28f-f4ce-47c7-b52a-2e5083fc729d");
        for (int i = 0; i < patterns.length; i++) {
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_8_10.coverage", "9a688c03-b12a-4515-a8d9-430d3361ec63");
            Matcher matcher = patterns[i].matcher(value);
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_8_10.coverage", "07474433-5063-40aa-a117-92be4231baf5");
            if (matcher.matches()) {
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_8_10.coverage", "737d6866-f3e0-47ce-95d7-ab8d19869a61");
                int count = matcher.groupCount();
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_8_10.coverage", "84de511a-da4d-472b-be83-e4cdc7e21916");
                if (count == 1) {
                    writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_8_10.coverage", "8a0f491d-ccc8-469c-b98b-c71566cfce74");
                    return matcher.group(1);
                }
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_8_10.coverage", "1217356f-3a6c-4739-9a98-bfa36ff6ed68");
                StringBuilder buffer = new StringBuilder();
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_8_10.coverage", "39c9ca36-ec1f-462e-b7dc-6c63e206a778");
                for (int j = 0; j < count; j++) {
                    writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_8_10.coverage", "b69be2c1-2eb0-4dfc-a19b-7898b543b0b0");
                    String component = matcher.group(j + 1);
                    writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_8_10.coverage", "c07738a4-15cf-4f38-bb55-0d4080997539");
                    if (component != null) {
                        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_8_10.coverage", "c2862233-a3ea-427f-9b6d-067b9c84add5");
                        buffer.append(component);
                    }
                }
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_8_10.coverage", "ec908b04-604b-4b49-9cf7-203e1f9a7f07");
                return buffer.toString();
            }
        }
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_8_10.coverage", "b06ab1aa-6530-4aa4-80eb-cf621609c9e8");
        return null;
    }

    /**
     * Provide a String representation of this validator.
     * @return A String representation of this validator
     */
    public String toString() {
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_8_10.coverage", "25058ecb-055d-4487-b9f7-0a3edea914d3");
        StringBuilder buffer = new StringBuilder();
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_8_10.coverage", "71de9635-21d7-485d-9353-96598af7d46e");
        buffer.append("RegexValidator{");
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_8_10.coverage", "51e7c621-e4c0-49ef-a604-ff30caeaf7aa");
        for (int i = 0; i < patterns.length; i++) {
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_8_10.coverage", "25b41c76-4c49-49cc-98ac-9424ffa2ba62");
            if (i > 0) {
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_8_10.coverage", "1f79e284-edd2-4056-987b-00684e349de1");
                buffer.append(",");
            }
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_8_10.coverage", "2c40e1ab-5eb4-4b55-a888-39eb9074e51e");
            buffer.append(patterns[i].pattern());
        }
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_8_10.coverage", "951d8ee2-6dc4-460a-a838-7c452449c083");
        buffer.append("}");
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_8_10.coverage", "27f087f4-1572-4f3d-8bbc-3efb3f878381");
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
