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
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_2_10.coverage", "d27eb7d6-c3b2-4ae1-97c4-50cabf46cf4d");
        if (value == null) {
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_2_10.coverage", "fccf3ac4-941b-458b-93f3-ce33bb41036c");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_2_10.coverage", "beafb0a0-c1d0-4028-84e2-0bc7e206c62a");
        for (int i = 0; i < patterns.length; i++) {
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_2_10.coverage", "a3cdcb76-208a-45e0-beba-ee3e59225191");
            if (patterns[i].matcher(value).matches()) {
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_2_10.coverage", "4f182ee9-5073-4b5c-a5cd-98dbb4a66061");
                return true;
            }
        }
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_2_10.coverage", "4fe89524-469a-4d2a-b4e4-6636d931b09f");
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
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_2_10.coverage", "5be0a1e1-6f74-46ca-823d-35eba40abd22");
        if (value == null) {
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_2_10.coverage", "7dce960c-f89a-4212-8d4d-e511476c0f5f");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_2_10.coverage", "3a7a623e-c27a-4c75-a74f-ac1399e0f770");
        for (int i = 0; i < patterns.length; i++) {
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_2_10.coverage", "31d608f8-e619-44a2-a89d-06f3a279a0e2");
            Matcher matcher = patterns[i].matcher(value);
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_2_10.coverage", "2b8ed460-cf22-425d-b872-24c9180508d8");
            if (matcher.matches()) {
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_2_10.coverage", "a8e80bde-f666-4ffe-b7d0-34999d162696");
                int count = matcher.groupCount();
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_2_10.coverage", "dd24e48a-cc05-456b-922d-16082993b5d4");
                String[] groups = new String[count];
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_2_10.coverage", "aa085a5f-9103-4550-8757-1c6adb99e36e");
                for (int j = 0; j < count; j++) {
                    writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_2_10.coverage", "d040676e-fc98-4359-9e8d-d204e5f48541");
                    groups[j] = matcher.group(j + 1);
                }
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_2_10.coverage", "b0a0286a-a485-4e1d-aca3-e726ddeda972");
                return groups;
            }
        }
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_2_10.coverage", "d3d4a5fc-efb5-467d-917c-44c76b026a63");
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
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_2_10.coverage", "5bc0d47e-79c0-4228-a4ca-dbed354064be");
        if (value == null) {
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_2_10.coverage", "42e635e8-650f-4a8f-93df-f1a7e7bd2095");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_2_10.coverage", "784be516-8b5d-44e3-8ab0-9a589a23d176");
        for (int i = 0; i < patterns.length; i++) {
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_2_10.coverage", "ef7d9f70-a9fb-4173-b995-6cfc2f4e61ba");
            Matcher matcher = patterns[i].matcher(value);
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_2_10.coverage", "cfcb41c7-ebf1-4542-817a-86aa26afd35b");
            if (matcher.matches()) {
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_2_10.coverage", "251e2858-c282-44a2-82e3-407abdc0c07e");
                int count = matcher.groupCount();
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_2_10.coverage", "4ff9b0bf-cf58-43f1-82fa-42d074618165");
                if (count == 1) {
                    writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_2_10.coverage", "28dd3576-c890-4e19-a207-262c3e99b675");
                    return matcher.group(1);
                }
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_2_10.coverage", "737da0da-7e3b-4a88-8510-d0ee0d6cfa94");
                StringBuilder buffer = new StringBuilder();
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_2_10.coverage", "7f3d44a6-e811-4250-b092-8930dafe1515");
                for (int j = 0; j < count; j++) {
                    writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_2_10.coverage", "b569f850-c0fd-4d1b-8b7a-25718016b2ab");
                    String component = matcher.group(j + 1);
                    writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_2_10.coverage", "69685736-cd33-4038-8efe-fc060e0f5a1e");
                    if (component != null) {
                        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_2_10.coverage", "e5f3dbb1-8fb7-42e6-836f-754c81519d74");
                        buffer.append(component);
                    }
                }
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_2_10.coverage", "d06f54c7-223b-4156-815d-59e96f973494");
                return buffer.toString();
            }
        }
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_2_10.coverage", "8f5f2d15-862e-474d-b486-32feadc6176d");
        return null;
    }

    /**
     * Provide a String representation of this validator.
     * @return A String representation of this validator
     */
    public String toString() {
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_2_10.coverage", "04e16635-1929-47a0-b455-32e763823f08");
        StringBuilder buffer = new StringBuilder();
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_2_10.coverage", "1ff5d24b-9e69-4e4f-bab7-7b69c8f0919c");
        buffer.append("RegexValidator{");
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_2_10.coverage", "bcdb37d9-6dd3-40b7-8362-efe7a2c9c1d0");
        for (int i = 0; i < patterns.length; i++) {
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_2_10.coverage", "56963d9c-ddb4-4b7a-ba8a-e29f38c3ddf0");
            if (i > 0) {
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_2_10.coverage", "2100df37-977f-4d6d-8b1c-b062795e06f5");
                buffer.append(",");
            }
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_2_10.coverage", "01f9971b-f8e1-44f1-9d01-93cda0ac6ff8");
            buffer.append(patterns[i].pattern());
        }
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_2_10.coverage", "e50cdcb5-6e69-4e70-94c6-91e8136c78e0");
        buffer.append("}");
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_2_10.coverage", "50ea0347-02f9-4951-83ef-b79201935d5f");
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
