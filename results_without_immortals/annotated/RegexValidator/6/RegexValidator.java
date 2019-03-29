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
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_6_10.coverage", "5f73cf58-e808-4daf-bff6-c2d64123100c");
        if (value == null) {
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_6_10.coverage", "ee05f59f-7428-4be2-9e59-d394a0530817");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_6_10.coverage", "26bd5f10-a4c8-46b4-baa0-37f35be1c56c");
        for (int i = 0; i < patterns.length; i++) {
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_6_10.coverage", "6d66be16-bec9-4917-b365-847f149628d9");
            if (patterns[i].matcher(value).matches()) {
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_6_10.coverage", "cc9c298f-7b01-414a-ac99-f82451b8dcac");
                return true;
            }
        }
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_6_10.coverage", "2566062f-9b9d-4af6-8632-deaa2a380f4f");
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
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_6_10.coverage", "8be552e7-60c0-4bdf-98c6-6269fb8e4411");
        if (value == null) {
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_6_10.coverage", "ed6c06c0-ed0c-4f3e-953a-6404ea6a3a38");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_6_10.coverage", "29392b69-4145-4790-82d9-6ce5977f8b68");
        for (int i = 0; i < patterns.length; i++) {
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_6_10.coverage", "856bc8da-4a83-434b-8dad-a7bae896d03f");
            Matcher matcher = patterns[i].matcher(value);
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_6_10.coverage", "b91bc501-9a11-4882-96f5-b1b35aa0f3ed");
            if (matcher.matches()) {
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_6_10.coverage", "8f999fb5-5568-4ad3-b731-d844e5d57e8a");
                int count = matcher.groupCount();
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_6_10.coverage", "e89e1c6e-80e2-42be-8334-08823571abd2");
                String[] groups = new String[count];
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_6_10.coverage", "34bce055-49c9-40de-8a71-a3df5c147e3c");
                for (int j = 0; j < count; j++) {
                    writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_6_10.coverage", "ea8c89b8-7382-4684-98cb-7e5a0a687bde");
                    groups[j] = matcher.group(j + 1);
                }
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_6_10.coverage", "016eaf6e-6952-4347-acec-177f175ed944");
                return groups;
            }
        }
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_6_10.coverage", "ebfe2294-87ee-41ee-9e3f-c3990f5d20dd");
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
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_6_10.coverage", "3ccdb48d-8727-40bd-a46f-70a959f4ed81");
        if (value == null) {
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_6_10.coverage", "ac0d70a4-d27b-41b0-8639-7d3e5879eb60");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_6_10.coverage", "948f701d-0e85-478b-9c91-d4228cdd7728");
        for (int i = 0; i < patterns.length; i++) {
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_6_10.coverage", "df2696da-2cc2-4d2e-b427-2746df018d8e");
            Matcher matcher = patterns[i].matcher(value);
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_6_10.coverage", "724e272a-5662-4ded-a8b7-c97bae5cbac6");
            if (matcher.matches()) {
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_6_10.coverage", "84c6dd81-17b3-4332-9458-12f3dfd9c6ef");
                int count = matcher.groupCount();
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_6_10.coverage", "0b68eb2e-8bc7-491f-bda7-84fddae3d053");
                if (count == 1) {
                    writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_6_10.coverage", "a2d02cb5-1208-48db-8127-2c42cce0e72c");
                    return matcher.group(1);
                }
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_6_10.coverage", "cddcdf1e-dc9b-442b-a19b-684f1a35412c");
                StringBuilder buffer = new StringBuilder();
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_6_10.coverage", "2e04d6ff-89be-423a-bb2a-605ac8964e19");
                for (int j = 0; j < count; j++) {
                    writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_6_10.coverage", "779b7d4e-c3bf-410f-88a1-92601ce3f7a7");
                    String component = matcher.group(j + 1);
                    writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_6_10.coverage", "c4dd45b8-fc26-453b-97ab-7ba4c0235bf1");
                    if (component != null) {
                        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_6_10.coverage", "f9d76ded-9f8d-478d-8749-b8def2826a03");
                        buffer.append(component);
                    }
                }
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_6_10.coverage", "d1face35-6d46-41b6-90c4-b7fddd96cf32");
                return buffer.toString();
            }
        }
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_6_10.coverage", "f135f27e-af5b-4452-a3c7-bb1e82e44c14");
        return null;
    }

    /**
     * Provide a String representation of this validator.
     * @return A String representation of this validator
     */
    public String toString() {
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_6_10.coverage", "23046e5e-ea23-434f-ac8b-b893d03c6d8f");
        StringBuilder buffer = new StringBuilder();
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_6_10.coverage", "277f3def-aac4-45c7-9df3-178e53e328bd");
        buffer.append("RegexValidator{");
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_6_10.coverage", "7a838091-f18e-45b9-8fad-036a02a5667e");
        for (int i = 0; i < patterns.length; i++) {
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_6_10.coverage", "e0d630ae-1cc6-46ca-b360-7f5ae1101a32");
            if (i > 0) {
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_6_10.coverage", "80f55203-67c9-472c-986f-2a8b69bb9a56");
                buffer.append(",");
            }
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_6_10.coverage", "19d77a7e-2572-4158-ac48-c9578df439c6");
            buffer.append(patterns[i].pattern());
        }
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_6_10.coverage", "36031088-b629-43e6-ad90-1049644e2089");
        buffer.append("}");
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_6_10.coverage", "5f21172c-597a-4fe3-ad07-da645fd4469d");
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
