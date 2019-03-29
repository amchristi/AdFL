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
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_9_10.coverage", "a1a57aa0-04b0-4119-8552-eb9f4f625153");
        if (value == null) {
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_9_10.coverage", "85a4779d-293d-4257-aa43-70331c4e713a");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_9_10.coverage", "5d9308a6-94b1-44ab-b4f7-fbbcf5fe2729");
        for (int i = 0; i < patterns.length; i++) {
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_9_10.coverage", "8486facc-2143-4edf-9431-5e6f892c2776");
            if (patterns[i].matcher(value).matches()) {
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_9_10.coverage", "ae80bff0-92b7-43f9-ba3f-6b439d1c13ff");
                return true;
            }
        }
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_9_10.coverage", "a1b5bc59-acc1-4c87-8b37-e105ae0c35bd");
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
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_9_10.coverage", "086c0c20-48c3-4a96-81d1-46f5c40e81e0");
        if (value == null) {
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_9_10.coverage", "45df3090-1cbf-418d-be3f-703668596aef");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_9_10.coverage", "849d2b41-8774-4ce0-aa6c-361e8e6f1958");
        for (int i = 0; i < patterns.length; i++) {
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_9_10.coverage", "babdf6fd-6817-44f4-8158-0566ecc37513");
            Matcher matcher = patterns[i].matcher(value);
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_9_10.coverage", "6f8992a6-54cc-4ddb-a390-7c39aa0dcc69");
            if (matcher.matches()) {
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_9_10.coverage", "670e306a-0317-4732-9e3b-4b2cd783f845");
                int count = matcher.groupCount();
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_9_10.coverage", "9d1a3a3e-77bd-4358-8d19-65b7f042da2d");
                String[] groups = new String[count];
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_9_10.coverage", "3ecf3f97-a671-4ecb-bc62-22bd400679d7");
                for (int j = 0; j < count; j++) {
                    writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_9_10.coverage", "dea6f9f5-f477-4ea5-840c-65bcab04067f");
                    groups[j] = matcher.group(j + 1);
                }
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_9_10.coverage", "316cbe80-d657-4ed8-a2b5-83746af267ad");
                return groups;
            }
        }
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_9_10.coverage", "70f1c5b1-ea3a-4f29-8855-f788f3e72727");
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
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_9_10.coverage", "98360a42-08ef-4bb6-8455-877561c8137c");
        if (value == null) {
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_9_10.coverage", "10fd3f31-493c-43bc-a855-06a8de6da499");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_9_10.coverage", "98b15bc3-b948-4acd-9ddb-76ed4e0b2888");
        for (int i = 0; i < patterns.length; i++) {
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_9_10.coverage", "654462fe-9f61-4873-912b-6441459b9c13");
            Matcher matcher = patterns[i].matcher(value);
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_9_10.coverage", "76d4d73b-ba82-41fa-95c2-938b4ca00d04");
            if (matcher.matches()) {
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_9_10.coverage", "ae5267ff-b2ee-411d-a88d-83684b13421a");
                int count = matcher.groupCount();
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_9_10.coverage", "b52bcc4b-ce52-4f86-9b40-1c42d4d0d42c");
                if (count == 1) {
                    writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_9_10.coverage", "edc2a8a0-0183-4ac2-bc95-25150b44880d");
                    return matcher.group(1);
                }
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_9_10.coverage", "baa4a547-fba9-487e-82f9-eb57a823a4f8");
                StringBuilder buffer = new StringBuilder();
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_9_10.coverage", "ca4b8309-e5c1-4c39-b8a2-6145cb74fc77");
                for (int j = 0; j < count; j++) {
                    writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_9_10.coverage", "dbd2799f-c0be-47f5-8496-607844fa9746");
                    String component = matcher.group(j + 1);
                    writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_9_10.coverage", "420b4900-1dab-4523-aa28-9d1832214c36");
                    if (component != null) {
                        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_9_10.coverage", "7689240d-2654-4bac-8f53-b7a83bd91b0a");
                        buffer.append(component);
                    }
                }
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_9_10.coverage", "d5e4627b-6087-4687-b207-2601a2e6fdb1");
                return buffer.toString();
            }
        }
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_9_10.coverage", "826789e2-4a24-4b02-9dfc-9c4262b9bb47");
        return null;
    }

    /**
     * Provide a String representation of this validator.
     * @return A String representation of this validator
     */
    public String toString() {
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_9_10.coverage", "373ba57c-bf8a-4885-8489-1ca0a3beffa7");
        StringBuilder buffer = new StringBuilder();
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_9_10.coverage", "40906a4c-a50e-44e2-b382-7c936f503d1f");
        buffer.append("RegexValidator{");
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_9_10.coverage", "61fb424b-81e0-4df2-bae7-c8e10cb473a9");
        for (int i = 0; i < patterns.length; i++) {
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_9_10.coverage", "7b820f26-b4a5-47e7-a5f7-2681e551688c");
            if (i > 0) {
                writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_9_10.coverage", "fead7514-3855-46a5-beb4-4b77ea712cf5");
                buffer.append(",");
            }
            writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_9_10.coverage", "fc1fc411-7f09-47c6-b378-a8825a452264");
            buffer.append(patterns[i].pattern());
        }
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_9_10.coverage", "fe8bf4d2-c639-4efc-b167-d8830c5ffaa5");
        buffer.append("}");
        writeline("/home/ubuntu/results/coverage/RegexValidator/RegexValidator_9_10.coverage", "f3f71ef6-368f-4682-ac40-acf69acc6690");
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
