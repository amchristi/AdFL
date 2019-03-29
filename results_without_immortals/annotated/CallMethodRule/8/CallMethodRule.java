package org.apache.commons.digester3;

import static java.lang.System.arraycopy;
import static java.lang.String.format;
import static java.util.Arrays.fill;
import static org.apache.commons.beanutils.ConvertUtils.convert;
import static org.apache.commons.beanutils.MethodUtils.invokeExactMethod;
import static org.apache.commons.beanutils.MethodUtils.invokeMethod;
import java.util.Formatter;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import java.io.*;

/**
 * <p>
 * Rule implementation that calls a method on an object on the stack (normally the top/parent object), passing arguments
 * collected from subsequent <code>CallParamRule</code> rules or from the body of this element.
 * </p>
 * <p>
 * By using {@link #CallMethodRule(String methodName)} a method call can be made to a method which accepts no arguments.
 * </p>
 * <p>
 * Incompatible method parameter types are converted using <code>org.apache.commons.beanutils.ConvertUtils</code>.
 * </p>
 * <p>
 * This rule now uses {@link org.apache.commons.beanutils.MethodUtils#invokeMethod} by default.
 * This increases the kinds of methods successfully and allows primitives to be matched by passing in wrapper classes.
 * There are rare cases when {@link org.apache.commons.beanutils.MethodUtils#invokeExactMethod} (the old default) is
 * required. This method is much stricter in it's reflection.
 * Setting the <code>UseExactMatch</code> to true reverts to the use of this method.
 * </p>
 * <p>
 * Note that the target method is invoked when the <i>end</i> of the tag the CallMethodRule fired on is encountered,
 * <i>not</i> when the last parameter becomes available. This implies that rules which fire on tags nested within the
 * one associated with the CallMethodRule will fire before the CallMethodRule invokes the target method. This behavior
 * is not configurable.
 * </p>
 * <p>
 * Note also that if a CallMethodRule is expecting exactly one parameter and that parameter is not available (eg
 * CallParamRule is used with an attribute name but the attribute does not exist) then the method will not be invoked.
 * If a CallMethodRule is expecting more than one parameter, then it is always invoked, regardless of whether the
 * parameters were available or not; missing parameters are converted to the appropriate target type by calling
 * ConvertUtils.convert. Note that the default ConvertUtils converters for the String type returns a null when passed a
 * null, meaning that CallMethodRule will passed null for all String parameters for which there is no parameter info
 * available from the XML. However parameters of type Float and Integer will be passed a real object containing a zero
 * value as that is the output of the default ConvertUtils converters for those types when passed a null. You can
 * register custom converters to change this behavior; see the BeanUtils library documentation for more info.
 * </p>
 * <p>
 * Note that when a constructor is used with paramCount=0, indicating that the body of the element is to be passed to
 * the target method, an empty element will cause an <i>empty string</i> to be passed to the target method, not null.
 * And if automatic type conversion is being applied (ie if the target function takes something other than a string as a
 * parameter) then the conversion will fail if the converter class does not accept an empty string as valid input.
 * </p>
 * <p>
 * CallMethodRule has a design flaw which can cause it to fail under certain rule configurations. All CallMethodRule
 * instances share a single parameter stack, and all CallParamRule instances simply store their data into the
 * parameter-info structure that is on the top of the stack. This means that two CallMethodRule instances cannot be
 * associated with the same pattern without getting scrambled parameter data. This same issue also applies when a
 * CallMethodRule matches some element X, a different CallMethodRule matches a child element Y and some of the
 * CallParamRules associated with the first CallMethodRule match element Y or one of its child elements. This issue has
 * been present since the very first release of Digester. Note, however, that this configuration of CallMethodRule
 * instances is not commonly required.
 * </p>
 */
public class CallMethodRule extends Rule {

    /**
     * Construct a "call method" rule with the specified method name. The parameter types (if any) default to
     * java.lang.String.
     *
     * @param methodName Method name of the parent method to call
     * @param paramCount The number of parameters to collect, or zero for a single argument from the body of this
     *            element.
     */
    public CallMethodRule(String methodName, int paramCount) {
        this(0, methodName, paramCount);
    }

    /**
     * Construct a "call method" rule with the specified method name. The parameter types (if any) default to
     * java.lang.String.
     *
     * @param targetOffset location of the target object. Positive numbers are relative to the top of the digester
     *            object stack. Negative numbers are relative to the bottom of the stack. Zero implies the top object on
     *            the stack.
     * @param methodName Method name of the parent method to call
     * @param paramCount The number of parameters to collect, or zero for a single argument from the body of this
     *            element.
     */
    public CallMethodRule(int targetOffset, String methodName, int paramCount) {
        this.targetOffset = targetOffset;
        this.methodName = methodName;
        this.paramCount = paramCount;
        if (paramCount == 0) {
            this.paramTypes = new Class[] { String.class };
        } else {
            this.paramTypes = new Class[paramCount];
            fill(this.paramTypes, String.class);
        }
    }

    /**
     * Construct a "call method" rule with the specified method name. The method should accept no parameters.
     *
     * @param methodName Method name of the parent method to call
     */
    public CallMethodRule(String methodName) {
        this(0, methodName, 0, (Class[]) null);
    }

    /**
     * Construct a "call method" rule with the specified method name. The method should accept no parameters.
     *
     * @param targetOffset location of the target object. Positive numbers are relative to the top of the digester
     *            object stack. Negative numbers are relative to the bottom of the stack. Zero implies the top object on
     *            the stack.
     * @param methodName Method name of the parent method to call
     */
    public CallMethodRule(int targetOffset, String methodName) {
        this(targetOffset, methodName, 0, (Class[]) null);
    }

    /**
     * Construct a "call method" rule with the specified method name and parameter types. If <code>paramCount</code> is
     * set to zero the rule will use the body of this element as the single argument of the method, unless
     * <code>paramTypes</code> is null or empty, in this case the rule will call the specified method with no arguments.
     *
     * @param methodName Method name of the parent method to call
     * @param paramCount The number of parameters to collect, or zero for a single argument from the body of ths element
     * @param paramTypes The Java class names of the arguments (if you wish to use a primitive type, specify the
     *            corresonding Java wrapper class instead, such as <code>java.lang.Boolean</code> for a
     *            <code>boolean</code> parameter)
     */
    public CallMethodRule(String methodName, int paramCount, String[] paramTypes) {
        this(0, methodName, paramCount, paramTypes);
    }

    /**
     * Construct a "call method" rule with the specified method name and parameter types. If <code>paramCount</code> is
     * set to zero the rule will use the body of this element as the single argument of the method, unless
     * <code>paramTypes</code> is null or empty, in this case the rule will call the specified method with no arguments.
     *
     * @param targetOffset location of the target object. Positive numbers are relative to the top of the digester
     *            object stack. Negative numbers are relative to the bottom of the stack. Zero implies the top object on
     *            the stack.
     * @param methodName Method name of the parent method to call
     * @param paramCount The number of parameters to collect, or zero for a single argument from the body of the element
     * @param paramTypes The Java class names of the arguments (if you wish to use a primitive type, specify the
     *            corresponding Java wrapper class instead, such as <code>java.lang.Boolean</code> for a
     *            <code>boolean</code> parameter)
     */
    public CallMethodRule(int targetOffset, String methodName, int paramCount, String[] paramTypes) {
        this.targetOffset = targetOffset;
        this.methodName = methodName;
        this.paramCount = paramCount;
        if (paramTypes == null) {
            this.paramTypes = new Class[paramCount];
            fill(this.paramTypes, String.class);
        } else {
            this.paramClassNames = new String[paramTypes.length];
            arraycopy(paramTypes, 0, this.paramClassNames, 0, paramTypes.length);
        }
    }

    /**
     * Construct a "call method" rule with the specified method name and parameter types. If <code>paramCount</code> is
     * set to zero the rule will use the body of this element as the single argument of the method, unless
     * <code>paramTypes</code> is null or empty, in this case the rule will call the specified method with no arguments.
     *
     * @param methodName Method name of the parent method to call
     * @param paramCount The number of parameters to collect, or zero for a single argument from the body of the element
     * @param paramTypes The Java classes that represent the parameter types of the method arguments (if you wish to use
     *            a primitive type, specify the corresponding Java wrapper class instead, such as
     *            <code>java.lang.Boolean.TYPE</code> for a <code>boolean</code> parameter)
     */
    public CallMethodRule(String methodName, int paramCount, Class<?> paramTypes[]) {
        this(0, methodName, paramCount, paramTypes);
    }

    /**
     * Construct a "call method" rule with the specified method name and parameter types. If <code>paramCount</code> is
     * set to zero the rule will use the body of this element as the single argument of the method, unless
     * <code>paramTypes</code> is null or empty, in this case the rule will call the specified method with no arguments.
     *
     * @param targetOffset location of the target object. Positive numbers are relative to the top of the digester
     *            object stack. Negative numbers are relative to the bottom of the stack. Zero implies the top object on
     *            the stack.
     * @param methodName Method name of the parent method to call
     * @param paramCount The number of parameters to collect, or zero for a single argument from the body of the element
     * @param paramTypes The Java classes that represent the parameter types of the method arguments (if you wish to use
     *            a primitive type, specify the corresponding Java wrapper class instead, such as
     *            <code>java.lang.Boolean.TYPE</code> for a <code>boolean</code> parameter)
     */
    public CallMethodRule(int targetOffset, String methodName, int paramCount, Class<?>[] paramTypes) {
        this.targetOffset = targetOffset;
        this.methodName = methodName;
        this.paramCount = paramCount;
        if (paramTypes == null) {
            this.paramTypes = new Class<?>[paramCount];
            fill(this.paramTypes, String.class);
        } else {
            this.paramTypes = new Class<?>[paramTypes.length];
            arraycopy(paramTypes, 0, this.paramTypes, 0, paramTypes.length);
        }
    }

    /**
     * The body text collected from this element.
     */
    protected String bodyText = null;

    /**
     * location of the target object for the call, relative to the top of the digester object stack. The default value
     * of zero means the target object is the one on top of the stack.
     */
    protected int targetOffset = 0;

    /**
     * The method name to call on the parent object.
     */
    protected String methodName = null;

    /**
     * The number of parameters to collect from <code>MethodParam</code> rules. If this value is zero, a single
     * parameter will be collected from the body of this element.
     */
    protected int paramCount = 0;

    /**
     * The parameter types of the parameters to be collected.
     */
    protected Class<?>[] paramTypes = null;

    /**
     * The names of the classes of the parameters to be collected. This attribute allows creation of the classes to be
     * postponed until the digester is set.
     */
    private String[] paramClassNames = null;

    /**
     * Should <code>MethodUtils.invokeExactMethod</code> be used for reflection.
     */
    private boolean useExactMatch = false;

    /**
     * Should <code>MethodUtils.invokeExactMethod</code> be used for the reflection.
     *
     * @return true, if <code>MethodUtils.invokeExactMethod</code> Should be used for the reflection,
     *         false otherwise
     */
    public boolean getUseExactMatch() {
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "75e0fd81-3672-4aa5-b656-1418135b29f7");
        return useExactMatch;
    }

    /**
     * Set whether <code>MethodUtils.invokeExactMethod</code> should be used for the reflection.
     *
     * @param useExactMatch The <code>MethodUtils.invokeExactMethod</code> flag
     */
    public void setUseExactMatch(boolean useExactMatch) {
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "24c5df29-a8d1-487c-b38e-7c339a391748");
        this.useExactMatch = useExactMatch;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setDigester(Digester digester) {
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "6bab01a7-cd96-4f1e-b6e8-121465c6de23");
        super.setDigester(digester);
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "ae007bd8-19b3-4b39-ac3b-d496ffb7caa9");
        if (this.paramClassNames != null) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "c8a4fcc0-4fbe-4540-b4fe-b295f82cb21f");
            this.paramTypes = new Class<?>[paramClassNames.length];
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "5227dd58-0aa4-4df6-aded-e12384e48693");
            for (int i = 0; i < this.paramClassNames.length; i++) {
                writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "6b65e960-1e6b-486e-a89a-f315f0d58f44");
                try {
                    writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "782fa0d9-5474-43df-b768-61788615ffce");
                    this.paramTypes[i] = digester.getClassLoader().loadClass(this.paramClassNames[i]);
                } catch (ClassNotFoundException e) {
                }
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void begin(String namespace, String name, Attributes attributes) throws Exception {
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "549e1e14-f01e-4423-87d1-1be5040428ba");
        if (paramCount > 0) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "dfdf7ca9-a345-4a3e-815c-19ab9b658516");
            Object parameters[] = new Object[paramCount];
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "88a299ac-7654-4f84-9d71-66e7dac7e410");
            fill(parameters, null);
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "037e963d-19ce-4b18-82f9-189be24632d2");
            getDigester().pushParams(parameters);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void body(String namespace, String name, String text) throws Exception {
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "bf3a42d2-ffd8-4f6a-949f-6a8bab2e472e");
        if (paramCount == 0) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "ac6c7b0c-b9b2-4982-9041-43c2e55df17f");
            this.bodyText = text.trim();
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void end(String namespace, String name) throws Exception {
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "e327f719-e211-4322-9e78-aceb0b5efb6e");
        Object[] parameters;
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "b2be5642-9789-4d8d-be80-a993cda667da");
        if (paramCount > 0) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "b0be7cab-ec5f-4cb7-b419-5fa66d8eab45");
            parameters = getDigester().popParams();
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "464bd658-ae44-434f-a323-337d09b3c134");
            if (getDigester().getLogger().isTraceEnabled()) {
                writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "e353ade6-eeec-4d36-b568-e1391e36c579");
                for (int i = 0, size = parameters.length; i < size; i++) {
                    writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "1e46ac7e-b36e-4330-bdd5-e823e3aaf9a5");
                    getDigester().getLogger().trace(format("[CallMethodRule]{%s} parameters[%s]=%s", getDigester().getMatch(), i, parameters[i]));
                }
            }
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "21a6cde8-a345-4d0f-9a87-1a35b21bd9a3");
            if (paramCount == 1 && parameters[0] == null) {
                writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "a8c9c083-0cb5-4762-aa86-887f786969e7");
                return;
            }
        } else if (paramTypes != null && paramTypes.length != 0) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "ea4a51a1-52e5-4bca-b663-1a9f476e962b");
            if (bodyText == null) {
                writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "f9621e95-4bda-44cb-b796-631832ee67ba");
                return;
            }
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "d0727ca4-48d2-4500-9d6f-490b7e21c99e");
            parameters = new Object[] { bodyText };
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "3f4cb03e-b9d7-4833-a3f9-84856a37ffd5");
            if (paramTypes.length == 0) {
                writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "3fb6938e-6fba-4992-b62a-16372f1eb307");
                paramTypes = new Class[] { String.class };
            }
        } else {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "99796296-da3f-41d6-bf64-e1143574b483");
            parameters = new Object[0];
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "de337e12-5a41-41d8-9be2-248a67b1f3a6");
            paramTypes = new Class<?>[0];
        }
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "df9c9fa5-8df5-4202-acf5-bcd77aa6b93e");
        Object[] paramValues = new Object[paramTypes.length];
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "696d1e07-1a70-49d8-9b7c-b1ea8af67b6a");
        for (int i = 0; i < paramTypes.length; i++) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "46eaf2ca-c041-4344-9bc4-5c0c5bad9b3c");
            if (parameters[i] == null || (parameters[i] instanceof String && !String.class.isAssignableFrom(paramTypes[i]))) {
                writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "252bc3e8-491b-41e5-8a47-cf86672723f3");
                paramValues[i] = convert((String) parameters[i], paramTypes[i]);
            } else {
                writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "0806b5b7-c54a-495b-9f96-168120c99fab");
                paramValues[i] = parameters[i];
            }
        }
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "6f7ab523-5a84-4efa-be2d-8bd8e8b640ad");
        Object target;
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "9e62c861-4f55-4134-bb0f-dc604252adea");
        if (targetOffset >= 0) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "d31e7823-5341-47d4-a3c2-a211155d4835");
            target = getDigester().peek(targetOffset);
        } else {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "f39b4706-d4c0-4253-8d48-5f17aa48462d");
            target = getDigester().peek(getDigester().getCount() + targetOffset);
        }
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "c0019e84-f66e-475e-bddd-36317344412a");
        if (target == null) {
        }
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "bb8a8f80-3024-47aa-94e8-36f0de62ab2b");
        if (getDigester().getLogger().isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "0e2a0843-2186-4585-8703-66aa84a2667d");
            Formatter formatter = new Formatter().format("[CallMethodRule]{%s} Call %s.%s(", getDigester().getMatch(), target.getClass().getName(), methodName);
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "0f3d4f41-ef31-4eea-91f2-eb8f0d2d2bda");
            for (int i = 0; i < paramValues.length; i++) {
                writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "84f7ac4c-98be-4439-9c75-61e368ce5e84");
                formatter.format("%s%s/%s", (i > 0 ? ", " : ""), paramValues[i], paramTypes[i].getName());
            }
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "572c6996-36fe-4255-acab-f606dc748309");
            formatter.format(")");
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "c5937bcf-6f00-4daf-a141-5deab13891b6");
            getDigester().getLogger().debug(formatter.toString());
        }
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "741b7b94-a58d-4034-8bae-9ee3e53b25a1");
        Object result = null;
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "5b7d6273-e564-4bf5-bd65-edb21aa1a28d");
        if (useExactMatch) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "8da8248c-1333-4ee4-a403-6c9ff8bae04b");
            result = invokeExactMethod(target, methodName, paramValues, paramTypes);
        } else {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "b6e43b71-8790-4931-b913-6033b19e16dc");
            result = invokeMethod(target, methodName, paramValues, paramTypes);
        }
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "32d9ce92-2bd8-449d-8460-ad7f9dbe70b4");
        processMethodCallResult(result);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void finish() throws Exception {
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "85e0dbd0-9360-42a4-a4c8-87ab8ad183dc");
        bodyText = null;
    }

    /**
     * Subclasses may override this method to perform additional processing of the invoked method's result.
     *
     * @param result the Object returned by the method invoked, possibly null
     */
    protected void processMethodCallResult(Object result) {
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "db4bb4c1-6333-4114-bfef-13d55d6321a1");
        Formatter formatter = new Formatter().format("CallMethodRule[methodName=%s, paramCount=%s, paramTypes={", methodName, paramCount);
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "a04b30e5-c87a-4802-8bc1-520826a2b135");
        if (paramTypes != null) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "0cc35979-64a5-44b0-a0db-337c837b5d69");
            for (int i = 0; i < paramTypes.length; i++) {
                writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "3d6cd83a-8604-4e28-82f0-4d8218f77fb9");
                formatter.format("%s%s", (i > 0 ? ", " : ""), (paramTypes[i] != null ? paramTypes[i].getName() : "null"));
            }
        }
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "c11b55b3-8478-4f95-ae95-ad0fa5c3d5bf");
        formatter.format("}]");
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_8_10.coverage", "e097dbb9-03af-4467-90a7-9adc7d4363be");
        return (formatter.toString());
    }

    public void writeline(String fullFilePath, String text) {
        try {
            java.io.File file = new File(fullFilePath);
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
