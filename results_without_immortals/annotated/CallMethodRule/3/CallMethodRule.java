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
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "9f603067-0d07-4195-939c-0dbd29ab62e7");
        return useExactMatch;
    }

    /**
     * Set whether <code>MethodUtils.invokeExactMethod</code> should be used for the reflection.
     *
     * @param useExactMatch The <code>MethodUtils.invokeExactMethod</code> flag
     */
    public void setUseExactMatch(boolean useExactMatch) {
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "c3393641-29e5-4999-87ef-30f5b872f3ce");
        this.useExactMatch = useExactMatch;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setDigester(Digester digester) {
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "1dbca102-8ad0-4e8b-8ce0-18499f48e2c9");
        super.setDigester(digester);
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "d4b19d51-8b91-4359-8234-45eea8d862f2");
        if (this.paramClassNames != null) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "8107e074-679b-43db-9787-a445da192292");
            this.paramTypes = new Class<?>[paramClassNames.length];
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "a1942f68-75a4-4e41-900c-eea4c144cf16");
            for (int i = 0; i < this.paramClassNames.length; i++) {
                writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "824830bd-bd9a-4f70-a1b3-f3009f60876a");
                try {
                    writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "ec4d305d-3500-4b9d-ac48-f2961f86f438");
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
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "fd662563-d924-4521-bab5-947a04142343");
        if (paramCount > 0) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "2336cb04-f258-42c8-b809-dd03c26b6f0f");
            Object parameters[] = new Object[paramCount];
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "a7f9c828-3318-4ed1-ae04-666004010998");
            fill(parameters, null);
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "a2942443-ae21-46fd-a8c2-11441e15d783");
            getDigester().pushParams(parameters);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void body(String namespace, String name, String text) throws Exception {
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "17443bc0-3379-46cd-9417-605b9807551c");
        if (paramCount == 0) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "3c57442f-2f5f-40a5-ba2c-0384fb05d36f");
            this.bodyText = text.trim();
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void end(String namespace, String name) throws Exception {
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "08939269-7d68-4dbd-9325-052e4e84e33d");
        Object[] parameters;
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "194dee70-ec27-431a-8cdd-faebf572f492");
        if (paramCount > 0) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "29608a73-546c-49b3-9bf5-e89a33372bc3");
            parameters = getDigester().popParams();
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "fc80b878-0b6e-4e2a-bf6e-55e46d37b3c5");
            if (getDigester().getLogger().isTraceEnabled()) {
                writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "f1fb8b9b-0d39-40f1-9a52-81a4b81e2121");
                for (int i = 0, size = parameters.length; i < size; i++) {
                    writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "9e2a18d1-ace4-4387-bd32-be584ace1488");
                    getDigester().getLogger().trace(format("[CallMethodRule]{%s} parameters[%s]=%s", getDigester().getMatch(), i, parameters[i]));
                }
            }
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "0f79dea1-21f8-4dee-9035-da524a35e516");
            if (paramCount == 1 && parameters[0] == null) {
                writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "6ee0c611-d3a8-4d3a-b45e-5d8bc2f96fa5");
                return;
            }
        } else if (paramTypes != null && paramTypes.length != 0) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "33735d8a-aad9-42d3-a1dd-52d1d3be5a59");
            if (bodyText == null) {
                writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "572b299b-ca55-4a04-81e5-fd74d2d06286");
                return;
            }
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "d9a1759f-bd8d-4adb-9ad0-84cadbee7a1a");
            parameters = new Object[] { bodyText };
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "48754faf-5da7-4e89-9943-63eac4d92b97");
            if (paramTypes.length == 0) {
                writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "ed69e8cc-ed28-4faf-b65f-8a8a67679f76");
                paramTypes = new Class[] { String.class };
            }
        } else {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "10088f80-248d-4b16-81a9-fdd65f687fc4");
            parameters = new Object[0];
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "116728e8-c64e-489a-8711-d791b6d05ee1");
            paramTypes = new Class<?>[0];
        }
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "e4a918d0-431d-406c-ac81-e851e262bf90");
        Object[] paramValues = new Object[paramTypes.length];
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "b5aad18b-0ff8-4e43-be42-4060445177bf");
        for (int i = 0; i < paramTypes.length; i++) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "6fedfd6e-d6f8-489d-b267-e338682aa78c");
            if (parameters[i] == null || (parameters[i] instanceof String && !String.class.isAssignableFrom(paramTypes[i]))) {
                writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "712a6d7b-3eea-4c65-89e5-3ac0cee7956b");
                paramValues[i] = convert((String) parameters[i], paramTypes[i]);
            } else {
                writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "95872320-1e3d-41dd-9e21-60e1bbf635d7");
                paramValues[i] = parameters[i];
            }
        }
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "69d4faf7-fdc4-4f4e-8403-2831c2b43c13");
        Object target;
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "f50fb9d0-5cd3-411f-91a1-c73bee16f254");
        if (targetOffset >= 0) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "498a43b5-0ac8-4969-b673-44a7d0352af9");
            target = getDigester().peek(targetOffset);
        } else {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "d0c7c779-0184-4b5e-8505-e803459e9095");
            target = getDigester().peek(getDigester().getCount() + targetOffset);
        }
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "9a2f39d4-b742-41ce-8f2f-eaaec725c37d");
        if (target == null) {
        }
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "80f94815-19de-4bc5-b888-583117fb9834");
        if (getDigester().getLogger().isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "efd3aeba-6907-4b1d-9481-b3a0a37f34c8");
            Formatter formatter = new Formatter().format("[CallMethodRule]{%s} Call %s.%s(", getDigester().getMatch(), target.getClass().getName(), methodName);
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "89ca9cd9-ce69-499f-bc21-456b1d5ad6c8");
            for (int i = 0; i < paramValues.length; i++) {
                writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "34985108-9294-404d-9dc6-50f6b20a11a2");
                formatter.format("%s%s/%s", (i > 0 ? ", " : ""), paramValues[i], paramTypes[i].getName());
            }
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "a91db8c7-a319-422b-b4ce-2947720f003f");
            formatter.format(")");
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "426f0db3-7de2-4968-9d9e-82247a20ac6f");
            getDigester().getLogger().debug(formatter.toString());
        }
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "ceb0a0cb-85a4-4457-85f7-f9e3063043d6");
        Object result = null;
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "53c90153-04ce-47df-98b8-dd173e9d4205");
        if (useExactMatch) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "bd4ab3e6-0aaa-4ee1-97b9-65c418fbab85");
            result = invokeExactMethod(target, methodName, paramValues, paramTypes);
        } else {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "bf13102a-c5a5-4f82-adac-13b70e142105");
            result = invokeMethod(target, methodName, paramValues, paramTypes);
        }
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "beec31c2-c404-46a4-ac22-ad4984096158");
        processMethodCallResult(result);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void finish() throws Exception {
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "4be272b1-57b7-42ec-9ee8-68e5f06d642f");
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
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "961fa6e8-545b-4753-a423-c0223ffff63d");
        Formatter formatter = new Formatter().format("CallMethodRule[methodName=%s, paramCount=%s, paramTypes={", methodName, paramCount);
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "296804a2-e65f-4e26-a96d-f2527c760a7c");
        if (paramTypes != null) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "cc51dd0a-975e-4487-a5fe-ce5141ffeb41");
            for (int i = 0; i < paramTypes.length; i++) {
                writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "89bbd38b-7d1f-4c46-a08b-5b58c97ca983");
                formatter.format("%s%s", (i > 0 ? ", " : ""), (paramTypes[i] != null ? paramTypes[i].getName() : "null"));
            }
        }
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "34adc053-b0ab-4318-955b-2168d32e70cd");
        formatter.format("}]");
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_3_10.coverage", "992ad8f4-d100-488b-8337-aa048ec6e394");
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
