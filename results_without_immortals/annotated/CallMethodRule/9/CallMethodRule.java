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
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "92088cd2-8714-4cd2-b33c-1f116293d2ba");
        return useExactMatch;
    }

    /**
     * Set whether <code>MethodUtils.invokeExactMethod</code> should be used for the reflection.
     *
     * @param useExactMatch The <code>MethodUtils.invokeExactMethod</code> flag
     */
    public void setUseExactMatch(boolean useExactMatch) {
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "882df428-be61-4588-b93a-b56f3f8780bb");
        this.useExactMatch = useExactMatch;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setDigester(Digester digester) {
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "cf54863c-1dd1-4cf3-abbf-8965478aa9db");
        super.setDigester(digester);
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "8cc10316-bbd6-45ff-b303-27b8135a29e0");
        if (this.paramClassNames != null) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "a71ee39d-ead7-4174-9487-27ca1e0323aa");
            this.paramTypes = new Class<?>[paramClassNames.length];
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "f74649bf-ee0a-4310-b05c-963395de3bf6");
            for (int i = 0; i < this.paramClassNames.length; i++) {
                writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "9d48ae25-0471-4c40-ae87-c7712d54114b");
                try {
                    writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "2671a6e1-cc36-4038-bbae-c4275a88dcdf");
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
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "697fdfab-452d-4355-9041-222ac3c72414");
        if (paramCount > 0) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "6d378d86-8982-44e2-86f6-882963f96115");
            Object parameters[] = new Object[paramCount];
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "272a4a54-5be7-47a7-b342-cb304799a8c1");
            fill(parameters, null);
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "37606959-48e5-4b24-8cfd-531941883a20");
            getDigester().pushParams(parameters);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void body(String namespace, String name, String text) throws Exception {
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "17b25a9e-48e6-4996-8a26-90d7327109a5");
        if (paramCount == 0) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "3e4fa3dd-7af5-408c-a7c3-367b6865d951");
            this.bodyText = text.trim();
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void end(String namespace, String name) throws Exception {
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "5d814a28-5e81-4f8c-9222-31d862f0b4f4");
        Object[] parameters;
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "be71b241-230f-4935-8286-a67e984f72ba");
        if (paramCount > 0) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "e769ec4a-28b8-4bd5-85ce-334fe01e9e0e");
            parameters = getDigester().popParams();
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "7e4571d6-742e-4005-9e85-f55a07343368");
            if (getDigester().getLogger().isTraceEnabled()) {
                writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "20833d5d-e71e-436e-a8a1-2bb17e623cd4");
                for (int i = 0, size = parameters.length; i < size; i++) {
                    writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "bc5ace13-d151-44a7-9232-b55cd27a9cad");
                    getDigester().getLogger().trace(format("[CallMethodRule]{%s} parameters[%s]=%s", getDigester().getMatch(), i, parameters[i]));
                }
            }
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "e20ace24-c792-40d3-9387-d4953efb68f1");
            if (paramCount == 1 && parameters[0] == null) {
                writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "4957a94a-058d-4368-9e38-b47f35133a92");
                return;
            }
        } else if (paramTypes != null && paramTypes.length != 0) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "5242187f-bef7-4375-9005-a691655a7415");
            if (bodyText == null) {
                writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "f1fee689-4806-4749-a7c6-9e82334742e8");
                return;
            }
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "78fb70e5-5b28-49b2-ae9c-79bb92e95e5a");
            parameters = new Object[] { bodyText };
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "eabdb189-81bf-46a9-b830-945441fc40f3");
            if (paramTypes.length == 0) {
                writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "c59cb49a-4312-4a14-8b49-98c733791ac1");
                paramTypes = new Class[] { String.class };
            }
        } else {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "2942315d-54ca-4acf-a8e3-cb4059bf0859");
            parameters = new Object[0];
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "b9091130-5de2-4c61-a57e-d78ca8eff817");
            paramTypes = new Class<?>[0];
        }
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "7644f450-bf7b-4552-a4ed-018632365cb5");
        Object[] paramValues = new Object[paramTypes.length];
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "c513ec67-fee7-4598-b4df-c6ef91222e83");
        for (int i = 0; i < paramTypes.length; i++) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "33d490f5-5d53-497e-9161-9328eb8561fc");
            if (parameters[i] == null || (parameters[i] instanceof String && !String.class.isAssignableFrom(paramTypes[i]))) {
                writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "483394d2-c707-4a2d-840c-1c44ee5941f4");
                paramValues[i] = convert((String) parameters[i], paramTypes[i]);
            } else {
                writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "3e0cbf3e-52e9-4927-ba64-01492f7492a8");
                paramValues[i] = parameters[i];
            }
        }
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "88f63926-dbf5-4f6b-acdb-ba818ed5fe50");
        Object target;
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "614e8f03-1c53-4208-814f-0d1416826c4c");
        if (targetOffset >= 0) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "9cfe18e4-8fc9-458a-a798-d3621418b9b4");
            target = getDigester().peek(targetOffset);
        } else {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "b970ad29-4c97-4aab-9dd4-658bf9affc18");
            target = getDigester().peek(getDigester().getCount() + targetOffset);
        }
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "ead47dd6-e9dc-419b-8c2d-e39730aa2c57");
        if (target == null) {
        }
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "e0d57a63-1f79-4afe-958f-fb14396aedfa");
        if (getDigester().getLogger().isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "9f64bd3e-e23c-49b7-bc79-5722853832c6");
            Formatter formatter = new Formatter().format("[CallMethodRule]{%s} Call %s.%s(", getDigester().getMatch(), target.getClass().getName(), methodName);
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "8b85592e-edf5-4eb6-848e-11f451778a6b");
            for (int i = 0; i < paramValues.length; i++) {
                writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "336c55c1-b493-430d-be1b-3ae017cb6257");
                formatter.format("%s%s/%s", (i > 0 ? ", " : ""), paramValues[i], paramTypes[i].getName());
            }
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "740a557c-4fd8-498d-9cc7-7a8e6bc3464a");
            formatter.format(")");
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "4d9e0493-b2cd-447a-9f7c-788b0de4ae99");
            getDigester().getLogger().debug(formatter.toString());
        }
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "7dcc93b4-55cf-4124-b7bd-3bdeb9d601d2");
        Object result = null;
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "4c338c47-c87e-4a01-a5eb-13edeeff2164");
        if (useExactMatch) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "bbb6cb97-59ae-4f31-9093-7a376d990439");
            result = invokeExactMethod(target, methodName, paramValues, paramTypes);
        } else {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "31f0cc16-4c1f-4e01-bfa2-47a249b62f0d");
            result = invokeMethod(target, methodName, paramValues, paramTypes);
        }
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "6b76ac8e-b98b-44ab-b68b-2e2ecdbad63f");
        processMethodCallResult(result);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void finish() throws Exception {
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "a5c186fa-f98b-4ac0-8d8c-0a6a4d22243e");
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
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "95fc0c67-96e0-4f09-924c-2e6d85c429a9");
        Formatter formatter = new Formatter().format("CallMethodRule[methodName=%s, paramCount=%s, paramTypes={", methodName, paramCount);
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "85bdf9d0-e9a3-48f5-b343-92669eee2019");
        if (paramTypes != null) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "a71bcd66-a27c-4510-b11f-9c16bc83f046");
            for (int i = 0; i < paramTypes.length; i++) {
                writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "180b810a-9a58-44e3-a5e2-0e45f127c2d7");
                formatter.format("%s%s", (i > 0 ? ", " : ""), (paramTypes[i] != null ? paramTypes[i].getName() : "null"));
            }
        }
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "644c1d42-6897-4d9a-a028-2f43c12e4d75");
        formatter.format("}]");
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_9_10.coverage", "963fe111-e960-4e8d-942e-230038f6bf4a");
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
