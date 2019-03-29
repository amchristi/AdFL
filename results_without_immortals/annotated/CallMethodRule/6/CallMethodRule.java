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
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "e1d2f26c-c27f-4ae8-adb8-9103ca4b4ae5");
        return useExactMatch;
    }

    /**
     * Set whether <code>MethodUtils.invokeExactMethod</code> should be used for the reflection.
     *
     * @param useExactMatch The <code>MethodUtils.invokeExactMethod</code> flag
     */
    public void setUseExactMatch(boolean useExactMatch) {
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "d0d60d4f-c468-43d4-ab12-44238601f304");
        this.useExactMatch = useExactMatch;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setDigester(Digester digester) {
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "cb216bb8-a6df-43f8-849e-cd069cb8fd11");
        super.setDigester(digester);
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "d7e81e99-bac7-48e0-9ded-25777c6480c3");
        if (this.paramClassNames != null) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "f95bddc9-f8bb-42e8-92dd-2149f414e14e");
            this.paramTypes = new Class<?>[paramClassNames.length];
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "163cd96d-aae0-49c6-9861-05935bdc3717");
            for (int i = 0; i < this.paramClassNames.length; i++) {
                writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "ddcc7d16-16d4-4c87-8686-7da479a239d4");
                try {
                    writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "119b21cb-e656-4e48-bb68-e921fe14ddaf");
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
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "caf7fc83-1cab-420e-95b2-c26f41050795");
        if (paramCount > 0) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "cb8cc812-d598-46e1-be90-ee58f830e297");
            Object parameters[] = new Object[paramCount];
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "06189370-9b13-4711-9eb3-786f19448826");
            fill(parameters, null);
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "11469621-5959-45ca-8398-063cd21bfe8c");
            getDigester().pushParams(parameters);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void body(String namespace, String name, String text) throws Exception {
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "c6331546-a663-43a4-a2f7-e83b471dc317");
        if (paramCount == 0) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "4242a009-0695-47b9-a6f0-f50f2fbf86d9");
            this.bodyText = text.trim();
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void end(String namespace, String name) throws Exception {
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "43203253-91c2-480a-ab0e-606cc1f64848");
        Object[] parameters;
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "49ead247-f31e-426e-a535-e72646f4e754");
        if (paramCount > 0) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "e1492aef-846a-41a9-adce-d9908b53ad22");
            parameters = getDigester().popParams();
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "87f652c1-27af-4428-b570-85bfc3290a38");
            if (getDigester().getLogger().isTraceEnabled()) {
                writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "2ab6291c-038a-495d-81a1-962cca05ba52");
                for (int i = 0, size = parameters.length; i < size; i++) {
                    writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "4ea0974d-f783-453a-897e-87fbef58a5cf");
                    getDigester().getLogger().trace(format("[CallMethodRule]{%s} parameters[%s]=%s", getDigester().getMatch(), i, parameters[i]));
                }
            }
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "4a3eeafc-ae18-42a8-be36-acf3230f8cd0");
            if (paramCount == 1 && parameters[0] == null) {
                writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "86568bc0-0ac2-4f39-a36e-7d50cb981bb3");
                return;
            }
        } else if (paramTypes != null && paramTypes.length != 0) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "f98e2562-c278-40c8-9805-022d8fc93ac6");
            if (bodyText == null) {
                writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "cdb98a45-9a9b-441c-a7e3-5d5b4e77559b");
                return;
            }
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "b9a87134-8e99-4e0f-98bb-0a9b6a9a288a");
            parameters = new Object[] { bodyText };
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "21ed7eb2-9394-4ec5-a09f-59f2dacda69c");
            if (paramTypes.length == 0) {
                writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "8cf0a835-bad9-43d6-8aa4-7a2661124454");
                paramTypes = new Class[] { String.class };
            }
        } else {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "eb3e9ac7-3ec1-4a56-acb1-da94cf2be9a3");
            parameters = new Object[0];
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "06e54f09-0429-49ad-877d-b1a4e5d67f6b");
            paramTypes = new Class<?>[0];
        }
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "719cc9ec-e916-4747-9f4b-84363b837504");
        Object[] paramValues = new Object[paramTypes.length];
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "5db516a3-0d14-4b8f-b182-319c33325da7");
        for (int i = 0; i < paramTypes.length; i++) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "2fc4f738-624e-4dfe-807f-b620d51b7e19");
            if (parameters[i] == null || (parameters[i] instanceof String && !String.class.isAssignableFrom(paramTypes[i]))) {
                writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "dd2250f4-5559-402e-9386-e93b41488518");
                paramValues[i] = convert((String) parameters[i], paramTypes[i]);
            } else {
                writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "8546b158-6e9b-4286-9603-0f80532d4467");
                paramValues[i] = parameters[i];
            }
        }
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "7c332b9c-41a2-4bdb-972b-572d211c05f5");
        Object target;
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "4c22d737-6986-46c8-9c68-e1d3e79332eb");
        if (targetOffset >= 0) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "71575c9d-2de7-42cd-b9d7-9392448eafb5");
            target = getDigester().peek(targetOffset);
        } else {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "2f2031ef-5cbc-49e4-80f5-65e6a3e01ee2");
            target = getDigester().peek(getDigester().getCount() + targetOffset);
        }
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "495d3467-92e0-44ea-ae09-c3c4757ebf8b");
        if (target == null) {
        }
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "c1f7e64f-d20f-47b6-9ec5-360c9ec4f1d9");
        if (getDigester().getLogger().isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "db253f0b-9e94-4ce6-b8f3-bf7dbc43f1d6");
            Formatter formatter = new Formatter().format("[CallMethodRule]{%s} Call %s.%s(", getDigester().getMatch(), target.getClass().getName(), methodName);
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "5aa23989-51c0-4dd4-889e-b3082498a0ef");
            for (int i = 0; i < paramValues.length; i++) {
                writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "3d44d13f-bbcd-4e65-a221-7693cd98a2c2");
                formatter.format("%s%s/%s", (i > 0 ? ", " : ""), paramValues[i], paramTypes[i].getName());
            }
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "cbd63a70-0ce3-4d41-aaf6-4339e6769b80");
            formatter.format(")");
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "b2fc56c6-2bb1-4c36-bb55-3f9c942a66fb");
            getDigester().getLogger().debug(formatter.toString());
        }
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "26d7fb01-bbd9-4d7a-bfb9-d215d67f20b6");
        Object result = null;
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "a704768a-dfc8-4a05-88a3-d751616197c1");
        if (useExactMatch) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "3080437e-291a-49da-983b-f9485ed99368");
            result = invokeExactMethod(target, methodName, paramValues, paramTypes);
        } else {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "c347ce4e-22c5-45eb-b95a-61c3ad297610");
            result = invokeMethod(target, methodName, paramValues, paramTypes);
        }
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "9a38a519-d0fc-43a8-9e8b-aa286283d847");
        processMethodCallResult(result);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void finish() throws Exception {
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "499947cb-df96-4125-86b9-48b4907922a8");
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
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "b78c7698-2021-4a71-b758-4067727213a1");
        Formatter formatter = new Formatter().format("CallMethodRule[methodName=%s, paramCount=%s, paramTypes={", methodName, paramCount);
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "62dad62d-1d42-4a72-9379-c8506a042081");
        if (paramTypes != null) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "07447e3c-4c19-49cb-99b6-2b6181f05a17");
            for (int i = 0; i < paramTypes.length; i++) {
                writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "95be7024-22bd-4480-b7a9-fb52ab7f0d43");
                formatter.format("%s%s", (i > 0 ? ", " : ""), (paramTypes[i] != null ? paramTypes[i].getName() : "null"));
            }
        }
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "e05cda6d-fabc-4d24-893d-2af752034cdc");
        formatter.format("}]");
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_6_10.coverage", "61ad4954-c0e8-45bf-b6e2-ec0aec9a97ca");
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
