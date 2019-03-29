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
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "7cb26818-4cb4-427c-8109-ac07aabd5a6c");
        return useExactMatch;
    }

    /**
     * Set whether <code>MethodUtils.invokeExactMethod</code> should be used for the reflection.
     *
     * @param useExactMatch The <code>MethodUtils.invokeExactMethod</code> flag
     */
    public void setUseExactMatch(boolean useExactMatch) {
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "e6bd5931-b3c2-4330-b220-806ba2ae7c1f");
        this.useExactMatch = useExactMatch;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setDigester(Digester digester) {
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "bb9aff01-1a43-450f-99c8-4061223c7732");
        super.setDigester(digester);
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "1a7547d5-9408-4fb4-a925-cedd14d265ab");
        if (this.paramClassNames != null) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "41a4207c-364c-4d1a-84ce-616d66cc8810");
            this.paramTypes = new Class<?>[paramClassNames.length];
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "b5661f47-22b0-4865-8942-081e6ec34d2b");
            for (int i = 0; i < this.paramClassNames.length; i++) {
                writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "8789a147-c4a7-4b0d-bdc5-e60bd0913bc6");
                try {
                    writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "6b3c5305-5033-463c-a52c-f386955d3253");
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
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "30dc6759-b5e8-4964-a3a2-c38cd15dd185");
        if (paramCount > 0) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "3483bcaa-760b-4d00-920b-f3e4bb706724");
            Object parameters[] = new Object[paramCount];
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "2703a8af-95ac-4e4d-bcd5-e85026a13af8");
            fill(parameters, null);
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "6df94b1e-5cb0-493f-a942-a0f5d4577b4d");
            getDigester().pushParams(parameters);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void body(String namespace, String name, String text) throws Exception {
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "168e25dd-80d5-465a-9993-c748a1383175");
        if (paramCount == 0) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "0a10966e-ff1b-47a9-99ee-df119f15d395");
            this.bodyText = text.trim();
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void end(String namespace, String name) throws Exception {
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "5eed73bf-431a-4885-b976-9ec3198ce68a");
        Object[] parameters;
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "eed11e9e-e0c6-46cf-a200-81e95cb44f08");
        if (paramCount > 0) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "07ecf403-6e85-498e-b01a-71e5e27a12ba");
            parameters = getDigester().popParams();
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "afcbaa8a-59bb-4abd-9f6a-991e31ab6ac2");
            if (getDigester().getLogger().isTraceEnabled()) {
                writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "308fe7b6-b952-40f3-a859-14055dad4ecc");
                for (int i = 0, size = parameters.length; i < size; i++) {
                    writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "c3e7508c-d719-498e-b49f-c3e56978c0a5");
                    getDigester().getLogger().trace(format("[CallMethodRule]{%s} parameters[%s]=%s", getDigester().getMatch(), i, parameters[i]));
                }
            }
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "e94b06e8-eb21-43cd-a87a-c10ec3a35222");
            if (paramCount == 1 && parameters[0] == null) {
                writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "e4acfc7e-1410-4274-897e-d4762e65a781");
                return;
            }
        } else if (paramTypes != null && paramTypes.length != 0) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "1419ef96-ca28-49ce-8454-409e3d0a7b4b");
            if (bodyText == null) {
                writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "0046a769-26b5-4d06-a0c8-eee9ddaed782");
                return;
            }
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "19052126-a53f-4246-b648-44504c9a06f3");
            parameters = new Object[] { bodyText };
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "0042e6d9-50a9-48a7-aa19-65df9a9cbb65");
            if (paramTypes.length == 0) {
                writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "06b7edf5-4045-406b-898f-ba169d5a0c2d");
                paramTypes = new Class[] { String.class };
            }
        } else {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "931170da-0202-480e-bf2f-b365e20795c7");
            parameters = new Object[0];
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "89a5112a-1a01-4e7c-a1ba-ab4520c7e0cf");
            paramTypes = new Class<?>[0];
        }
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "fe482ef0-cc45-4cb1-84c4-6d070dc2fff7");
        Object[] paramValues = new Object[paramTypes.length];
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "5aa820b0-c9e0-4aeb-9467-4f2779e4f24e");
        for (int i = 0; i < paramTypes.length; i++) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "93b9ef53-ea2e-48df-9c4a-c3347ab1d91a");
            if (parameters[i] == null || (parameters[i] instanceof String && !String.class.isAssignableFrom(paramTypes[i]))) {
                writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "1bee1d0b-9eff-447b-8ec0-bc2f82a2197f");
                paramValues[i] = convert((String) parameters[i], paramTypes[i]);
            } else {
                writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "beffdfb0-f5c2-47d7-b1b3-7838df7eb51b");
                paramValues[i] = parameters[i];
            }
        }
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "037a5ef7-65d4-44a5-8093-7f1da3d7371d");
        Object target;
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "b6b3c419-9032-458b-b41b-fef1e962a242");
        if (targetOffset >= 0) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "2cd1b946-cc3d-4bc8-9470-5db84f9a57b3");
            target = getDigester().peek(targetOffset);
        } else {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "d0361266-ea08-434c-8c7a-15dc995aaceb");
            target = getDigester().peek(getDigester().getCount() + targetOffset);
        }
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "d17c0f6d-8447-4316-87f9-4bd9044e7a7e");
        if (target == null) {
        }
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "331bf624-7b42-4cbc-8533-0ecbeeb30c21");
        if (getDigester().getLogger().isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "bcf755cd-24ce-4d62-a5b4-e8c16debcaa6");
            Formatter formatter = new Formatter().format("[CallMethodRule]{%s} Call %s.%s(", getDigester().getMatch(), target.getClass().getName(), methodName);
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "baf9aecb-504d-4114-bb62-6f4b26f171df");
            for (int i = 0; i < paramValues.length; i++) {
                writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "3f749a45-c02a-46ce-8df7-5cd7940c73bb");
                formatter.format("%s%s/%s", (i > 0 ? ", " : ""), paramValues[i], paramTypes[i].getName());
            }
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "1832c2a7-4afc-4be2-a1ca-c93008c9146f");
            formatter.format(")");
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "619ff3d6-0ffc-497c-9e48-9cc1c0426f9f");
            getDigester().getLogger().debug(formatter.toString());
        }
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "0dfac9f2-3b7b-4e4e-8fa1-6bb17e6896a0");
        Object result = null;
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "f191b563-0663-478e-b76d-0dad556ff2af");
        if (useExactMatch) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "201f45d3-fcd9-4fcb-880c-9c0c819b8f61");
            result = invokeExactMethod(target, methodName, paramValues, paramTypes);
        } else {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "442ba30d-b90c-4822-94ca-69ca7125a151");
            result = invokeMethod(target, methodName, paramValues, paramTypes);
        }
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "8c884ec5-9616-45e0-a2eb-9188ad667e8f");
        processMethodCallResult(result);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void finish() throws Exception {
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "d9edd775-36d2-4bc1-b48d-2454bfa6304b");
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
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "646a4099-f501-4b77-8528-61e2319726ff");
        Formatter formatter = new Formatter().format("CallMethodRule[methodName=%s, paramCount=%s, paramTypes={", methodName, paramCount);
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "e4c8637e-5df4-4e78-90dd-997226a94cb6");
        if (paramTypes != null) {
            writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "8bc5ec16-eb81-415d-bc7c-4c851de89a45");
            for (int i = 0; i < paramTypes.length; i++) {
                writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "ba1727d1-4a7d-46da-bbc0-963daa91a886");
                formatter.format("%s%s", (i > 0 ? ", " : ""), (paramTypes[i] != null ? paramTypes[i].getName() : "null"));
            }
        }
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "021be16e-06c2-4c91-a886-35c9cb634d14");
        formatter.format("}]");
        writeline("/home/ubuntu/results/coverage/CallMethodRule/CallMethodRule_7_10.coverage", "24de4944-d922-4121-9cae-285557232d52");
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
