/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2010 Oracle and/or its affiliates. All rights reserved.
 *
 * Oracle and Java are registered trademarks of Oracle and/or its affiliates.
 * Other names may be trademarks of their respective owners.
 *
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common
 * Development and Distribution License("CDDL") (collectively, the
 * "License"). You may not use this file except in compliance with the
 * License. You can obtain a copy of the License at
 * http://www.netbeans.org/cddl-gplv2.html
 * or nbbuild/licenses/CDDL-GPL-2-CP. See the License for the
 * specific language governing permissions and limitations under the
 * License.  When distributing the software, include this License Header
 * Notice in each file and include the License file at
 * nbbuild/licenses/CDDL-GPL-2-CP.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the GPL Version 2 section of the License file that
 * accompanied this code. If applicable, add the following below the
 * License Header, with the fields enclosed by brackets [] replaced by
 * your own identifying information:
 * "Portions Copyrighted [year] [name of copyright owner]"
 *
 * Contributor(s):
 *
 * The Original Software is NetBeans. The Initial Developer of the Original
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2006 Sun
 * Microsystems, Inc. All Rights Reserved.
 *
 * If you wish your version of this file to be governed by only the CDDL
 * or only the GPL Version 2, indicate your decision by adding
 * "[Contributor] elects to include this software in this distribution
 * under the [CDDL or GPL Version 2] license." If you do not indicate a
 * single choice of license, a recipient has the option to distribute
 * your version of this file under either the CDDL, the GPL Version 2 or
 * to extend the choice of license to its licensees as provided above.
 * However, if you add GPL Version 2 code and therefore, elected the GPL
 * Version 2 license, then the option applies only if the new code is
 * made subject to such option by the copyright holder.
 */
package org.openide.awt;

import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.Vector;
import java.io.*;

/**
 * A class that produces a Spin Button.
 *
 * <P>
 * <TABLE BORDER COLS=3 WIDTH=100%>
 * <TR><TH WIDTH=15%>Property<TH WIDTH=15%>Property Type<TH>Description
 * <TR><TD> Orientation <TD> boolean <TD> Orientation of SpinButton (Left-right or Up-down)
 * <TR><TD> Minimum     <TD> int     <TD> Minimum value.
 * <TR><TD> Maximum     <TD> int     <TD> Maximum value.
 * <TR><TD> Step        <TD> int     <TD> Step.
 * <TR><TD> Value       <TD> int     <TD> Current value.
 * <TR><TD> RepeatDelay <TD> int     <TD> Delay time after press SpinButton [ms]
 * <TR><TD> RepeatRate  <TD> int     <TD> Repeat rate while holding PressButton [ms]
 * </TABLE>
 *
 * @deprecated Obsoleted by <code>javax.swing.JSpinner</code> in JDK 1.4
 * @version 3.06, November 17, 1997
 * @author  Petr Hamernik, Jan Jancura
 */
@Deprecated
public class SpinButton extends Canvas {

    /**
     * generated Serialized Version UID
     */
    static final long serialVersionUID = -3525959415481788776L;

    /**
     * Default orientation of SpinButton. Currently false (UP-DOWN).
     * @see #DEFAULT_ORIENTATION
     * @see #setOrientation
     * @see #getOrientation
     */
    public static final boolean DEFAULT_ORIENTATION = false;

    /**
     * Default minimum. Currently 0.
     * @see #minimum
     * @see #setMinimum
     * @see #getMinimum
     */
    public static final int DEFAULT_MINIMUM = 0;

    /**
     * Default maximum. Currently 100.
     * @see #maximum
     * @see #setMaximum
     * @see #getMaximum
     */
    public static final int DEFAULT_MAXIMUM = 100;

    /**
     * Default step. Currently 1.
     * @see #step
     * @see #setStep
     * @see #getStep
     */
    public static final int DEFAULT_STEP = 1;

    /**
     * Default value of repeatDelay. Currently 300 ms.
     * @see #setDelay
     * @see #getDelay
     * @see #repeatDelay
     */
    public static final int DEFAULT_REPEAT_DELAY = 300;

    /**
     * Default value of repeatRate. Currently 70 ms.
     * @see #setRate
     * @see #getRate
     * @see #repeatRate
     */
    public static final int DEFAULT_REPEAT_RATE = 70;

    /**
     * Helper constant
     */
    private static final boolean SPIN_UP = true;

    /**
     * Helper constant
     */
    private static final boolean SPIN_DOWN = false;

    /**
     * Current orientation of SpinButton.
     * True = LEFT-RIGHT, False = UP-DOWN
     * @see #DEFAULT_ORIENTATION
     * @see #setOrientation
     * @see #getOrientation
     */
    protected boolean orientation = DEFAULT_ORIENTATION;

    /**
     * Current orientation of arrows of SpinButton.
     * True = LEFT-RIGHT, False = UP-DOWN
     * @see #DEFAULT_ORIENTATION
     * @see #setOrientation
     * @see #getOrientation
     */
    protected boolean arrowsOrientation = DEFAULT_ORIENTATION;

    /**
     * Minimum of the range of the SpinButton.
     * @see #DEFAULT_MINIMUM
     * @see #setMinimum
     * @see #getMinimum
     */
    protected int minimum = DEFAULT_MINIMUM;

    /**
     * Maximum of the range of the SpinButton.
     * @see #DEFAULT_MAXIMUM
     * @see #setMaximum
     * @see #getMaximum
     */
    protected int maximum = DEFAULT_MAXIMUM;

    /**
     * Step of the SpinButton.
     * @see #DEFAULT_STEP
     * @see #setStep
     * @see #getStep
     */
    protected int step = DEFAULT_STEP;

    /**
     * Value of the SpinButton. Default 0.
     * @see #setValue
     * @see #getValue
     */
    protected int value = 0;

    /**
     * Adjusts the amount of time that elapses before a increment
     * (or decrement) begins repeating when you hold down a mouse
     * button. [ms]
     * @see #setDelay
     * @see #getDelay
     * @see #DEFAULT_REPEAT_DELAY
     */
    protected int repeatDelay = DEFAULT_REPEAT_DELAY;

    /**
     * Adjusts the speed at which a increment (or decrement)
     * repeats when you hold down a mouse button. [ms]
     * @see #setRate
     * @see #getRate
     * @see #DEFAULT_REPEAT_RATE
     */
    protected int repeatRate = DEFAULT_REPEAT_RATE;

    /**
     * Spin repeat thread. When the SpinButton is holded this thread
     * runs and regulary sends the events to SpinButton.
     */
    protected RepeatThread rt = null;

    /**
     * Flag if the SpinRepeatThread is currently running.
     */
    protected boolean running = false;

    /**
     * Flag if the SpinRepeatThread is currently running.
     */
    protected boolean repeating = true;

    /**
     * Current direction of the run of the SpinRepeatThread.
     */
    protected boolean runningDir = SPIN_DOWN;

    protected boolean boundsIgnored = false;

    /**
     * Property change listeners storage
     */
    private PropertyChangeSupport valueSupport = new PropertyChangeSupport(this);

    /**
     * SpinButton change listeners storage
     */
    private Vector<SpinButtonListener> spinButtonListeners = new Vector<SpinButtonListener>(3, 3);

    /**
     * Create new SpinButton.
     */
    public SpinButton() {
        setBackground(SystemColor.control);
        setForeground(SystemColor.controlText);
        addMouseListener(new MouseAdapter() {

            @Override
            public void mousePressed(MouseEvent evt) {
                Dimension d = getSize();
                boolean newDir = SPIN_UP;
                if (orientation) {
                    if (evt.getX() <= ((d.width - 1) / 2)) {
                        newDir = SPIN_DOWN;
                    } else {
                        newDir = SPIN_UP;
                    }
                } else {
                    if (evt.getY() <= ((d.height - 1) / 2)) {
                        newDir = SPIN_UP;
                    } else {
                        newDir = SPIN_DOWN;
                    }
                }
                if ((((newDir == SPIN_UP) && (value >= maximum)) || ((newDir == SPIN_DOWN) && (value <= minimum))) && !boundsIgnored) {
                    return;
                }
                switchRun(newDir);
                repaint();
            }

            @Override
            public void mouseReleased(MouseEvent evt) {
                boolean r = running;
                switchStop();
                if (r) {
                    repaint();
                }
            }
        });
    }

    /**
     * Setter method for foreground color.
     *
     * @param color New foreground color.
     */
    @Override
    public void setForeground(Color color) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4ef0a266-a877-4318-8241-37826f7f87e1");
        super.setForeground(color);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "84d40b21-5b62-4492-b0ea-56b3d003c661");
        repaint();
    }

    /**
     * Sets the new orientation.
     * @param aDir new value of orientation.
     * @see #orientation
     * @see #DEFAULT_ORIENTATION
     * @see #getOrientation
     */
    public void setOrientation(boolean aDir) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c2fe962a-5dbc-49d1-bb03-a5da49ee5373");
        orientation = aDir;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "004da81d-1cca-4b5f-ab81-b4f78ec9eaca");
        switchStop();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "178797b7-b3b5-422a-b0ef-a976b481be40");
        repaint();
    }

    /**
     * Sets the new orientation of arows.
     * @param aDir new value of orientation of arows.
     * @see #orientation
     * @see #DEFAULT_ORIENTATION
     * @see #getOrientation
     */
    public void setArrowsOrientation(boolean aDir) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bafdf0c0-5f43-4f66-a07a-cf9f60702371");
        arrowsOrientation = aDir;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1155731e-5067-4a76-b778-201a2e52b43c");
        switchStop();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a1492b2a-0820-409b-8d70-7c7afc5d195b");
        repaint();
    }

    /**
     * Gets the current orientation of SpinButton.
     * @return value of orientation.
     * @see #orientation
     * @see #DEFAULT_ORIENTATION
     * @see #setOrientation
     */
    public boolean getOrientation() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9852b178-4ecf-4d7c-ad8d-6e339ad97152");
        return orientation;
    }

    /**
     * Gets the current orientation of Arrows of SpinButton.
     * @return value of orientation of Arrows.
     * @see #orientation
     * @see #DEFAULT_ORIENTATION
     * @see #setOrientation
     */
    public boolean getArrowsOrientation() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a0e58a9e-46a6-413e-bf07-8cc26efd6928");
        return arrowsOrientation;
    }

    /**
     * Sets a minimum of the range of the SpinButton. If value
     * or maximum fall out of acceptable values they are adjusted.
     * @param aMin New minimum.
     * @see #getMinimum
     */
    public void setMinimum(int aMin) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8b14e8da-222f-47c2-a84d-c980a5537764");
        minimum = aMin;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3e6640ed-71cc-4d69-b710-e59f4466db30");
        if (maximum < minimum) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6c173652-1f78-4e1c-90c8-f3d641a4d46e");
            maximum = minimum;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "80012697-59b1-43ed-9239-c383fe6261c2");
        if (value < minimum) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "69f63ebf-b482-4cf3-a2d3-4adf2b3a0392");
            setValue(value);
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fe661fc8-60b8-4325-93a3-c7460863cb00");
        switchStop();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "02e3992a-381a-44ae-a227-09f1ad10ed5f");
        repaint();
    }

    /**
     * Gets the current minimum of the range of SpinButton.
     * @return Minimum.
     * @see #setMinimum
     */
    public int getMinimum() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c4396818-2c73-48f0-bcc2-4c1a670b0d31");
        return minimum;
    }

    /**
     * Sets a maximum of the range of the SpinButton. If value
     * or minimum fall out of acceptable values they are adjusted.
     * @param aMax New maximum.
     * @see #getMinimum
     */
    public void setMaximum(int aMax) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8c4f59b4-f819-4df3-992d-5edc86d75523");
        maximum = aMax;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "590739d1-0e69-4bb0-a2cd-f1165ea08a4a");
        if (maximum < minimum) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "801d1e4c-7089-429b-bc73-613570e3676d");
            minimum = maximum;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "82af9683-0913-4a64-b45f-9e65e8dad6cb");
        if (value > maximum) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f43b62dd-7c5c-458f-a473-ecbad9f6fccf");
            setValue(value);
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0d6077c3-0397-425e-8823-0831b99b9a08");
        switchStop();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9d56f6b1-1c5b-4f27-b1d4-3fad19f82bba");
        repaint();
    }

    /**
     * Gets the current maximum of the range of SpinButton.
     * @return Maximum.
     * @see #setMaximum
     */
    public int getMaximum() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e17f1a68-397d-40b3-8053-bf3bfd4f0d92");
        return maximum;
    }

    /**
     * Sets a new value of the SpinButton. If value is outside
     * the ranges it is set to nearest acceptable value.
     * @param aValue New value.
     * @see #getValue
     */
    public void setValue(int aValue) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2a4efbfd-9b89-4660-880b-efd2851c4ae9");
        int oldValue = value;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "feb0c944-4c90-43ca-b479-d1b0b3284ad1");
        value = aValue;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "751a5f0e-ecb8-4be6-8c9c-1a7489593e77");
        if (!boundsIgnored) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d7253fc6-a14f-4b0a-940f-f851f8a930e6");
            if (value < minimum) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2a9aec16-150a-4e06-baa6-1d2dbb208882");
                value = minimum;
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f6beefd8-f3f2-418b-b3c6-7b6624613eec");
            if (value > maximum) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d539b883-b7bb-4622-85c3-bae98f7b5361");
                value = maximum;
            }
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "426e5be4-1a85-497b-bfaa-83e308762727");
        if (value != oldValue) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "38a47930-9d08-4034-9c37-a1b7c2e835a1");
            // NOI18N
            valueSupport.firePropertyChange("value", new Integer(oldValue), new Integer(value));
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4176015f-6b18-4f09-95c3-5381f49e1c74");
        if ((getValue() == minimum) || (getValue() == maximum) || (oldValue == minimum) || (oldValue == maximum)) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1e1ad090-c138-440a-b05b-3a07f2f02883");
            repaint();
        }
    }

    /**
     * Gets the current value of the SpinButton.
     * @return Value.
     * @see #setValue
     */
    public int getValue() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2144629b-0c2d-4032-8d87-2252946ee78b");
        return value;
    }

    /**
     * Sets a new step of the SpinButton.
     * @param aStep New step.
     * @see #getStep
     */
    public void setStep(int aStep) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "043b2d5e-9290-42eb-9634-b27a9a1edf46");
        step = aStep;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6001a1a0-2672-4c2b-9ca6-77fdf2148385");
        switchStop();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ed6fdf13-82d4-4ff2-8d4a-3a7e226b2de9");
        repaint();
    }

    /**
     * Gets the current step of the SpinButton.
     * @return Step.
     * @see #setStep
     */
    public int getStep() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "82b900ce-3ff9-4122-8b9e-78c7014eaff6");
        return step;
    }

    /**
     * Sets new value of repeatDelay variable.
     * @param aDelay New delay.
     * @see #repeatDelay
     * @see #getDelay
     */
    public void setDelay(int aDelay) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6b3136fb-abab-413d-9b3f-2f938336e8b5");
        repeatDelay = aDelay;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6d4c2815-0358-4838-b2c6-d1040927feaa");
        switchStop();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a2b207d9-30e1-4449-85c6-f5b9bd58c19b");
        repaint();
    }

    /**
     * Gets the current value of repeatDelay variable.
     * @return Delay.
     * @see #repeatDelay
     * @see #setDelay
     */
    public int getDelay() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7a5ab9bd-73c3-455e-9be1-7b6a705de304");
        return repeatDelay;
    }

    /**
     * Sets new value of repeatRate variable.
     * @param aRate New rate.
     * @see #repeatRate
     * @see #getRate
     */
    public void setRate(int aRate) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f4f05ce1-0e88-4c53-9e90-14d372d7dfb5");
        repeatRate = aRate;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f2ff338e-0a83-415c-9440-5c7117a3f64d");
        switchStop();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1a98482a-bc98-4912-9fcd-ec41a132867e");
        repaint();
    }

    /**
     * Gets the current value of rate variable.
     * @return Rate.
     * @see #repeatRate
     * @see #setRate
     */
    public int getRate() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "595746dc-8876-49b0-8f51-400a72b0ada2");
        return repeatRate;
    }

    public boolean isBoundsIgnored() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0df9c329-381e-453a-86b7-cb24b3daa9d5");
        return boundsIgnored;
    }

    public void setBoundsIgnored(boolean ignored) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "67cf6fd0-24e7-4ac6-bb79-8ba5ab9f1052");
        boundsIgnored = ignored;
    }

    public boolean isRepeating() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b58d1351-7c3b-4123-8913-d13cd74cadda");
        return repeating;
    }

    public void setRepeating(boolean aRepeating) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4d7d4854-3c99-446a-b2b5-c5f37434a581");
        repeating = aRepeating;
    }

    @Override
    public void paint(Graphics g) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "146207f8-1315-4e00-bbc3-1299e8de6910");
        Dimension d = getSize();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "23bf8753-c736-41db-9f67-5531deff3b29");
        int left = 0;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "dbbaeeec-2d52-4e0d-b790-671b7cf2ab1d");
        int top = 0;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a9aa01c8-5996-4e5c-aea6-b5212a20471c");
        int w = d.width - 1;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "45d2da49-c12e-47d1-b7b6-e072b5ab96d9");
        int h = d.height - 1;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "38dd07b5-08ce-4578-99d9-deb8d735f99c");
        g.setColor(getBackground());
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "84208511-fab8-413b-ae71-ba012367a84a");
        g.fillRect(left, top, w, h);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "23e8c156-bb27-4cff-859e-51fa0207df32");
        if (orientation) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4a1ddd75-d1e7-449f-ab02-abe641896299");
            w = w / 2;
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9e0efee1-c882-4b60-a214-7bdb25c60c6c");
            paintBorder(g, left, top, w, h, running && (runningDir == SPIN_DOWN), SPIN_DOWN);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6de68cfb-5706-4075-9a2f-dd40142687f7");
            left += (w + 1);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ce64f14b-ece4-41f7-a38d-4738669bd2ee");
            w = d.width - 1 - left;
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4b702178-ae78-441c-a9f5-7b464c617f1f");
            paintBorder(g, left, top, w, h, running && (runningDir == SPIN_UP), SPIN_UP);
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6e677b7e-3b11-4fa6-8370-da81e317c7e9");
            h = h / 2;
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6dfb5310-298a-4484-9ec9-bdfd6fe267e1");
            paintBorder(g, left, top, w, h, running && (runningDir == SPIN_UP), SPIN_UP);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ed0171ca-c98a-4eb4-82e6-103ebfb389b2");
            top += (h + 1);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e6472552-bfb5-4a3c-b8cc-ee56f2fcf06c");
            h = d.height - 1 - top;
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "538b9d6e-baa4-4c61-a865-d41b64cd480b");
            paintBorder(g, left, top, w, h, running && (runningDir == SPIN_DOWN), SPIN_DOWN);
        }
    }

    private void paintBorder(Graphics g, int x, int y, int w, int h, boolean isDown, boolean aDir) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4819ed04-2d0f-4f06-8978-c6066a53eb77");
        g.setColor(Color.black);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4f92d31f-3e29-4e4c-adb8-f056f0ae0225");
        if (!isDown) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a05bea40-f28d-40cc-846d-d5599eb1cdfa");
            g.drawLine(x, y + h, x + w, y + h);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6dfcd044-bfe1-4c3f-8621-72518ab6957f");
            g.drawLine(x + w, y, x + w, y + h);
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5007ea26-cd70-45be-a4ce-90eddce155b7");
            g.drawLine(x, y, x + w, y);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f39e62b3-d42b-42f2-9192-6d2841df132f");
            g.drawLine(x, y, x, y + h);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1da058f9-eeb8-47e0-a198-1d01cc3be601");
            x++;
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ac975631-4038-4922-9123-81d670990d43");
            y++;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ee502ff4-b39b-4004-8f0a-bc6f302c0278");
        w--;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "30cb41be-cac1-4817-aca8-8694a8215f9b");
        h--;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "14d4ebce-acbe-4bf6-a734-1652c22e9619");
        g.setColor(SystemColor.controlHighlight);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f0c363fc-ef77-485f-9789-197ab4805164");
        g.draw3DRect(x, y, w, h, !isDown);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5ebf1e2d-706b-44ba-a322-010cfc03a4f4");
        paintArrow(g, x, y, w, h, aDir);
    }

    private void paintArrow(Graphics g, int x, int y, int w, int h, boolean aDir) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4157dd01-d599-42fa-b0b1-6ae440aecfcd");
        if ((w <= 0) || (h <= 0)) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b3561bea-0b68-447c-a8c2-d9a449b923d8");
            return;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "eee3d9d4-5d84-4b43-b981-25d4cc93c037");
        int wd = w / 4;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "91156ee7-3d30-4ab1-8321-fadb63a3fa82");
        int hd = h / 4;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c22cfba4-0d30-4a8d-9867-9e17e0b5caab");
        int[] xP = new int[3];
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3bacbdcd-5ec0-4bb2-8d2c-0ec620a446bc");
        int[] yP = new int[3];
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a396d5aa-ee52-4920-9c95-43b4490fca84");
        if (arrowsOrientation) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b8e4870e-02c2-4c1e-9f9d-8432be63bac9");
            if (aDir == SPIN_UP) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "41766e09-77f7-4bbc-b66d-a2e0c0ddc5a5");
                xP[0] = x + wd;
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "214fe7d8-14ec-4d7c-9cd3-f00357143005");
                xP[2] = (x + w) - wd;
            } else {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d85a0ef5-9390-43c7-9908-cd41845caabf");
                xP[0] = (x + w) - wd;
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b8766f7b-1f70-435f-b7d2-12bf46a8d6f4");
                xP[2] = x + wd;
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cc0068e1-747f-4083-ad89-5f2a3500aa56");
            xP[1] = xP[0];
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "49f79587-eeca-473e-b035-90942a364a59");
            yP[0] = y + hd;
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4dc583a8-3a27-4981-872c-516b1bfe98c2");
            yP[1] = (y + h) - hd;
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "59f1836f-8298-4b33-a2f8-f90f5d8858ce");
            yP[2] = y + (h / 2);
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3be73edd-3d45-49d7-8671-c65734fa5a8f");
            if (aDir == SPIN_UP) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a061f5a6-86f8-485c-8a27-d673ecdbd76c");
                yP[0] = (y + h) - hd;
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2f29ba8f-4e06-46fb-a404-6dbdcf55ba57");
                yP[2] = y + hd;
            } else {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "36c968ca-d087-45d7-9fde-a59c19025493");
                yP[0] = y + hd;
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "346241ad-403c-403f-b4f0-a3f683112487");
                yP[2] = (y + h) - hd;
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "173f6113-90dd-4314-8195-0e1a00d20e74");
            yP[1] = yP[0];
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3213f0f4-72b8-44d3-bd93-93745e48ba00");
            xP[0] = x + wd;
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f32b5458-976a-4ced-b062-b7b7788002b8");
            xP[1] = (x + w) - wd;
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "44e4fe39-8edf-49ce-af86-54a9a5f0bcf2");
            xP[2] = x + (w / 2);
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ad291605-7130-4870-b39a-39de7783b1f5");
        if ((((aDir == SPIN_UP) && (value >= maximum)) || ((aDir == SPIN_DOWN) && (value <= minimum))) && !boundsIgnored) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d811aace-2a07-46c8-95e4-28d4afea480c");
            Color fg = getForeground();
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4d88174c-8e97-4c15-8f23-d28f63c5b1d9");
            Color bg = getBackground();
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "97f5c41f-ab31-489e-9195-0865026481b5");
            g.setColor(new Color((fg.getRed() + (2 * bg.getRed())) / 3, (fg.getGreen() + (2 * bg.getGreen())) / 3, (fg.getBlue() + (2 * bg.getBlue())) / 3));
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "97f0d77f-8898-4520-b6c1-be65d1c75720");
            g.setColor(getForeground());
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "20852c96-9860-4776-9ca5-4b409ec2e80d");
        g.fillPolygon(xP, yP, 3);
    }

    protected synchronized void switchRun(boolean aDirect) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0bacfcb8-fb38-4543-b532-6d8bf1012e42");
        if (running) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b4f52470-d613-4e66-8d36-fbaef7aabd8e");
            rt.finish = true;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9d171a04-4887-4b04-98a0-10e1183727ff");
        rt = new RepeatThread();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c1489487-a2f4-4c02-aba0-e84631b272a2");
        rt.start();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "31a40852-81f4-43b8-9294-6c511eee31eb");
        runningDir = aDirect;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c82a6577-188b-41a8-9a99-bf296d299d69");
        running = true;
    }

    public synchronized void switchStop() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2e0f43ca-a54f-4eb9-a3be-0a93e39e703e");
        if (rt == null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "719cc56e-23c6-40c9-8909-33754533672e");
            return;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f7432d1e-f214-49f3-a0f9-a5780641a0ad");
        rt.finish = true;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "963f2895-12eb-407d-bcca-13233f1f97a8");
        running = false;
    }

    @Override
    public Dimension getMinimumSize() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2875d329-fe13-48b6-a938-130c80773f9f");
        return countSize();
    }

    @Override
    public Dimension getPreferredSize() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0e0e7977-564e-4825-b6af-2829bf1c568e");
        return countSize();
    }

    private Dimension countSize() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "702a2f9b-3989-4d0b-9520-6706eb117ecb");
        int x = 11;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6cf331ca-8bab-4249-85d3-2fea26d01db9");
        int y = x;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "92344b5f-9b3c-4538-995d-2e0186812bf9");
        if (orientation) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f05db507-d8de-42d6-9c81-5dd8d4dbcc8a");
            x = x + x;
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8b54f038-2cef-482b-b557-ec0ec7ddc3b5");
            y = y + y;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "64409640-e178-4776-805b-8a71f4e68fa8");
        return new Dimension(x, y);
    }

    @Override
    public void addPropertyChangeListener(PropertyChangeListener l) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7ee62a08-a45e-4e0f-b520-c1e919187431");
        valueSupport.addPropertyChangeListener(l);
    }

    @Override
    public void removePropertyChangeListener(PropertyChangeListener l) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "47ed575c-11cb-419c-bcba-0d5317915ffb");
        valueSupport.removePropertyChangeListener(l);
    }

    public void addSpinButtonListener(SpinButtonListener spinButtonListener) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a65cdce9-a5a7-4809-8741-808477ff484c");
        spinButtonListeners.addElement(spinButtonListener);
    }

    public void removeSpinButtonListener(SpinButtonListener spinButtonListener) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "84ab544e-be48-4e70-8de6-db79481876fc");
        spinButtonListeners.removeElement(spinButtonListener);
    }

    public void notifySpinButtonListenersAboutUpMove() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cfc79321-6862-43bc-85d5-231b3ea3afaf");
        int i;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1c9e7ebf-f8fc-4fe6-b051-5dff707902ca");
        int k = spinButtonListeners.size();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7ec0648c-6116-4ebd-b4b0-fec8902f0b32");
        for (i = 0; i < k; i++) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4b0a995c-fdd3-42ef-93bf-33fb507ec6a6");
            spinButtonListeners.elementAt(i).moveUp();
        }
    }

    public void notifySpinButtonListenersAboutDownMove() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "00c8f5cc-fc5b-4360-8996-5749e3f725df");
        int i;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f884530b-2157-42cd-9d4d-5a3e34570b0b");
        int k = spinButtonListeners.size();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "17ae27cd-9900-4005-82bb-a39c8e03e322");
        for (i = 0; i < k; i++) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "82d13d12-5bc3-46fd-9638-a93ef9831efc");
            spinButtonListeners.elementAt(i).moveDown();
        }
    }

    protected void repeatThreadNotify() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5b151735-4541-4bee-93a2-6128305742f1");
        int old_val = getValue();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "254761da-3d32-4bd8-b360-1b30ee86a2b9");
        if (runningDir) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "36716b2d-7a78-47c8-9840-df5240b94f82");
            setValue(getValue() + step);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d2e18318-2c1a-448e-bec3-55c36b48f996");
            if (value != old_val) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ead70b10-d081-4e2f-b541-78f5cae3dd87");
                notifySpinButtonListenersAboutUpMove();
            }
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9e8c06d0-4a21-441e-b305-8aab07e1cbea");
            setValue(getValue() - step);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2dc547bf-d5f7-4696-a8ff-3b8f3cdd076f");
            if (value != old_val) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "663302d9-929e-48f9-85b1-36d255bb5fad");
                notifySpinButtonListenersAboutDownMove();
            }
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d3e7fea1-3505-4d7c-a42d-b4be26081607");
        if ((getValue() == old_val) && !boundsIgnored) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5ef17271-0f62-4bfb-8a39-415469bd978b");
            switchStop();
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f4659b32-20f0-4eed-a443-d8ed4a6e8fad");
            repaint();
        }
    }

    /**
     * @deprecated Made visible only because it was (by mistake)
     * visible from public signatures. No need to use it.
     * @since 7.0
     */
    @Deprecated
    protected final class RepeatThread extends Thread {

        boolean finish = false;

        RepeatThread() {
            finish = false;
        }

        @Override
        public void run() {
            repeatThreadNotify();
            try {
                sleep(repeatDelay);
            } catch (InterruptedException e) {
            }
            if (!repeating) {
                return;
            }
            while (true) {
                if (finish) {
                    break;
                }
                repeatThreadNotify();
                if (finish) {
                    break;
                }
                try {
                    sleep(repeatRate);
                } catch (InterruptedException e) {
                }
            }
        }
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