package test;
import org.openide.awt.ActionReference;
import org.openide.awt.ActionID;
import java.awt.event.*;
import org.openide.awt.*;
import java.awt.event.*;
@ActionID(category="Tools",id="my.action")@ActionRegistration(displayName="AAA", key="K") @ActionReference(path="Shortcuts")public class A implements ActionListener {
    public void actionPerformed(ActionEvent e) {}}

