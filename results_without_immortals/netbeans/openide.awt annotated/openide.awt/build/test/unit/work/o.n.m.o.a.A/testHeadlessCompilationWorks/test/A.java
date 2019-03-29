package test;
import org.openide.awt.ActionRegistration;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import java.awt.event.*;
@ActionID(category="Tools",id="my.action")@ActionRegistration(displayName="AAA") @ActionReference(path="Shortcuts", name="C-F2 D-A")public class A implements ActionListener {
    public void actionPerformed(ActionEvent e) {}}

