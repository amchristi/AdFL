package test;
import org.openide.awt.ActionRegistration;
import org.openide.awt.ActionReference;
import org.openide.awt.ActionID;
import org.openide.util.actions.Presenter;
import java.awt.event.*;
import java.util.List;
import javax.swing.*;
public class A {
    @ActionID(category="Tools",id="my.action")    @ActionRegistration(displayName="AAA", key="K", iconBase="does/not/exist.png")     @ActionReference(path="manka", position=11)    public class B implements ActionListener {
      public void actionPerformed(ActionEvent e) {}
    }
}

