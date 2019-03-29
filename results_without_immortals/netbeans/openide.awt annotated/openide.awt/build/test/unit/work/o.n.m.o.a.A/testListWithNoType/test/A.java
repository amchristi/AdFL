package test;
import org.openide.awt.ActionRegistration;
import org.openide.awt.ActionID;
import org.openide.util.actions.Presenter;
import java.awt.event.*;
import java.util.List;
import javax.swing.*;
@ActionID(category="Tools",id="my.action")@ActionRegistration(displayName="AAA", key="K") public class A extends AbstractAction {
    public A(List wrongCnt) {}
    public void actionPerformed(ActionEvent e) {}}

