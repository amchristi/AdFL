package test;
import org.openide.awt.ActionRegistration;
import org.openide.awt.ActionID;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
@ActionID(category="Tools",id="my.action")@ActionRegistration(displayName="AAA", key="K") public class A implements ActionListener {
    public A(Integer[] params) {}
    public void actionPerformed(ActionEvent e) {}}

