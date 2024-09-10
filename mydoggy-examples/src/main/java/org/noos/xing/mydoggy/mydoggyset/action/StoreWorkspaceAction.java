package org.noos.xing.mydoggy.mydoggyset.action;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.io.FileOutputStream;

import javax.swing.AbstractAction;
import javax.swing.JOptionPane;

import org.noos.xing.mydoggy.ToolWindowManager;

/**
 * @author Angelo De Caro (angelo.decaro@gmail.com)
 */
public class StoreWorkspaceAction extends AbstractAction {
    private Component parentComponent;
    private ToolWindowManager toolWindowManager;

    public StoreWorkspaceAction(Component parentComponent, ToolWindowManager toolWindowManager) {
        super("Store Workspace");
        this.parentComponent = parentComponent;
        this.toolWindowManager = toolWindowManager;
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        try (FileOutputStream output = new FileOutputStream("workspace.xml")) {
            toolWindowManager.getPersistenceDelegate().save(output);
            JOptionPane.showMessageDialog(parentComponent, "Workspace saved to 'workspace.xml'.");
        } catch (Exception e1) {
            e1.printStackTrace();
        }
    }
}