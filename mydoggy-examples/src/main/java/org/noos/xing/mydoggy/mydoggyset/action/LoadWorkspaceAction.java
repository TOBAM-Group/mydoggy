package org.noos.xing.mydoggy.mydoggyset.action;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.FileInputStream;

import javax.swing.AbstractAction;
import javax.swing.JOptionPane;

import org.noos.xing.mydoggy.Content;
import org.noos.xing.mydoggy.PersistenceDelegate;
import org.noos.xing.mydoggy.PersistenceDelegateCallback;
import org.noos.xing.mydoggy.ToolWindow;
import org.noos.xing.mydoggy.ToolWindowGroup;
import org.noos.xing.mydoggy.ToolWindowManager;
import org.noos.xing.mydoggy.mydoggyset.MyDoggySet;
import org.noos.xing.mydoggy.mydoggyset.context.MyDoggySetContext;
import org.noos.xing.mydoggy.plaf.ui.ResourceManager;
import org.noos.xing.yasaf.view.ViewContext;
import org.noos.xing.yasaf.view.ViewContextChangeListener;
import org.noos.xing.yasaf.view.event.ViewContextChangeEvent;

/**
 * @author Angelo De Caro (angelo.decaro@gmail.com)
 */
public class LoadWorkspaceAction extends AbstractAction implements ViewContextChangeListener {
    protected Component parentComponent;
    protected ToolWindowManager toolWindowManager;
    protected ViewContext myDoggySetContext;

    public LoadWorkspaceAction(ViewContext myDoggySetContext, Component parentComponent, ToolWindowManager toolWindowManager) {
        super("Load Workspace");
        this.myDoggySetContext = myDoggySetContext;
        this.parentComponent = parentComponent;
        this.toolWindowManager = toolWindowManager;
        myDoggySetContext.addViewContextChangeListener("loadWorkspace", this);
    }

    @Override
    public void contextChange(ViewContextChangeEvent evt) {
        actionPerformed(null);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        File workspaceFile = new File("workspace.xml");
        if (workspaceFile.exists()) {
            try (FileInputStream inputStream = new FileInputStream("workspace.xml")) {
                toolWindowManager.getPersistenceDelegate().merge(inputStream, PersistenceDelegate.MergePolicy.RESET, new PersistenceDelegateCallback() {

                    @Override
                    public ToolWindow toolwindowNotFound(ToolWindowManager toolWindowManager, String toolWindowId, PersistenceNode node) {
                        return null;
                    }

                    @Override
                    public Content contentNotFound(ToolWindowManager toolWindowManager, String contentId, PersistenceNode node) {
                        if ("Welcome".equals(contentId)) {
                            myDoggySetContext.put(MyDoggySet.class, null);
                        } else if ("Manager".equals(contentId)) {
                            myDoggySetContext.put(ToolWindowManager.class, null);
                        } else if ("Tools".equals(contentId)) {
                            myDoggySetContext.put(ToolWindow.class, null);
                        } else if ("Groups".equals(contentId)) {
                            myDoggySetContext.put(ToolWindowGroup.class, null);
                        } else if ("Contents".equals(contentId)) {
                            myDoggySetContext.put(Content.class, null);
                        } else if ("Customize".equals(contentId)) {
                            myDoggySetContext.put(ResourceManager.class, null);
                        } else if ("Nested Manager".equals(contentId)) {
                            myDoggySetContext.put(MyDoggySetContext.ActionKey.NEST_TOOLMANAGER, null);
                        }

                        return toolWindowManager.getContentManager().getContent(contentId);
                    }

                    @Override
                    public String validate(PersistenceNode node, String attribute, String attributeValue, Object attributeDefaultValue) {
                        return attributeValue;
                    }
                });
            } catch (Exception e1) {
                e1.printStackTrace();
            }
        } else {
            JOptionPane.showMessageDialog(parentComponent, "You must save the workspace before the load.");
        }
    }
}
