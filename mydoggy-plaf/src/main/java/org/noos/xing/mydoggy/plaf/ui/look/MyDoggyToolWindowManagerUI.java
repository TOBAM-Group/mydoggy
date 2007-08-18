package org.noos.xing.mydoggy.plaf.ui.look;

import org.noos.xing.mydoggy.ToolWindowManager;
import org.noos.xing.mydoggy.plaf.ui.ToolWindowManagerUI;
import org.noos.xing.mydoggy.plaf.ui.ToolWindowDescriptor;
import org.noos.xing.mydoggy.plaf.ui.DockedContainer;
import org.noos.xing.mydoggy.plaf.ui.cmp.DebugSplitPane;
import org.noos.xing.mydoggy.plaf.ui.cmp.ContentDesktopManager;
import org.noos.xing.mydoggy.plaf.ui.cmp.ToolWindowActiveButton;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.PanelUI;
import javax.swing.plaf.ButtonUI;
import javax.swing.plaf.basic.BasicButtonUI;
import java.awt.*;
import java.util.Map;
import java.util.Hashtable;

/**
 * @author Angelo De Caro (angelo.decaro@gmail.com)
 */
public class MyDoggyToolWindowManagerUI implements ToolWindowManagerUI {
    protected Map<String, ComponentCreator> cmpCreators;
    protected Map<String, ComponentUICreator> cmpUiCreators;
    protected Map<String, ComponentCustomizer> cmpCustomizers;

    public MyDoggyToolWindowManagerUI() {
        initComponentCreators();
    }

    public Component createComponent(String key, ToolWindowManager manager, Object... args) {
        return cmpCreators.get(key).createComponent(manager, args);
    }

    public ComponentUI createComponentUI(String key, ToolWindowManager manager, Object... args) {
        return cmpUiCreators.get(key).createComponentUI(manager, args);
    }

    public void applyCustomization(String key, Component component, Object... args) {
        cmpCustomizers.get(key).applyCustomization(component, args);
    }


    protected void initComponentCreators() {
        cmpCreators = new Hashtable<String, ComponentCreator>();
        cmpCreators.put(BAR_SPLIT_PANE, new BarSplitPaneComponentCreator());
        cmpCreators.put(BAR_CONTENT_PANE, new BarContentPaneComponentCreator());
        cmpCreators.put(CORNER_CONTENT_PANE, new CornerContentPaneComponentCreator());
        cmpCreators.put(MY_DOGGY_MANAGER_MAIN_CONTAINER, new MyDoggyManagerMainContainerComponentCreator());
        cmpCreators.put(DESKTOP_CONTENT_PANE, new DesktopContentPaneComponentCreator());
        cmpCreators.put(TOOL_WINDOW_TITLE_BAR, new ToolWindowTitleBarComponentCreator());
        cmpCreators.put(TOOL_WINDOW_TITLE_BUTTON, new ToolWindowTitleButtonComponentCreator());

        cmpUiCreators = new Hashtable<String, ComponentUICreator>();
        cmpUiCreators.put(REPRESENTATIVE_ANCHOR_BUTTON_UI, new RepresentativeAnchorButtonComponentUICreator());
        cmpUiCreators.put(TOOL_WINDOW_TITLE_BAR_UI, new ToolWindowTitleBarComponentUICreator());

        cmpCustomizers = new Hashtable<String, ComponentCustomizer>();
        cmpCustomizers.put(MY_DOGGY_MANAGER_PANEL, new MyDoggyManagerPanelComponentCustomizer());
    }


    public static interface ComponentCreator {
        Component createComponent(ToolWindowManager manager, Object... args);
    }

    public static interface ComponentUICreator {
        ComponentUI createComponentUI(ToolWindowManager manager, Object... args);
    }

    public static interface ComponentCustomizer {

        void applyCustomization(Component component, Object... args);
    }


    public static class BarSplitPaneComponentCreator implements ComponentCreator {

        public Component createComponent(ToolWindowManager manager, Object... args) {
            JSplitPane splitPane = new DebugSplitPane((Integer) args[0]);
            splitPane.setBorder(null);
            splitPane.setContinuousLayout(true);
            splitPane.setDividerSize(0);
            splitPane.setDividerLocation(300);
            splitPane.setLeftComponent(null);
            splitPane.setRightComponent(null);
            return splitPane;
        }
    }

    public static class BarContentPaneComponentCreator implements ComponentCreator {

        public Component createComponent(ToolWindowManager manager, Object... args) {
            JPanel panel = new JPanel();
//            panel.setBackground(Color.black);
            return panel;
        }
    }

    public static class CornerContentPaneComponentCreator implements ComponentCreator {

        public Component createComponent(ToolWindowManager manager, Object... args) {
            JPanel panel = new JPanel();
//            panel.setBackground(Color.black);
            return panel;
        }
    }

    public static class DesktopContentPaneComponentCreator implements ComponentCreator {

        public Component createComponent(ToolWindowManager manager, Object... args) {
            JDesktopPane desktopPane = new JDesktopPane();
            desktopPane.setDesktopManager(new ContentDesktopManager());
            return desktopPane;
        }
    }

    public static class ToolWindowTitleBarComponentCreator implements ComponentCreator {

        public Component createComponent(ToolWindowManager manager, Object... args) {
            JPanel titleBar = new JPanel() {
                public void setUI(PanelUI ui) {
                    if (ui instanceof ToolWindowTitleBarUI)
                        super.setUI(ui);
                }
            };
            titleBar.setBorder(null);
            titleBar.setUI(new ToolWindowTitleBarUI((ToolWindowDescriptor) args[0], (DockedContainer) args[1]));
            return titleBar;
        }
    }

    public static class ToolWindowTitleButtonComponentCreator implements ComponentCreator {

        public Component createComponent(ToolWindowManager manager, Object... args) {
            JButton button = new ToolWindowActiveButton();
            button.setUI((ButtonUI) BasicButtonUI.createUI(button));
            return button;
        }
        
    }


    public static class MyDoggyManagerMainContainerComponentCreator implements ComponentCreator {

        public Component createComponent(ToolWindowManager manager, Object... args) {
            JPanel panel = new JPanel();
            panel.setBackground(Color.GRAY);
            return panel;
        }
    }

    public static class RepresentativeAnchorButtonComponentUICreator implements ComponentUICreator {

        public ComponentUI createComponentUI(ToolWindowManager manager, Object... args) {
            return new RepresentativeAnchorUI((ToolWindowDescriptor) args[0]);
        }
    }

    public static class ToolWindowTitleBarComponentUICreator implements ComponentUICreator {

        public ComponentUI createComponentUI(ToolWindowManager manager, Object... args) {
            return new ToolWindowTitleBarUI((ToolWindowDescriptor) args[0], (DockedContainer) args[1]);
        }
    }

    public static class MyDoggyManagerPanelComponentCustomizer implements ComponentCustomizer {

        public void applyCustomization(Component component, Object... args) {
//            component.setBackground(Color.black);
        }
    }

}
