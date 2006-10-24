package org.noos.xing.mydoggy.plaf.ui.content.tabbed;

import org.noos.xing.mydoggy.Content;
import org.noos.xing.mydoggy.plaf.MyDoggyContentManager;
import org.noos.xing.mydoggy.plaf.MyDoggyToolWindowManager;
import org.noos.xing.mydoggy.plaf.collections.ResolvableHashtable;
import org.noos.xing.mydoggy.plaf.ui.TransparencyAnimation;
import org.noos.xing.mydoggy.plaf.ui.content.ContentManagerUI;
import org.noos.xing.mydoggy.plaf.ui.content.ContentUI;
import org.noos.xing.mydoggy.plaf.ui.transparency.TransparencyManager;
import org.noos.xing.mydoggy.plaf.ui.util.SwingUtil;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import java.awt.*;
import java.awt.event.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Map;

/**
 * @author Angelo De Caro (angelo.decaro@gmail.com)
 */
public class TabbedContentManagerUI implements ContentManagerUI, PropertyChangeListener {
    private MyDoggyToolWindowManager toolWindowManager;
    private MyDoggyContentManager contentManager;

    private JTabbedContentManager tabbedContentManager;

    private Map<String, PropertyChangeListener> listeners;

    boolean valueAdjusting;

    public TabbedContentManagerUI(MyDoggyContentManager contentManager) {
        this.contentManager = contentManager;
        this.toolWindowManager = contentManager.getToolWindowManager();

        initComponents();
        initListeners();
    }


    public void addContent(ContentUI content) {
        addUIForContent(content);
        content.addUIPropertyChangeListener(this);
    }

    public void removeContent(ContentUI content) {
        if (content.isDetached())
            content.setDetached(false);

        int index = tabbedContentManager.indexOfComponent(content.getComponent());
        if (index != -1) {
            tabbedContentManager.removeTabAt(index);
            content.removePropertyChangeListener(this);
        } else if (toolWindowManager.getMainContent() != content.getComponent())
            throw new IllegalStateException("Invalid content ui state.");

        if (tabbedContentManager.getTabCount() == 0)
            toolWindowManager.resetMainContent();

        content.removeUIPropertyChangeListener(this);
    }

    public void detach(Content content) {
    }

    public JPopupMenu getPopupMenu() {
        return tabbedContentManager.getPopupMenu();
    }

    public void setPopupMenu(JPopupMenu popupMenu) {
        tabbedContentManager.setPopupMenu(popupMenu);
    }

    public boolean isSelected(Content content) {
        if (content.isDetached()) {
            Window anchestor = SwingUtilities.windowForComponent(content.getComponent());
            boolean focused = anchestor.isFocused();
            if (focused)
                return true;
            Component focusOwner = KeyboardFocusManager.getCurrentKeyboardFocusManager().getFocusOwner();
            return focusOwner != null && anchestor == SwingUtilities.windowForComponent(focusOwner);
        } else {
            int index = tabbedContentManager.indexOfComponent(content.getComponent());
            if (index != -1)
                return tabbedContentManager.getSelectedIndex() == index;
            else {
                if (toolWindowManager.getMainContent() != content.getComponent())
                    throw new IllegalStateException("Invalid content ui state.");
                else
                    return true;
            }
        }
    }

    public void setSelected(Content content, boolean selected) {
        if (content.isDetached()) {
            SwingUtil.requestFocus(
                    SwingUtilities.windowForComponent(content.getComponent())
            );
        } else {
            int index = tabbedContentManager.indexOfComponent(content.getComponent());
            if (index != -1)
                tabbedContentManager.setSelectedIndex(index);
            else if (toolWindowManager.getMainContent() != content.getComponent())
                throw new IllegalStateException("Invalid content ui state.");
        }
    }

    public void updateUI() {
        tabbedContentManager.updateUI();
    }


    public void propertyChange(PropertyChangeEvent evt) {
        listeners.get(evt.getPropertyName()).propertyChange(evt);
    }


    protected void initComponents() {
        final JTabbedContentManager tabbedContentManager = new JTabbedContentManager(
        );

        tabbedContentManager.addTabListener(new TabListener() {
            public void tabEventFired(TabEvent event) {
                switch (event.getActionId()) {
                    case ON_CLOSE:
                        contentManager.removeContent(event.getContentManager().getComponentAt(event.getOverTabIndex()));
                        break;
                    case ON_DETACH:
                        contentManager.getContent(
                                event.getContentManager().getComponentAt(event.getOverTabIndex())
                        ).setDetached(true);
                        break;
                }
            }
        });

        tabbedContentManager.addChangeListener(new ChangeListener() {
            private Component lastCmp;

            public void stateChanged(ChangeEvent e) {
                if (!valueAdjusting) {
                    Component selectedCmp = tabbedContentManager.getSelectedComponent();

                    if (lastCmp != null) {
                        try {
                            ((ContentUI) contentManager.getContent(lastCmp)).fireSelected(false);
                        } catch (Exception ignoreIt) {
                        }
                    }

                    for (Content content : contentManager.getContents()) {
                        if (content.getComponent() == selectedCmp) {
                            lastCmp = selectedCmp;
                            ((ContentUI) content).fireSelected(true);
                            break;
                        }
                    }
                }
            }
        });

        this.tabbedContentManager = tabbedContentManager;
    }

    protected void initListeners() {
        listeners = new ResolvableHashtable<String, PropertyChangeListener>(new PropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent evt) {
                System.out.println("NO LISTENER FOR : " + evt.getPropertyName());
            }
        });
        listeners.put("component", new ComponentListener());
        listeners.put("disabledIcon", new DisabledIconListener());
        listeners.put("icon", new IconListener());
        listeners.put("enabled", new EnabledListener());
        listeners.put("foreground", new ForegroundListener());
        listeners.put("popupMenu", new PopupMenuListener());
        listeners.put("title", new TitleListener());
        listeners.put("toolTipText", new ToolTipTextListener());
        listeners.put("detached", new DetachedListener());
        listeners.put("selected", new PropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent evt) {
//                System.out.println("SELECTED " + evt.getNewValue());
            }
        });

        contentManager.addPropertyChangeListener(this);
    }

    protected void addUIForContent(Content content) {
        if (tabbedContentManager.getTabCount() == 0 && toolWindowManager.getMainContent() == null) {
            toolWindowManager.setMainContent(content.getComponent());
        } else {
            if (tabbedContentManager.getParent() == null) {
                valueAdjusting = true;
                addTab(contentManager.getContent(toolWindowManager.getMainContent()));
                valueAdjusting = false;
            }

            addTab(content);
            toolWindowManager.setMainContent(tabbedContentManager);

            if (!tabbedContentManager.isEnabledAt(tabbedContentManager.getSelectedIndex()))
                tabbedContentManager.setSelectedIndex(tabbedContentManager.getTabCount() - 1);
        }
    }

    protected void addTab(Content content) {
        tabbedContentManager.addTab(content.getTitle(),
                                    content.getIcon(),
                                    content.getComponent(),
                                    content.getToolTipText());
        int index = tabbedContentManager.getTabCount() - 1;
        tabbedContentManager.setDisabledIconAt(index, content.getDisabledIcon());
        tabbedContentManager.setPopupMenuAt(index, content.getPopupMenu());
        tabbedContentManager.setForegroundAt(index, content.getForeground());
    }


    class ComponentListener implements PropertyChangeListener {
        public void propertyChange(PropertyChangeEvent evt) {
            Content content = (Content) evt.getSource();
            Component oldCmp = (Component) evt.getOldValue();
            Component newCmp = (Component) evt.getNewValue();

            if (content.isDetached()) {
                RootPaneContainer rootPaneContainer = (RootPaneContainer) SwingUtilities.windowForComponent(content.getComponent());
                Container container = rootPaneContainer.getContentPane();
                container.removeAll();
                container.add(newCmp);
            } else {
                int index = tabbedContentManager.indexOfComponent(oldCmp);
                if (index != -1)
                    tabbedContentManager.setComponentAt(index, newCmp);
                else {
                    if (toolWindowManager.getMainContent() == oldCmp)
                        toolWindowManager.setMainContent(newCmp);
                    else
                        throw new IllegalStateException("Invalid content ui state.");
                }
            }
        }
    }

    class DisabledIconListener implements PropertyChangeListener {
        public void propertyChange(PropertyChangeEvent evt) {
            Content content = (Content) evt.getSource();

            if (!content.isDetached()) {
                int index = tabbedContentManager.indexOfComponent(content.getComponent());
                if (index != -1)
                    tabbedContentManager.setDisabledIconAt(index, (Icon) evt.getNewValue());
                else if (toolWindowManager.getMainContent() != content.getComponent())
                    throw new IllegalStateException("Invalid content ui state.");
            }
        }
    }

    class IconListener implements PropertyChangeListener {
        public void propertyChange(PropertyChangeEvent evt) {
            Content content = (Content) evt.getSource();

            if (!content.isDetached()) {
                int index = tabbedContentManager.indexOfComponent(content.getComponent());
                if (index != -1)
                    tabbedContentManager.setIconAt(index, (Icon) evt.getNewValue());
                else if (toolWindowManager.getMainContent() != content.getComponent())
                    throw new IllegalStateException("Invalid content ui state.");
            }
        }
    }

    class EnabledListener implements PropertyChangeListener {
        public void propertyChange(PropertyChangeEvent evt) {
            Content content = (Content) evt.getSource();

            if (content.isDetached()) {
                Window anchestor = SwingUtilities.windowForComponent(content.getComponent());
                anchestor.setEnabled((Boolean) evt.getNewValue());
            } else {
                int index = tabbedContentManager.indexOfComponent(content.getComponent());
                if (index != -1)
                    tabbedContentManager.setEnabledAt(index, (Boolean) evt.getNewValue());
                else if (toolWindowManager.getMainContent() != content.getComponent())
                    throw new IllegalStateException("Invalid content ui state.");
            }
        }
    }

    class ForegroundListener implements PropertyChangeListener {
        public void propertyChange(PropertyChangeEvent evt) {
            Content content = (Content) evt.getSource();

            if (!content.isDetached()) {
                int index = tabbedContentManager.indexOfComponent(content.getComponent());
                if (index != -1)
                    tabbedContentManager.setForegroundAt(index, (Color) evt.getNewValue());
                else if (toolWindowManager.getMainContent() != content.getComponent())
                    throw new IllegalStateException("Invalid content ui state.");
            }
        }
    }

    class PopupMenuListener implements PropertyChangeListener {
        public void propertyChange(PropertyChangeEvent evt) {
            Content content = (Content) evt.getSource();

            if (!content.isDetached()) {
                int index = tabbedContentManager.indexOfComponent(content.getComponent());
                if (index != -1)
                    tabbedContentManager.setPopupMenuAt(index, (JPopupMenu) evt.getNewValue());
                else if (toolWindowManager.getMainContent() != content.getComponent())
                    throw new IllegalStateException("Invalid content ui state.");
            }
        }
    }

    class TitleListener implements PropertyChangeListener {
        public void propertyChange(PropertyChangeEvent evt) {
            Content content = (Content) evt.getSource();

            if (content.isDetached()) {
                JDialog dialog = (JDialog) SwingUtilities.windowForComponent(content.getComponent());
                dialog.setTitle((String) evt.getNewValue());
            } else {
                int index = tabbedContentManager.indexOfComponent(content.getComponent());
                if (index != -1)
                    tabbedContentManager.setTitleAt(index, (String) evt.getNewValue());
                else if (toolWindowManager.getMainContent() != content.getComponent())
                    throw new IllegalStateException();
            }
        }
    }

    class ToolTipTextListener implements PropertyChangeListener {
        public void propertyChange(PropertyChangeEvent evt) {
            Content content = (Content) evt.getSource();

            if (!content.isDetached()) {
                int index = tabbedContentManager.indexOfComponent(content.getComponent());
                if (index != -1) {
                    String newToolTip = (String) evt.getNewValue();
                    if (newToolTip == null)
                        newToolTip = "";
                    tabbedContentManager.setToolTipTextAt(index, newToolTip);
                } else if (toolWindowManager.getMainContent() != content.getComponent())
                    throw new IllegalStateException("Invalid content ui state.");
            }
        }
    }

    class DetachedListener implements PropertyChangeListener {
        private Frame parentFrame;

        public DetachedListener() {
            parentFrame = (toolWindowManager.getAnchestor() instanceof Frame) ? (Frame) toolWindowManager.getAnchestor() : null;
        }

        public void propertyChange(PropertyChangeEvent evt) {
            Content content = (Content) evt.getSource();
            boolean oldValue = (Boolean) evt.getOldValue();
            boolean newValue = (Boolean) evt.getNewValue();

            if (!oldValue && newValue) {
                final JDialog dialog = new JDialog(parentFrame, false);
                dialog.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);

                Window parentWindow = SwingUtilities.windowForComponent(tabbedContentManager);
                Component component = content.getComponent();

                int tabIndex = tabbedContentManager.indexOfComponent(component);
                if (tabIndex != -1) {
                    tabbedContentManager.removeTabAt(tabIndex);
                } else {
                    if (tabbedContentManager.getParent() == null)
                        toolWindowManager.setMainContent(null);
                    else
                        throw new IllegalStateException("Invalid Content : " + content);
                }

                component.setPreferredSize(component.getSize());

                dialog.setTitle(content.getTitle());
                dialog.getContentPane().add(component);

                Point location = parentWindow.getLocation();
                location.x += 5;
                location.y += 5;
                dialog.setLocation(location);

                dialog.pack();

                if (TransparencyManager.getInstance().isServiceAvailable()) {
                    TransparencyListener transparencyListener = new TransparencyListener(dialog);
                    dialog.addWindowListener(transparencyListener);
                    dialog.addWindowFocusListener(transparencyListener);
                }

                dialog.addWindowListener(new WindowAdapter() {
                    public void windowClosing(WindowEvent event) {
                        Component component = dialog.getContentPane().getComponent(0);
                        ContentUI content = (ContentUI) contentManager.getContent(component);
                        content.fireSelected(false);
                        content.setDetached(false);
                    }
                });

                dialog.addWindowFocusListener(new WindowFocusListener() {
                    public void windowGainedFocus(WindowEvent e) {
                        ContentUI content = (ContentUI) contentManager.getContent(dialog.getContentPane().getComponent(0));
                        content.fireSelected(true);
                    }

                    public void windowLostFocus(WindowEvent e) {
                        if (dialog.getContentPane().getComponentCount() > 0) {
                            ContentUI content = (ContentUI) contentManager.getContent(dialog.getContentPane().getComponent(0));
                            content.fireSelected(false);
                        }
                    }
                });

                if (parentFrame == null) {
                    WindowFocusListener windowFocusListener = new WindowFocusListener() {
                        long start;
                        long end;

                        public void windowGainedFocus(WindowEvent e) {
                            start = System.currentTimeMillis();
                        }

                        public void windowLostFocus(WindowEvent e) {
                            end = System.currentTimeMillis();
                            long elapsed = end - start;
                            //System.out.println(elapsed);
                            if (elapsed < 100)
                                dialog.toFront();

                            dialog.removeWindowFocusListener(this);
                        }
                    };
                    dialog.addWindowFocusListener(windowFocusListener);
                }

                dialog.toFront();
                dialog.setVisible(true);
                SwingUtil.requestFocus(dialog);
            } else if (oldValue && !newValue) {
                Window window = SwingUtilities.windowForComponent(content.getComponent());
                window.setVisible(false);
                window.dispose();

                addUIForContent(content);
                tabbedContentManager.setSelectedIndex(tabbedContentManager.getTabCount() - 1);
            }
        }

        class TransparencyListener extends WindowAdapter implements WindowFocusListener, ActionListener {
            private final TransparencyManager transparencyManager = TransparencyManager.getInstance();

            private TransparencyAnimation animation;

            private Timer timer;
            private JDialog window;

            public TransparencyListener(JDialog window) {
                this.window = window;
                this.animation = new TransparencyAnimation(window, 0.7f);
            }

            public void windowGainedFocus(WindowEvent e) {
                if (timer != null)
                    timer.stop();

                if (transparencyManager.isAlphaModeEnabled(e.getWindow())) {
                    animation.hide();
                    transparencyManager.setAlphaModeRatio(e.getWindow(), 0.0f);
                }
            }

            public void windowLostFocus(WindowEvent e) {
                if (!transparencyManager.isAlphaModeEnabled(e.getWindow())) {
                    timer = new Timer(1500, this);
                    timer.start();
                }
            }

            public void actionPerformed(ActionEvent e) {
                if (timer.isRunning()) {
                    timer.stop();
                    synchronized (transparencyManager) {
                        animation.show();
                    }
                }
            }

            public void windowClosing(WindowEvent event) {
                if (transparencyManager.isAlphaModeEnabled(event.getWindow())) {
                    animation.hide();
                    transparencyManager.setAlphaModeRatio(window, 0.0f);
                }
            }

        }
    }
}