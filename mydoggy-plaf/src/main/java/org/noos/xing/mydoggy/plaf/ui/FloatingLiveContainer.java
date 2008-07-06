package org.noos.xing.mydoggy.plaf.ui;

import org.noos.xing.mydoggy.AggregationPosition;
import org.noos.xing.mydoggy.FloatingLiveTypeDescriptor;
import org.noos.xing.mydoggy.ToolWindowAnchor;
import org.noos.xing.mydoggy.ToolWindowType;
import org.noos.xing.mydoggy.plaf.ui.cmp.FloatingLivePanel;
import org.noos.xing.mydoggy.plaf.ui.cmp.event.FloatingMoveMouseInputHandler;
import org.noos.xing.mydoggy.plaf.ui.util.SwingUtil;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

/**
 * @author Angelo De Caro
 */
public class FloatingLiveContainer extends MyDoggyToolWindowContainer {
    protected FloatingLivePanel floatingLivePanel;
    protected FloatingMoveMouseInputHandler moveMouseInputHandler;
    protected ComponentListener livePanelComponentListener;

    protected boolean settedListener = false;
    protected Rectangle lastBounds;
    protected boolean valueAdjusting;


    public FloatingLiveContainer(DockedContainer dockedContainer) {
        super(dockedContainer);

        initComponents();
        initListeners();
    }


    public void cleanup() {
        // Remove Listeners
        toolWindowTabContainer.removeEventDispatcherlListener(moveMouseInputHandler);

        titleBar.removeMouseMotionListener(moveMouseInputHandler);
        titleBar.removeMouseListener(moveMouseInputHandler);

        descriptor.getTypeDescriptor(ToolWindowType.FLOATING_LIVE).removePropertyChangeListener(this);

        floatingLivePanel = null;
        
        super.cleanup();
    }


    public void setVisible(boolean visible) {
        Component content = dockedContainer.getContentContainer();

        if (visible) {
            descriptor.setIdOnTitleBar();
            dockedContainer.getToolWindowTitleButtonPanel().setType(ToolWindowType.FLOATING_LIVE);

            // retrieve common panel
            floatingLivePanel = descriptor.getFloatingLivePanel(toolWindow);

            // setup components
            floatingLivePanel.resetLayout();
            content.setVisible(true);
            floatingLivePanel.addDockable(toolWindow, content);

            // setup listener
            moveMouseInputHandler.setFloatingContainer(floatingLivePanel);
            floatingLivePanel.addComponentListener(livePanelComponentListener);

            // Prepare common panel
            floatingLivePanel.setBorder(BorderFactory.createEtchedBorder());

            if (lastBounds == null) {
                FloatingLiveTypeDescriptor typeDescriptor = (FloatingLiveTypeDescriptor) descriptor.getTypeDescriptor(ToolWindowType.FLOATING_LIVE);

                // Set Size
                if (typeDescriptor.getSize() == null) {
                    Component managerCmp = descriptor.getManager();

                    switch (toolWindow.getAnchor()) {
                        case LEFT:
                        case RIGHT:
                            floatingLivePanel.setSize(descriptor.getDockedTypeDescriptor().getDockLength(),
                                                      (int) (managerCmp.getHeight() / 1.5));
                            break;
                        case TOP:
                        case BOTTOM:
                            floatingLivePanel.setSize((int) (managerCmp.getWidth() / 1.5),
                                                      descriptor.getDockedTypeDescriptor().getDockLength());
                            break;
                    }
                } else
                    floatingLivePanel.setSize(typeDescriptor.getSize());

                SwingUtil.validateBounds(floatingLivePanel, descriptor.getManager().getMainContainer().getBounds());

                // Set Location
                if (typeDescriptor.getLocation() == null ||
                    typeDescriptor.getLocation().x > descriptor.getManager().getWidth() ||
                    typeDescriptor.getLocation().y > descriptor.getManager().getHeight() ||
                    typeDescriptor.getLocation().x < 0 ||
                    typeDescriptor.getLocation().y < 0) {
                    Component managerCmp = descriptor.getManager();

                    switch (toolWindow.getAnchor()) {
                        case LEFT:
                            floatingLivePanel.setLocation(50, 50);
                            break;
                        case RIGHT:
                            floatingLivePanel.setLocation(managerCmp.getWidth() - 50 - floatingLivePanel.getWidth(),
                                                          50);
                            break;
                        case TOP:
                            floatingLivePanel.setLocation(50, 50);
                            break;
                        case BOTTOM:
                            floatingLivePanel.setLocation(50,
                                                          managerCmp.getHeight() - 50 - floatingLivePanel.getHeight());
                            break;
                    }
                } else
                    floatingLivePanel.setLocation(typeDescriptor.getLocation());
            } else {
                floatingLivePanel.setBounds(lastBounds);
                lastBounds = null;
            }

            // mount common panel
            floatingLivePanel.mount();
        } else {
            // remove dockable
            floatingLivePanel.removeDockable(toolWindow);
            floatingLivePanel.removeComponentListener(livePanelComponentListener);

            // unmount
            floatingLivePanel.unmount();
            
            floatingLivePanel = null;
        }
    }

    public void setVisible(ToolWindowDescriptor destDescriptor, Component content, AggregationPosition aggregationPosition) {
        // retrieve common panel
        floatingLivePanel = ((FloatingLiveContainer) destDescriptor.getToolWindowContainer(ToolWindowType.FLOATING_LIVE)).getFloatingLivePanel();

        // setup components
        floatingLivePanel.addDockable(toolWindow,
                                      content,
                                      destDescriptor.getToolWindow(), 
                                      aggregationPosition);

        // setup listeners
        moveMouseInputHandler.setFloatingContainer(floatingLivePanel);
        floatingLivePanel.addComponentListener(livePanelComponentListener);

        // Mount panel
        floatingLivePanel.mount();
    }


    public FloatingLivePanel getFloatingLivePanel() {
        return floatingLivePanel;
    }


    protected void initComponents() {
    }

    protected void initListeners() {
        addPropertyChangeListener("type", new PropertyChangeListener() {

            public void propertyChange(PropertyChangeEvent evt) {
                if (evt.getSource() != descriptor)
                    return;

                assert "type".equals(evt.getPropertyName());
                if (evt.getNewValue() == ToolWindowType.FLOATING_LIVE) {
                    if (descriptor.getManager().getLayeredPane() != null) {
                        descriptor.setIdOnTitleBar();

                        // Remove listeners
                        toolWindowTabContainer.removeEventDispatcherlListener(moveMouseInputHandler);

                        titleBar.removeMouseMotionListener(moveMouseInputHandler);
                        titleBar.removeMouseListener(moveMouseInputHandler);

                        // Add listeners
                        toolWindowTabContainer.addEventDispatcherlListener(moveMouseInputHandler);

                        titleBar.addMouseMotionListener(moveMouseInputHandler);
                        titleBar.addMouseListener(moveMouseInputHandler);

                        settedListener = true;
                    }
                } else if (evt.getOldValue() == ToolWindowType.FLOATING_LIVE) {
                    if (descriptor.getManager().getLayeredPane() != null) {
                        if (!descriptor.getDockedTypeDescriptor().isIdVisibleOnTitleBar())
                            dockedContainer.disableIdOnTitleBar();

                        if (settedListener)
                            lastBounds = descriptor.getFloatingLivePanel(toolWindow).getBounds();

                        // Remove listeners
                        toolWindowTabContainer.removeEventDispatcherlListener(moveMouseInputHandler);

                        titleBar.removeMouseMotionListener(moveMouseInputHandler);
                        titleBar.removeMouseListener(moveMouseInputHandler);

                        settedListener = false;
                    }
                }
            }
        });
        addPropertyChangeListener("maximized", new PropertyChangeListener() {
            protected Rectangle oldBounds = null;

            public void propertyChange(PropertyChangeEvent evt) {
                if (toolWindow.getType() == ToolWindowType.FLOATING_LIVE) {

                    FloatingLivePanel floatingLivePanel = descriptor.getFloatingLivePanel(toolWindow);
                    if ((Boolean) evt.getNewValue()) {
                        oldBounds = floatingLivePanel.getBounds();

                        Rectangle bounds = descriptor.getManager().getMainContainer().getBounds();
                        bounds = SwingUtilities.convertRectangle(descriptor.getManager().getMainContainer(),
                                                                 bounds,
                                                                 descriptor.getManager().getRootPane().getLayeredPane());
                        floatingLivePanel.setBounds(bounds);
                    } else {
                        floatingLivePanel.setBounds(oldBounds);
                    }

                    SwingUtil.repaint(floatingLivePanel);
                }
            }
        });
        addPropertyChangeListener("location", new PropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent evt) {
                if (descriptor.getTypeDescriptor(ToolWindowType.FLOATING_LIVE) != evt.getSource())
                    return;

                if (valueAdjusting)
                    return;

                FloatingLivePanel floatingLivePanel = descriptor.getFloatingLivePanel(toolWindow);
                if (floatingLivePanel.isVisible()) {
                    Point location = (Point) evt.getNewValue();
                    floatingLivePanel.setLocation(location);
                }
                lastBounds = null;
            }
        });
        addPropertyChangeListener("size", new PropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent evt) {
                if (descriptor.getTypeDescriptor(ToolWindowType.FLOATING_LIVE) != evt.getSource())
                    return;

                if (valueAdjusting)
                    return;

                FloatingLivePanel floatingLivePanel = descriptor.getFloatingLivePanel(toolWindow);
                if (floatingLivePanel.isVisible()) {
                    Dimension size = (Dimension) evt.getNewValue();
                    floatingLivePanel.setSize(size);
                }
                lastBounds = null;
            }
        });
        addPropertyChangeListener("enabled", new TypeEnabledPropertyChangeListener());

        descriptor.getTypeDescriptor(ToolWindowType.FLOATING_LIVE).addPropertyChangeListener(this);
        moveMouseInputHandler = new FloatingLiveMoveMouseInputHandler(null);
        livePanelComponentListener = new LivePanelComponentListener();
    }


    public class TypeEnabledPropertyChangeListener implements PropertyChangeListener {

        public void propertyChange(PropertyChangeEvent evt) {
            boolean newValue = (Boolean) evt.getNewValue();

            if (!newValue && toolWindow.getType() == ToolWindowType.FLOATING_LIVE)
                toolWindow.setType(ToolWindowType.DOCKED);
        }

    }

    public class FloatingLiveMoveMouseInputHandler extends FloatingMoveMouseInputHandler {
        protected ToolWindowAnchor onAnchor;

        public FloatingLiveMoveMouseInputHandler(Component floatingContainer) {
            super(floatingContainer);
        }

        public void mouseDragged(MouseEvent ev) {
            super.mouseDragged(ev);

            if (isMovingWindow) {
                // TODO: activate tempo showed
                onAnchor = descriptor.getManager().getToolWindowAnchor(
                        SwingUtilities.convertPoint(ev.getComponent(), ev.getPoint(), descriptor.getManager())
                );

            }
        }

        public void mouseReleased(MouseEvent ev) {
            if (isMovingWindow) {
                if (onAnchor != null) {
                    try {
                        toolWindow.setAnchor(onAnchor);
                        toolWindow.setType(ToolWindowType.DOCKED);
                    } finally {
                        isMovingWindow = false;
                        dragCursor = 0;
                    }
                } else
                    super.mouseReleased(ev);
            }
        }
    }

    public class LivePanelComponentListener extends ComponentAdapter {

        public void componentResized(ComponentEvent e) {
            valueAdjusting = true;
            try {
                toolWindow.getTypeDescriptor(FloatingLiveTypeDescriptor.class).setSize(floatingLivePanel.getWidth(),
                                                                                       floatingLivePanel.getHeight());
            } finally {
                valueAdjusting = false;
            }
        }

        public void componentMoved(ComponentEvent e) {
            valueAdjusting = true;
            try {
                toolWindow.getTypeDescriptor(FloatingLiveTypeDescriptor.class).setLocation(floatingLivePanel.getX(),
                                                                                           floatingLivePanel.getY());
            } finally {
                valueAdjusting = false;
            }
        }
    }

}