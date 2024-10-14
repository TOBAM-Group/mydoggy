package org.noos.xing.mydoggy.plaf;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.awt.Component;

import javax.swing.JFrame;
import javax.swing.JLabel;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.noos.xing.mydoggy.ToolWindow;
import org.noos.xing.mydoggy.ToolWindowAnchor;
import org.noos.xing.mydoggy.ToolWindowGroup;
import org.noos.xing.mydoggy.ToolWindowManager;
import org.noos.xing.mydoggy.ToolWindowManagerListener;
import org.noos.xing.mydoggy.event.ToolWindowManagerEvent;

/**
 * @author Angelo De Caro (angelo.decaro@gmail.com)
 */
public class TestTWManagerListener {
    private JFrame frame;
    private ToolWindowManager toolWindowManager;

	@BeforeEach
    protected void setUp() throws Exception {
        frame = new JFrame("test");
        toolWindowManager = new MyDoggyToolWindowManager();
        frame.add((Component) toolWindowManager);
    }

	@AfterEach
    protected void tearDown() throws Exception {
        frame.dispose();
    }

	@Test
	void testRegisterToolWindow() {
        TestToolWindowManagerListener listener = new TestToolWindowManagerListener();
        toolWindowManager.addToolWindowManagerListener(listener);

        ToolWindow tool = toolWindowManager.registerToolWindow(
                "test", "title", null, new JLabel("Test"), ToolWindowAnchor.TOP
        );

        assertTrue(listener.isWindowRegistered());
        assertNotNull(listener.getLastEvent());
        assertEquals(ToolWindowManagerEvent.ActionId.TOOL_REGISTERED, listener.getLastEvent().getId());
        assertEquals(tool, listener.getLastEvent().getToolWindow());
    }

	@Test
	void testUnregisterToolWindow() {
        TestToolWindowManagerListener listener = new TestToolWindowManagerListener();
        toolWindowManager.addToolWindowManagerListener(listener);
        ToolWindow tool = toolWindowManager.registerToolWindow(
                "test", "title", null, new JLabel("Test"), ToolWindowAnchor.TOP
        );

        toolWindowManager.unregisterToolWindow(tool.getId());

        assertTrue(listener.isWindowUnregistered());
        assertNotNull(listener.getLastEvent());
        assertEquals(ToolWindowManagerEvent.ActionId.TOOL_UNREGISTERED, listener.getLastEvent().getId());
        assertEquals(tool, listener.getLastEvent().getToolWindow());
    }

	@Test
	void testUnregisterAllToolWindow() {
        TestToolWindowManagerListener listener = new TestToolWindowManagerListener();
        toolWindowManager.addToolWindowManagerListener(listener);
        ToolWindow tool = toolWindowManager.registerToolWindow(
                "test", "title", null, new JLabel("Test"), ToolWindowAnchor.TOP
        );

        toolWindowManager.unregisterAllToolWindow();

        assertTrue(listener.isWindowUnregistered());
        assertNotNull(listener.getLastEvent());
        assertEquals(ToolWindowManagerEvent.ActionId.TOOL_UNREGISTERED, listener.getLastEvent().getId());
        assertEquals(tool, listener.getLastEvent().getToolWindow());
    }

	@Test
	void testAddToolWindowGroup() {
        TestToolWindowManagerListener listener = new TestToolWindowManagerListener();
        toolWindowManager.addToolWindowManagerListener(listener);

        ToolWindowGroup group = toolWindowManager.getToolWindowGroup("main");

        assertTrue(listener.isWindowGroupAdded());
        assertNotNull(listener.getLastEvent());
        assertEquals(ToolWindowManagerEvent.ActionId.GROUP_ADDED, listener.getLastEvent().getId());
        assertEquals(group, listener.getLastEvent().getToolWindowGroup());
    }

	@Test
	void testRemoveToolWindowGroup() {
        TestToolWindowManagerListener listener = new TestToolWindowManagerListener();
        toolWindowManager.addToolWindowManagerListener(listener);

        ToolWindowGroup group = toolWindowManager.getToolWindowGroup("main");
        toolWindowManager.removeToolWindowGroup(group.getName());

        assertTrue(listener.isWindowGroupRemoved());
        assertNotNull(listener.getLastEvent());
        assertEquals(ToolWindowManagerEvent.ActionId.GROUP_REMOVED, listener.getLastEvent().getId());
        assertEquals(group, listener.getLastEvent().getToolWindowGroup());
    }

    private class TestToolWindowManagerListener implements ToolWindowManagerListener {
        private boolean windowRegistered;
        private boolean windowUnregistered;
        private boolean windowGroupAdded;
        private boolean windowGroupRemoved;

        private ToolWindowManagerEvent lastEvent;

        @Override
		public void toolWindowRegistered(ToolWindowManagerEvent event) {
            this.windowRegistered = true;
            this.lastEvent = event;
        }

        @Override
		public void toolWindowUnregistered(ToolWindowManagerEvent event) {
            this.windowUnregistered = true;
            this.lastEvent = event;
        }

        @Override
		public void toolWindowGroupAdded(ToolWindowManagerEvent event) {
            this.windowGroupAdded = true;
            this.lastEvent = event;
        }

        @Override
		public void toolWindowGroupRemoved(ToolWindowManagerEvent event) {
            this.windowGroupRemoved = true;
            this.lastEvent = event;
        }


        public boolean isWindowGroupAdded() {
            return windowGroupAdded;
        }

        public boolean isWindowGroupRemoved() {
            return windowGroupRemoved;
        }

        public boolean isWindowRegistered() {
            return windowRegistered;
        }

        public boolean isWindowUnregistered() {
            return windowUnregistered;
        }

        public ToolWindowManagerEvent getLastEvent() {
            return lastEvent;
        }

    }

}
