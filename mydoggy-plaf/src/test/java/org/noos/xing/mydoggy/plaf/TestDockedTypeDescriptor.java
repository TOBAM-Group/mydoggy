package org.noos.xing.mydoggy.plaf;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.awt.Component;

import javax.swing.JFrame;
import javax.swing.JLabel;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.noos.xing.mydoggy.DockedTypeDescriptor;
import org.noos.xing.mydoggy.ToolWindow;
import org.noos.xing.mydoggy.ToolWindowAnchor;
import org.noos.xing.mydoggy.ToolWindowManager;
import org.noos.xing.mydoggy.ToolWindowType;

/**
 * @author Angelo De Caro (angelo.decaro@gmail.com)
 */
class TestDockedTypeDescriptor {

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
	void testDockedTypeDescriptor() {
        assertNotNull(toolWindowManager.getTypeDescriptorTemplate(ToolWindowType.DOCKED));
    }

	@Test
	void testDockedTypeDescriptorDockLength() {
        DockedTypeDescriptor descriptor = (DockedTypeDescriptor) toolWindowManager.getTypeDescriptorTemplate(ToolWindowType.DOCKED);
        descriptor.setDockLength(150);

        descriptor = (DockedTypeDescriptor) toolWindowManager.getTypeDescriptorTemplate(ToolWindowType.DOCKED);
        assertEquals(150, descriptor.getDockLength());
    }

	@Test
	void testDockedTypeDescriptorPopupMenuEnabled() {
        DockedTypeDescriptor descriptor = (DockedTypeDescriptor) toolWindowManager.getTypeDescriptorTemplate(ToolWindowType.DOCKED);
        descriptor.setPopupMenuEnabled(false);

        descriptor = (DockedTypeDescriptor) toolWindowManager.getTypeDescriptorTemplate(ToolWindowType.DOCKED);
        assertEquals(false, descriptor.isPopupMenuEnabled());

        descriptor = (DockedTypeDescriptor) toolWindowManager.getTypeDescriptorTemplate(ToolWindowType.DOCKED);
        descriptor.setPopupMenuEnabled(true);

        descriptor = (DockedTypeDescriptor) toolWindowManager.getTypeDescriptorTemplate(ToolWindowType.DOCKED);
        assertEquals(true, descriptor.isPopupMenuEnabled());
    }

	@Test
	void testDockedTypeDescriptorUserDefinedMenu() {
        DockedTypeDescriptor descriptor = (DockedTypeDescriptor) toolWindowManager.getTypeDescriptorTemplate(ToolWindowType.DOCKED);
        assertNotNull(descriptor.getToolsMenu());
    }


	@Test
	void testTWDockedTypeDescriptorDockLength() {
        DockedTypeDescriptor descriptor = (DockedTypeDescriptor) toolWindowManager.getTypeDescriptorTemplate(ToolWindowType.DOCKED);
        descriptor.setDockLength(150);

        // Check DockLength on tool window type manager
        ToolWindow toolWindow = toolWindowManager.registerToolWindow("id", "title", null, new JLabel("title"), ToolWindowAnchor.LEFT);
        descriptor = (DockedTypeDescriptor) toolWindow.getTypeDescriptor(ToolWindowType.DOCKED);
        assertEquals(150, descriptor.getDockLength());

        // Modify manager type manager
        descriptor = (DockedTypeDescriptor) toolWindowManager.getTypeDescriptorTemplate(ToolWindowType.DOCKED);
        descriptor.setDockLength(250);

        // Check modification
        descriptor = (DockedTypeDescriptor) toolWindow.getTypeDescriptor(ToolWindowType.DOCKED);
        assertEquals(250, descriptor.getDockLength());
    }

	@Test
	void testTWDockedTypeDescriptorPopupMenuEnabled() {
        DockedTypeDescriptor descriptor = (DockedTypeDescriptor) toolWindowManager.getTypeDescriptorTemplate(ToolWindowType.DOCKED);
        descriptor.setPopupMenuEnabled(false);

        // Check DockLength on tool window type manager
        ToolWindow toolWindow = toolWindowManager.registerToolWindow("id", "title", null, new JLabel("title"), ToolWindowAnchor.LEFT);
        descriptor = (DockedTypeDescriptor) toolWindow.getTypeDescriptor(ToolWindowType.DOCKED);
        assertEquals(false, descriptor.isPopupMenuEnabled());

        // Modify manager type manager
        descriptor = (DockedTypeDescriptor) toolWindowManager.getTypeDescriptorTemplate(ToolWindowType.DOCKED);
        descriptor.setPopupMenuEnabled(true);

        // Check modification
        descriptor = (DockedTypeDescriptor) toolWindow.getTypeDescriptor(ToolWindowType.DOCKED);
        assertEquals(true, descriptor.isPopupMenuEnabled());
    }

}
