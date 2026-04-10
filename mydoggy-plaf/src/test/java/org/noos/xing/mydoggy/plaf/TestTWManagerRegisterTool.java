package org.noos.xing.mydoggy.plaf;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.fail;

import java.awt.Component;

import javax.swing.JFrame;
import javax.swing.JLabel;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.noos.xing.mydoggy.ToolWindow;
import org.noos.xing.mydoggy.ToolWindowAnchor;
import org.noos.xing.mydoggy.ToolWindowManager;

/**
 * @author Angelo De Caro (angelo.decaro@gmail.com)
 */
public class TestTWManagerRegisterTool {

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
    public void testRegisterToolWindow() {

        ToolWindow toolWindow = toolWindowManager.registerToolWindow(
                "id", "title", null, new JLabel("label"), ToolWindowAnchor.LEFT
        );

        assertNotNull(toolWindow);
        assertEquals("id", toolWindow.getId());
        assertEquals("title", toolWindow.getTitle());
        assertNull(toolWindow.getIcon());
        assertEquals(ToolWindowAnchor.LEFT, toolWindow.getAnchor());

        assertEquals(1, toolWindowManager.getToolWindows().length);
        assertEquals(toolWindow, toolWindowManager.getToolWindow("id"));
    }

    /**
     * We try to register a tool window with a null id.
     */
	@Test
    public void testRegisterToolWindowUnuseCaseOne() {
        try {
            ToolWindow toolWindow = toolWindowManager.registerToolWindow(
                    null, "title", null, new JLabel("label"), ToolWindowAnchor.LEFT
            );
            fail("Try body must throws a RuntimeException");
        } catch (Exception e) {
        }
    }

    /**
     * We try to register a tool window with a null compoennt.
     */
	@Test
    public void testRegisterToolWindowUnuseCaseTwo() {
        try {
            ToolWindow toolWindow = toolWindowManager.registerToolWindow(
                    "id", "title", null, null, ToolWindowAnchor.LEFT
            );
            fail("Try body must throws a RuntimeException");
        } catch (Exception e) {
        }
    }

}
