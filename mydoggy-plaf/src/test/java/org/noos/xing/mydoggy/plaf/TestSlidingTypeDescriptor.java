package org.noos.xing.mydoggy.plaf;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.fail;

import java.awt.Component;

import javax.swing.JFrame;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.noos.xing.mydoggy.SlidingTypeDescriptor;
import org.noos.xing.mydoggy.ToolWindowManager;
import org.noos.xing.mydoggy.ToolWindowType;

/**
 * @author Angelo De Caro (angelo.decaro@gmail.com)
 */
public class TestSlidingTypeDescriptor {

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
    public void testSlidingTypeDescriptor() {
        assertNotNull(toolWindowManager.getTypeDescriptorTemplate(ToolWindowType.SLIDING));
    }

	@Test
    public void testSlidingTypeDescriptorTransparentDelay() {
        SlidingTypeDescriptor descriptor = (SlidingTypeDescriptor) toolWindowManager.getTypeDescriptorTemplate(ToolWindowType.SLIDING);
        descriptor.setTransparentDelay(1000);

        descriptor = (SlidingTypeDescriptor) toolWindowManager.getTypeDescriptorTemplate(ToolWindowType.SLIDING);
        assertEquals(1000, descriptor.getTransparentDelay());
    }

	@Test
    public void testSlidingTypeDescriptorTransparentMode() {
        SlidingTypeDescriptor descriptor = (SlidingTypeDescriptor) toolWindowManager.getTypeDescriptorTemplate(ToolWindowType.SLIDING);
        descriptor.setTransparentMode(false);

        descriptor = (SlidingTypeDescriptor) toolWindowManager.getTypeDescriptorTemplate(ToolWindowType.SLIDING);
        assertEquals(false, descriptor.isTransparentMode());

        descriptor = (SlidingTypeDescriptor) toolWindowManager.getTypeDescriptorTemplate(ToolWindowType.SLIDING);
        descriptor.setTransparentMode(true);

        descriptor = (SlidingTypeDescriptor) toolWindowManager.getTypeDescriptorTemplate(ToolWindowType.SLIDING);
        assertEquals(true, descriptor.isTransparentMode());
    }

	@Test
    public void testSlidingTypeDescriptorTransparentRatio() {
        SlidingTypeDescriptor descriptor = (SlidingTypeDescriptor) toolWindowManager.getTypeDescriptorTemplate(ToolWindowType.SLIDING);
        descriptor.setTransparentRatio(0.5f);

        descriptor = (SlidingTypeDescriptor) toolWindowManager.getTypeDescriptorTemplate(ToolWindowType.SLIDING);
        assertEquals(0.5f, descriptor.getTransparentRatio());
    }

	@Test
    public void testSlidingTypeDescriptorTransparentRatioUnuseCase() {
        SlidingTypeDescriptor descriptor = (SlidingTypeDescriptor) toolWindowManager.getTypeDescriptorTemplate(ToolWindowType.SLIDING);
        try {
            descriptor.setTransparentRatio(1.5f);
            fail("Must fail with 1.5f");
        } catch (Exception e) {}

        try {
            descriptor.setTransparentRatio(-0.1f);
            fail("Must fail with -0.1f");
        } catch (Exception e) {}
    }

}
