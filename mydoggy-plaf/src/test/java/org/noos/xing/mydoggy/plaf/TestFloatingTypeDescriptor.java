package org.noos.xing.mydoggy.plaf;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.fail;

import java.awt.Component;

import javax.swing.JFrame;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.noos.xing.mydoggy.FloatingTypeDescriptor;
import org.noos.xing.mydoggy.ToolWindowManager;
import org.noos.xing.mydoggy.ToolWindowType;

/**
 * @author Angelo De Caro (angelo.decaro@gmail.com)
 */
class TestFloatingTypeDescriptor {

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
	void testFloatingTypeDescriptor() {
        assertNotNull(toolWindowManager.getTypeDescriptorTemplate(ToolWindowType.FLOATING));
    }

	@Test
	void testFloatingTypeDescriptorLocation() {
        FloatingTypeDescriptor descriptor = (FloatingTypeDescriptor) toolWindowManager.getTypeDescriptorTemplate(ToolWindowType.FLOATING);
        descriptor.setLocation(10,20);

        descriptor = (FloatingTypeDescriptor) toolWindowManager.getTypeDescriptorTemplate(ToolWindowType.FLOATING);
        assertEquals(10, descriptor.getLocation().x);
        assertEquals(20, descriptor.getLocation().y);
    }

	@Test
	void testFloatingTypeDescriptorModal() {
        FloatingTypeDescriptor descriptor = (FloatingTypeDescriptor) toolWindowManager.getTypeDescriptorTemplate(ToolWindowType.FLOATING);
        descriptor.setModal(true);

        descriptor = (FloatingTypeDescriptor) toolWindowManager.getTypeDescriptorTemplate(ToolWindowType.FLOATING);
        assertEquals(true, descriptor.isModal());

        descriptor = (FloatingTypeDescriptor) toolWindowManager.getTypeDescriptorTemplate(ToolWindowType.FLOATING);
        descriptor.setModal(false);

        descriptor = (FloatingTypeDescriptor) toolWindowManager.getTypeDescriptorTemplate(ToolWindowType.FLOATING);
        assertEquals(false, descriptor.isModal());
    }

	@Test
	void testFloatingTypeDescriptorSize() {
        FloatingTypeDescriptor descriptor = (FloatingTypeDescriptor) toolWindowManager.getTypeDescriptorTemplate(ToolWindowType.FLOATING);
        descriptor.setSize(100,200);

        descriptor = (FloatingTypeDescriptor) toolWindowManager.getTypeDescriptorTemplate(ToolWindowType.FLOATING);
        assertEquals(100, descriptor.getSize().width);
        assertEquals(200, descriptor.getSize().height);
    }

	@Test
	void testFloatingTypeDescriptorTransparentDelay() {
        FloatingTypeDescriptor descriptor = (FloatingTypeDescriptor) toolWindowManager.getTypeDescriptorTemplate(ToolWindowType.FLOATING);
        descriptor.setTransparentDelay(1000);

        descriptor = (FloatingTypeDescriptor) toolWindowManager.getTypeDescriptorTemplate(ToolWindowType.FLOATING);
        assertEquals(1000, descriptor.getTransparentDelay());
    }

	@Test
	void testFloatingTypeDescriptorTransparentMode() {
        FloatingTypeDescriptor descriptor = (FloatingTypeDescriptor) toolWindowManager.getTypeDescriptorTemplate(ToolWindowType.FLOATING);
        descriptor.setTransparentMode(false);

        descriptor = (FloatingTypeDescriptor) toolWindowManager.getTypeDescriptorTemplate(ToolWindowType.FLOATING);
        assertEquals(false, descriptor.isTransparentMode());

        descriptor = (FloatingTypeDescriptor) toolWindowManager.getTypeDescriptorTemplate(ToolWindowType.FLOATING);
        descriptor.setTransparentMode(true);

        descriptor = (FloatingTypeDescriptor) toolWindowManager.getTypeDescriptorTemplate(ToolWindowType.FLOATING);
        assertEquals(true, descriptor.isTransparentMode());
    }

	@Test
	void testFloatingTypeDescriptorTransparentRatio() {
        FloatingTypeDescriptor descriptor = (FloatingTypeDescriptor) toolWindowManager.getTypeDescriptorTemplate(ToolWindowType.FLOATING);
        descriptor.setTransparentRatio(0.5f);

        descriptor = (FloatingTypeDescriptor) toolWindowManager.getTypeDescriptorTemplate(ToolWindowType.FLOATING);
        assertEquals(0.5f, descriptor.getTransparentRatio());
    }

	@Test
	void testFloatingTypeDescriptorTransparentRatioUnuseCase() {
        FloatingTypeDescriptor descriptor = (FloatingTypeDescriptor) toolWindowManager.getTypeDescriptorTemplate(ToolWindowType.FLOATING);
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
