package org.noos.xing.mydoggy.plaf;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.awt.Component;

import javax.swing.JButton;
import javax.swing.JFrame;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.noos.xing.mydoggy.Content;
import org.noos.xing.mydoggy.ContentManager;
import org.noos.xing.mydoggy.ContentManagerListener;
import org.noos.xing.mydoggy.ToolWindowManager;
import org.noos.xing.mydoggy.event.ContentManagerEvent;

/**
 * @author Angelo De Caro (angelo.decaro@gmail.com)
 */
class TestContentManagerListener {

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
	void testAddContent() {
        ContentManager contentManager = toolWindowManager.getContentManager();

        TestCMListener listener = new TestCMListener();
        contentManager.addContentManagerListener(listener);

        Content content = contentManager.addContent("key", "content", null, new JButton("Hello World!!!"));

        assertTrue(listener.isContentAdded());
        assertNotNull(listener.getLastEvent());
        assertEquals(ContentManagerEvent.ActionId.CONTENT_ADDED, listener.getLastEvent().getId());
        assertEquals(content, listener.getLastEvent().getContent());
    }

	@Test
	void testRemoveContent() {
        ContentManager contentManager = toolWindowManager.getContentManager();

        TestCMListener listener = new TestCMListener();
        contentManager.addContentManagerListener(listener);

        Content content = contentManager.addContent("key", "content", null, new JButton("Hello World!!!"));
        assertTrue(contentManager.removeContent(content));

        assertTrue(listener.isContentRemoved());
        assertNotNull(listener.getLastEvent());
        assertEquals(ContentManagerEvent.ActionId.CONTENT_REMOVED, listener.getLastEvent().getId());
        assertEquals(content, listener.getLastEvent().getContent());
    }

    private class TestCMListener implements ContentManagerListener {
        private boolean contentAdded;
        private boolean contentRemoved;
        private boolean contentSelected;

        private ContentManagerEvent lastEvent;


        @Override
		public void contentAdded(ContentManagerEvent event) {
            this.contentAdded = true;
            this.lastEvent = event;
        }

        @Override
		public void contentRemoved(ContentManagerEvent event) {
            this.contentRemoved = true;
            this.lastEvent = event;
        }

        @Override
		public void contentSelected(ContentManagerEvent event) {
            this.contentSelected = true;
            this.lastEvent = event;
        }

        public boolean isContentAdded() {
            return contentAdded;
        }

        public boolean isContentRemoved() {
            return contentRemoved;
        }

        public boolean isContentSelected() {
            return contentSelected;
        }

        public ContentManagerEvent getLastEvent() {
            return lastEvent;
        }

    }

}
