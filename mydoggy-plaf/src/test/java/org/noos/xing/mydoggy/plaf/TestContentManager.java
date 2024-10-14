package org.noos.xing.mydoggy.plaf;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.awt.Component;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.JButton;
import javax.swing.JFrame;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.noos.xing.mydoggy.Content;
import org.noos.xing.mydoggy.ContentManager;
import org.noos.xing.mydoggy.ToolWindowManager;

/**
 * @author Angelo De Caro (angelo.decaro@gmail.com)
 */
public class TestContentManager {

    private JFrame frame;
    private ToolWindowManager toolWindowManager;

	@BeforeEach
    protected void setUp() throws Exception {
        frame = new JFrame("test");
        toolWindowManager = new MyDoggyToolWindowManager();
        frame.add((Component) toolWindowManager);
    }

    protected void tearDown() throws Exception {
        frame.dispose();
    }

    public void testContentManager() {
        assertNotNull(toolWindowManager.getContentManager());
    }

	@Test
    public void testAddContentOne() {
        ContentManager contentManager = toolWindowManager.getContentManager();
        Content content = contentManager.addContent("key", "title", null, new JButton("Hello World!!!"));

        assertNotNull(content);
        assertEquals(contentManager.getContentCount(), 1);

        content = contentManager.getContent(0);

        assertEquals(content.getTitle(), "title");
        assertEquals(content.getIcon(), null);
        assertEquals(content.getToolTipText(), null);
        assertTrue(content.getComponent() instanceof JButton);

        content = contentManager.getContent("key");

        assertEquals(content.getTitle(), "title");
        assertEquals(content.getIcon(), null);
        assertEquals(content.getToolTipText(), null);
        assertTrue(content.getComponent() instanceof JButton);
    }

	@Test
    public void testAddContentTwo() {
        ContentManager contentManager = toolWindowManager.getContentManager();
        Content content = contentManager.addContent("key", "title", null, new JButton("Hello World!!!"), "tip");

        assertNotNull(content);
        assertEquals(contentManager.getContentCount(), 1);

        content = contentManager.getContent(0);

        assertEquals(content.getTitle(), "title");
        assertEquals(content.getIcon(), null);
        assertEquals(content.getToolTipText(), "tip");
        assertTrue(content.getComponent() instanceof JButton);
    }

	@Test
    public void testAlias() {
        ContentManager contentManager = toolWindowManager.getContentManager();
        Content content = contentManager.addContent("key", "title", null, new JButton("Hello World!!!"), "tip");

        assertNotNull(contentManager.getAliases(content));
        assertEquals(0, contentManager.getAliases(content).length);

        contentManager.addAlias(content, ContentManager.class);

        assertEquals(1, contentManager.getAliases(content).length);

        Content contentByAlias = contentManager.getContent(ContentManager.class);
        assertEquals(content, contentByAlias);

        contentManager.removeAlias(ContentManager.class);

        contentByAlias = contentManager.getContent(ContentManager.class);
        assertNull(contentByAlias);
    }


	@Test
    public void test1() {
        ContentManager contentManager = toolWindowManager.getContentManager();

        Content ctn1 = contentManager.addContent("ctn1", "ctn1", null, new JButton("ctn1"));
        PropertyChangeListenerTracer ctn1Tracer = new PropertyChangeListenerTracer();
        ctn1.addPropertyChangeListener(ctn1Tracer);

        Content ctn2 = contentManager.addContent("ctn2", "ctn2", null, new JButton("ctn2"));
        PropertyChangeListenerTracer ctn2Tracer = new PropertyChangeListenerTracer();
        ctn2.addPropertyChangeListener(ctn2Tracer);

        Content ctn3 = contentManager.addContent("ctn3", "ctn3", null, new JButton("ctn3"));
        PropertyChangeListenerTracer ctn3Tracer = new PropertyChangeListenerTracer();
        ctn3.addPropertyChangeListener(ctn3Tracer);

        contentManager.removeContent(ctn1);
        System.out.printf("dd");
    }


    public class PropertyChangeListenerTracer implements PropertyChangeListener {
        protected Map<String, List<PropertyChangeEvent>> eventMap;

        public PropertyChangeListenerTracer() {
            eventMap = new HashMap<>();
        }

        @Override
		public void propertyChange(PropertyChangeEvent evt) {
            String propertyName = evt.getPropertyName();
            List<PropertyChangeEvent> events = eventMap.get(propertyName);
            if (events == null) {
                events = new ArrayList<>();
                eventMap.put(propertyName, events);
            }
            events.add(evt);
        }
    }
}
