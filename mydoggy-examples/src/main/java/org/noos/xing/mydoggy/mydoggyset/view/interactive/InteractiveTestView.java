package org.noos.xing.mydoggy.mydoggyset.view.interactive;

import info.clearthought.layout.TableLayout;
import org.noos.xing.mydoggy.ToolWindowManager;
import org.noos.xing.mydoggy.itest.InteractiveTest;
import org.noos.xing.mydoggy.itest.InteractiveTestRunner;
import org.noos.xing.mydoggy.itest.impl.SingleThreadInteractiveTestRunner;
import org.noos.xing.mydoggy.mydoggyset.view.interactive.tests.InteractiveToolVisisbleTest;
import org.noos.xing.mydoggy.mydoggyset.view.interactive.tests.InteractiveDragTest;
import org.noos.xing.mydoggy.mydoggyset.view.interactive.tests.InteractiveSimpleTest;
import org.noos.xing.mydoggy.plaf.ui.cmp.border.LineBorder;
import org.noos.xing.yasaf.plaf.view.ComponentView;
import org.noos.xing.yasaf.plaf.view.MapViewContext;
import org.noos.xing.yasaf.view.View;
import org.noos.xing.yasaf.view.ViewContext;
import org.noos.xing.yasaf.view.ViewContextChangeListener;
import org.noos.xing.yasaf.view.event.ViewContextChangeEvent;

import javax.swing.*;
import javax.swing.text.html.HTMLEditorKit;
import java.awt.*;
import java.awt.event.ItemListener;
import java.awt.event.ItemEvent;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

/**
 * @author Angelo De Caro (angelo.decaro@gmail.com)
 */
public class InteractiveTestView implements View {
    protected ToolWindowManager toolWindowManager;
    protected JFrame frame;

    public InteractiveTestView(JFrame frame, ToolWindowManager toolWindowManager) {
        this.frame = frame;
        this.toolWindowManager = toolWindowManager;
    }

    public Component getComponent() {
        ViewContext viewContext = new MapViewContext();
        viewContext.put(ToolWindowManager.class, toolWindowManager);
        viewContext.put(JFrame.class, frame);

        JPanel panel = new JPanel();
        panel.setLayout(new TableLayout(new double[][]{{-1}, {20,3,-1}}));
        panel.add(new TestSelectorPanel(viewContext).getComponent(),
                  "0,0,FULL,FULL");
        panel.add(new TestDescriptionPanel(viewContext).getComponent(),
                  "0,2,FULL,FULL");

        return panel;
    }

    public class TestDescriptionPanel extends ComponentView {
        private JEditorPane editorPane;

        public TestDescriptionPanel(ViewContext viewContext) {
            super(viewContext);
        }

        protected Component initComponent() {
            JPanel panel = new JPanel(new TableLayout(new double[][]{{-1},{-1}}));
            panel.setBorder(new LineBorder(Color.DARK_GRAY));

            this.editorPane = new JEditorPane();
            editorPane.setEditorKit(new HTMLEditorKit());
            editorPane.setEditable(false);
            editorPane.setOpaque(false);

            panel.add(new JScrollPane(editorPane), "0,0,FULL,FULL");

            return panel;
        }

        protected void initListeners() {
            viewContext.addViewContextChangeListener(InteractiveTest.class, new ViewContextChangeListener() {
                public void contextChange(ViewContextChangeEvent evt) {
                    InteractiveTest interactiveTest = (InteractiveTest) evt.getNewValue();
                    editorPane.setText(interactiveTest.getDescription());
                }
            });
        }
    }

    public class TestSelectorPanel extends ComponentView {
        protected JComboBox tests;

        public TestSelectorPanel(ViewContext viewContext) {
            super(viewContext);
        }

        protected Component initComponent() {
            JPanel panel = new JPanel(new TableLayout(new double[][]{{150,3,-1,3, 100},{-1}}));

            JFrame frame = viewContext.get(JFrame.class);

            tests = new JComboBox(new Object[]{
                    new InteractiveToolVisisbleTest(frame),
                    new InteractiveDragTest(frame),
                    new InteractiveSimpleTest(frame)
            });
            tests.addItemListener(new ItemListener() {
                public void itemStateChanged(ItemEvent e) {
                    InteractiveTest interactiveTest = (InteractiveTest) e.getItem();
                    viewContext.put(InteractiveTest.class, interactiveTest);
                }
            });
            tests.setSelectedIndex(0);

            JButton execute = new JButton("Execute");
            execute.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    InteractiveTest interactiveTest = viewContext.get(InteractiveTest.class);

                    InteractiveTestRunner runner = new SingleThreadInteractiveTestRunner();
                    runner.addInteractiveTest(interactiveTest);
                    runner.run();
                }
            });

            panel.add(new JLabel("Interactive Test : "), "0,0,r,FULL");
            panel.add(tests, "2,0,FULL,FULL");
            panel.add(execute , "4,0,FULL,FULL");

            return panel;
        }

        protected void onVisible() {
            tests.setSelectedItem(null);
            tests.setSelectedIndex(0);
        }
    }

}