package org.noos.xing.mydoggy.tutorialset.basic;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;

import javax.swing.AbstractAction;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JTree;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;

import org.noos.xing.mydoggy.Content;
import org.noos.xing.mydoggy.ContentManager;
import org.noos.xing.mydoggy.ContentManagerUIListener;
import org.noos.xing.mydoggy.DockedTypeDescriptor;
import org.noos.xing.mydoggy.FloatingTypeDescriptor;
import org.noos.xing.mydoggy.RepresentativeAnchorDescriptor;
import org.noos.xing.mydoggy.SlidingTypeDescriptor;
import org.noos.xing.mydoggy.TabbedContentManagerUI;
import org.noos.xing.mydoggy.TabbedContentUI;
import org.noos.xing.mydoggy.ToolWindow;
import org.noos.xing.mydoggy.ToolWindowActionHandler;
import org.noos.xing.mydoggy.ToolWindowAnchor;
import org.noos.xing.mydoggy.ToolWindowManager;
import org.noos.xing.mydoggy.ToolWindowType;
import org.noos.xing.mydoggy.event.ContentManagerUIEvent;
import org.noos.xing.mydoggy.plaf.MyDoggyToolWindowManager;

import info.clearthought.layout.TableLayout;

public class TutorialSet6 {
    private JFrame frame;
    private ToolWindowManager toolWindowManager;


    protected void run() {
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                setUp();
                start();
            }
        });
    }

    protected void setUp() {
        initComponents();
        initToolWindowManager();
    }

    protected void start() {
        // Activate "Debug" Tool
        ToolWindow debugTool = toolWindowManager.getToolWindow("Debug");
        debugTool.setActive(true);

        frame.setVisible(true);
    }

    protected void initComponents() {
        // Init the frame
        this.frame = new JFrame("Sample App...");
        this.frame.setSize(640, 480);
        this.frame.setLocation(100, 100);
        this.frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);

        // Create a simple JMenuBar
        JMenuBar menuBar = new JMenuBar();
        JMenu fileMenu = new JMenu("File");
        JMenuItem exitMenuItem = new JMenuItem("Exit");
        exitMenuItem.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                frame.setVisible(false);
                frame.dispose();
            }
        });
        fileMenu.add(exitMenuItem);
        menuBar.add(fileMenu);
        this.frame.setJMenuBar(menuBar);

        // Set a layout manager. I love TableLayout. It's powerful.
        this.frame.getContentPane().setLayout(new TableLayout(new double[][]{{0, -1, 0}, {0, -1, 0}}));

        // Store (on close) and load (on start) the toolwindow manager workspace.
        this.frame.addWindowListener(new WindowAdapter() {
            @Override
            public void windowOpened(WindowEvent e) {
                File workspaceFile = new File("workspace.xml");
                if (workspaceFile.exists()) {
                    try (FileInputStream inputStream = new FileInputStream("workspace.xml")) {
                        toolWindowManager.getPersistenceDelegate().apply(inputStream);
                    } catch (Exception e1) {
                        e1.printStackTrace();
                    }
                }
            }

            @Override
            public void windowClosing(WindowEvent e) {
                try (FileOutputStream output = new FileOutputStream("workspace.xml")) {
                    toolWindowManager.getPersistenceDelegate().save(output);
                } catch (Exception e1) {
                    e1.printStackTrace();
                }
            }
        });
    }

    protected void initToolWindowManager() {
        // Create a new instance of MyDoggyToolWindowManager passing the frame.
        MyDoggyToolWindowManager myDoggyToolWindowManager = new MyDoggyToolWindowManager();
        this.toolWindowManager = myDoggyToolWindowManager;

        // Register a Tool.
        toolWindowManager.registerToolWindow("Debug",                      // Id
                                             "Debug Tool",                 // Title
                                             null,                         // Icon
                                             new JButton("Debug Tool"),    // Component
                                             ToolWindowAnchor.LEFT);       // Anchor

        setupDebugTool();

        // Made all tools available
        for (ToolWindow window : toolWindowManager.getToolWindows()) {
            window.setAvailable(true);
        }

        initContentManager();

        // Add myDoggyToolWindowManager to the frame. MyDoggyToolWindowManager is an extension of a JPanel
        this.frame.getContentPane().add(myDoggyToolWindowManager, "1,1,");
    }


    protected void setupDebugTool() {
        ToolWindow debugTool = toolWindowManager.getToolWindow("Debug");

        // RepresentativeAnchorDescriptor
        RepresentativeAnchorDescriptor representativeAnchorDescriptor = debugTool.getRepresentativeAnchorDescriptor();
        representativeAnchorDescriptor.setPreviewEnabled(true);
        representativeAnchorDescriptor.setPreviewDelay(1500);
        representativeAnchorDescriptor.setPreviewTransparentRatio(0.4f);

        // DockedTypeDescriptor
        DockedTypeDescriptor dockedTypeDescriptor = (DockedTypeDescriptor) debugTool.getTypeDescriptor(ToolWindowType.DOCKED);
        dockedTypeDescriptor.setDockLength(300);
        dockedTypeDescriptor.setPopupMenuEnabled(true);
        JMenu toolsMenu = dockedTypeDescriptor.getToolsMenu();
        toolsMenu.add(new AbstractAction("Hello World!!!") {
            @Override
            public void actionPerformed(ActionEvent e) {
                JOptionPane.showMessageDialog(frame, "Hello World!!!");
            }
        });
        dockedTypeDescriptor.setToolWindowActionHandler(new ToolWindowActionHandler() {
            @Override
            public void onHideButtonClick(ToolWindow toolWindow) {
                JOptionPane.showMessageDialog(frame, "Hiding...");
                toolWindow.setVisible(false);
            }
        });
        dockedTypeDescriptor.setAnimating(true);

        // SlidingTypeDescriptor
        SlidingTypeDescriptor slidingTypeDescriptor = (SlidingTypeDescriptor) debugTool.getTypeDescriptor(ToolWindowType.SLIDING);
        slidingTypeDescriptor.setEnabled(false);
        slidingTypeDescriptor.setTransparentMode(true);
        slidingTypeDescriptor.setTransparentRatio(0.8f);
        slidingTypeDescriptor.setTransparentDelay(0);
        slidingTypeDescriptor.setAnimating(true);

        // FloatingTypeDescriptor
        FloatingTypeDescriptor floatingTypeDescriptor = (FloatingTypeDescriptor) debugTool.getTypeDescriptor(ToolWindowType.FLOATING);
        floatingTypeDescriptor.setEnabled(true);
        floatingTypeDescriptor.setLocation(150,200);
        floatingTypeDescriptor.setSize(320,200);
        floatingTypeDescriptor.setModal(false);
        floatingTypeDescriptor.setTransparentMode(true);
        floatingTypeDescriptor.setTransparentRatio(0.2f);
        floatingTypeDescriptor.setTransparentDelay(1000);
        floatingTypeDescriptor.setAnimating(true);
    }

    protected void initContentManager() {
        JTree treeContent = new JTree();

        ContentManager contentManager = toolWindowManager.getContentManager();
        Content content = contentManager.addContent("Tree Key",
                                                    "Tree Title",
                                                    null,      // An icon
                                                    treeContent);
        content.setToolTipText("Tree tip");

        setupContentManagerUI();
    }

    protected void setupContentManagerUI() {
        // By default the content manager ui is a TabbedContentManagerUI<TabbedContentUI> instance. 
        TabbedContentManagerUI<TabbedContentUI> contentManagerUI = (TabbedContentManagerUI<TabbedContentUI>) toolWindowManager.getContentManager().getContentManagerUI();
        contentManagerUI.setShowAlwaysTab(true);
        contentManagerUI.setTabPlacement(TabbedContentManagerUI.TabPlacement.BOTTOM);
        contentManagerUI.addContentManagerUIListener(new ContentManagerUIListener() {
            @Override
            public boolean contentUIRemoving(ContentManagerUIEvent event) {
                return JOptionPane.showConfirmDialog(frame, "Are you sure?") == JOptionPane.OK_OPTION;
            }

            @Override
            public void contentUIDetached(ContentManagerUIEvent event) {
                JOptionPane.showMessageDialog(frame, "Hello World!!!");
            }
        });

        TabbedContentUI contentUI = contentManagerUI.getContentUI(toolWindowManager.getContentManager().getContent(0));

        contentUI.setCloseable(true);
        contentUI.setDetachable(true);
        contentUI.setTransparentMode(true);
        contentUI.setTransparentRatio(0.7f);
        contentUI.setTransparentDelay(1000);
    }

    public static void main(String[] args) {
        TutorialSet6 test = new TutorialSet6();
        try {
            test.run();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}