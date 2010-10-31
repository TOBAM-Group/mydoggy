package org.noos.xing.mydoggy.itest.impl;

import org.noos.xing.mydoggy.itest.ComponentAdapter;
import org.noos.xing.mydoggy.itest.impl.ui.JBalloonTip;

import javax.swing.*;
import java.awt.*;
import java.awt.event.InputEvent;
import java.awt.geom.PathIterator;
import java.awt.image.BufferedImage;

/**
 * @author Angelo De Caro (angelo.decaro@gmail.com)
 */
public class RobotComponentAdapter implements ComponentAdapter {
    protected Robot robot;
    protected Component component;
    protected MouseButton lastMouseButtonPressed;
    protected JBalloonTip balloonTip;
    protected int delay;

    public RobotComponentAdapter(Robot robot, Component component, int delay) {
        this.robot = robot;
        this.component = component;
        this.delay = delay;
    }

    public void setDelay(int delay) {
        this.delay = delay;
    }

    public ComponentAdapter moveTo() {
        return moveTo(5,5);
    }

    public ComponentAdapter moveToCenter() {
        return moveTo(component.getWidth() / 2, component.getHeight() / 2);
    }

    public ComponentAdapter moveTo(int x, int y) {
        Point to = new Point(x, y);
        SwingUtilities.convertPointToScreen(to, component);
        ComponentAdapter componentAdapter = moveToInternal(to);
        doDelay();
        return componentAdapter;
    }

    public ComponentAdapter moveTo(Location location) {
        switch (location) {
            case LEFT:
                return moveTo(component.getWidth() - 10, component.getHeight() / 2);
            case BOTTOM:
                return moveTo(component.getWidth() / 2, component.getHeight() - 10);
            default:
                return moveToCenter();
        }
    }

    public ComponentAdapter move(Shape shape) {
        PathIterator iterator = shape.getPathIterator(null);
        double[] values = new double[6];

        while (!iterator.isDone()) {
            int type = iterator.currentSegment(values);
//            System.out.println(type);
            switch (type) {
                case PathIterator.SEG_MOVETO :
                    moveToInternal(new Point((int) values[0], (int) values[1]));
                    break;
                case PathIterator.SEG_LINETO :
                    moveToInternal(new Point((int) values[0], (int) values[1]));
                    break;
                case PathIterator.SEG_CUBICTO :
                    moveToInternal(new Point((int) values[0], (int) values[1]));
                    moveToInternal(new Point((int) values[2], (int) values[3]));
                    moveToInternal(new Point((int) values[4], (int) values[5]));
                break;
            }
            robot.delay(5);
            iterator.next();
        }

        doDelay();

        return this;
    }

    public ComponentAdapter press(MouseButton mouseButton) {
        lastMouseButtonPressed = mouseButton;
        switch (mouseButton) {
            case LEFT:
                robot.mousePress(InputEvent.BUTTON1_MASK);
                break;
            case CENTER:
                robot.mousePress(InputEvent.BUTTON2_MASK);
                break;
            case RIGHT:
                robot.mousePress(InputEvent.BUTTON3_MASK);
                break;
        }

        doDelay();

        return this;
    }

    public ComponentAdapter release() {
        return release(lastMouseButtonPressed);
    }

    public ComponentAdapter release(MouseButton mouseButton) {
        switch (mouseButton) {
            case LEFT:
                robot.mouseRelease(InputEvent.BUTTON1_MASK);
                break;
            case CENTER:
                robot.mouseRelease(InputEvent.BUTTON2_MASK);
                break;
            case RIGHT:
                robot.mouseRelease(InputEvent.BUTTON3_MASK);
                break;
        }
        doDelay();
        return this;
    }

    public ComponentAdapter click(MouseButton mouseButton) {
        int oldDelay = delay;
        delay = 0;
        try {
            press(mouseButton);
            release();
        } finally {
            delay = oldDelay;
            doDelay();
        }
        return this;
    }

    public ComponentAdapter wheel(int amount) {
        robot.mouseWheel(amount);
        doDelay();
        return this;
    }

    public ComponentAdapter showTip(String message) {
        if (balloonTip == null)
            balloonTip = new JBalloonTip(null);

        Window windowAncestor = SwingUtilities.getWindowAncestor(component);
        if (windowAncestor instanceof RootPaneContainer) {
            balloonTip.setRootPaneContainer((RootPaneContainer) windowAncestor);
            balloonTip.setText(message);
            Point point = MouseInfo.getPointerInfo().getLocation();
            SwingUtilities.convertPointFromScreen(point, windowAncestor);
            balloonTip.show(point.x, point.y);
            doDelay();
            balloonTip.setVisible(false);
        }
        return this;
    }

    public BufferedImage shot() {
        return robot.createScreenCapture(component.getBounds());
    }


    protected ComponentAdapter moveToInternal(Point to) {
        Point from = MouseInfo.getPointerInfo().getLocation();

        int x0 = from.x;
        int y0 = from.y;

        int x1 = to.x;
        int y1 = to.y;

        int dx = x1 - x0;
        int dy = y1 - y0;

        robot.mouseMove(x0, y0);
        if (Math.abs(dx) > Math.abs(dy)) {          // slope < 1
            float m = (float) dy / (float) dx;      // compute slope
            float b = y0 - m * x0;
            dx = (dx < 0) ? -5 : 5;
            while ((dx < 0 && x0 >= x1) || (dx > 0 && x0 <= x1)) {
                x0 += dx;
                robot.mouseMove(x0, Math.round(m * x0 + b));
                robot.delay(10);
            }
        } else if (dy != 0) {                        // slope >= 1
            float m = (float) dx / (float) dy;      // compute slope
            float b = x0 - m * y0;
            dy = (dy < 0) ? -5 : 5;
            while ((dy < 0 && y0 >= y1) || (dy > 0 && y0 <= y1)) {
                y0 += dy;
                robot.mouseMove(Math.round(m * y0 + b), y0);
                robot.delay(10);
            }
        }
        robot.mouseMove(to.x, to.y);
        
        return this;
    }

    protected void doDelay() {
        if (delay > 0)
            robot.delay(delay);
    }

}