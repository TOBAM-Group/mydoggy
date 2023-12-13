package org.noos.xing.mydoggy.mydoggyset.ui;

import org.jdesktop.swingx.JXDatePicker;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Date;

public class DatePicker extends JPanel {

    public DatePicker() {
        final JLabel label = new JLabel();
        label.setText("Choose Date by selecting below.");

        add(label, BorderLayout.NORTH);

        final JXDatePicker datePicker = new JXDatePicker(new Date());
        datePicker.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                label.setText(datePicker.getDate().toString());
            }
        });

        add(datePicker, BorderLayout.CENTER);
    }
}