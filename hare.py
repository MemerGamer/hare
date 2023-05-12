#!/bin/env python3

import sys
from PyQt5.QtWidgets import QApplication, QWidget, QPushButton, QLabel, QFileDialog, QMessageBox
import PyQt5.QtCore as QtCore
import subprocess


class ServerStarter(QWidget):

    def __init__(self):
        super().__init__()

        # Set up the window
        self.setGeometry(200, 200, 300, 150)
        self.setFixedSize(300, 180)
        self.setWindowTitle('Hare')

        # Set up the label
        self.label = QLabel('Click the button to start the server!', self)
        self.label.setAlignment(QtCore.Qt.AlignCenter)
        self.label.move(40, 20)
        # Set up the start button
        self.start_button = QPushButton('Start Server', self)
        self.start_button.move(45, 70)
        self.start_button.clicked.connect(self.start_server)

        # Set up the stop button
        self.stop_button = QPushButton('Stop Server', self)
        self.stop_button.move(154, 70)
        self.stop_button.clicked.connect(self.stop_server)

        # Set up the add folder button
        self.add_folder_button = QPushButton('Add Web Project (folder)', self)
        self.add_folder_button.move(45, 110)
        self.add_folder_button.resize(210, 35)
        self.add_folder_button.clicked.connect(self.add_folder)

        # Show the window
        self.show()

    def start_server(self):
        self.process = subprocess.Popen('runhaskell Main.hs', shell=True)
        self.label.setText('Server started!')
        QMessageBox.about(
            self, "Success", "Server started!")

    def stop_server(self):
        self.process.terminate()
        self.label.setText('Server stopped.')
        QMessageBox.about(
            self, "Success", "Server stopped!")

    def add_folder(self):
        # Open a file dialog to select a folder
        folder_path = QFileDialog.getExistingDirectory(self, 'Select Folder')

        # Add the selected folder to the sites directory
        if folder_path:
            subprocess.Popen(f'cp -r {folder_path} sites/', shell=True)
            self.label.setText(
                f'Folder {folder_path} added to sites directory!')
            # alert with qmessagebox
            QMessageBox.about(
                self, "Success", "Folder added to sites directory!")


if __name__ == '__main__':
    app = QApplication(sys.argv)
    server_starter = ServerStarter()
    sys.exit(app.exec_())
