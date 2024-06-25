#!/bin/bash
sudo systemctl daemon-reload
sudo systemctl enable phorum.socket
sudo systemctl start phorum.socket
sudo systemctl enable phorum.service
sudo systemctl start phorum.service