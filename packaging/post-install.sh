#!/bin/bash

# Reload systemd manager configuration
systemctl daemon-reload

# Enable and start the phorum service
systemctl enable phorum.service
systemctl start phorum.service
