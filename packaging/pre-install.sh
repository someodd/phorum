#!/bin/bash

# Create a system user and group for the phorum service if they don't exist
if ! id -u phorum &>/dev/null; then
    useradd -r -s /bin/false phorum
fi