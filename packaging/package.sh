#!/bin/bash

# As of right now simply creates the Debian package. Plans for the future include
# creating packages for other distributions and maybe BSD as well.
#
# `fpm` to be installed (`gem install fpm`).

# Stop the script if a command fails.
set -e

# Set the temporary package directory variable
TEMPORARY_PKG_DIR=package

# Extract the version from package.yaml
VERSION=$(awk '/^version:/ {print $2}' package.yaml)

# Build the Haskell project and copy the binaries
stack build --copy-bins --local-bin-path ./bin

# Create necessary directories in the temporary package directory
mkdir -p $TEMPORARY_PKG_DIR/usr/local/bin
mkdir -p $TEMPORARY_PKG_DIR/etc/phorum
mkdir -p $TEMPORARY_PKG_DIR/etc/systemd/system

# Copy the built binary and configuration files to the temporary package directory
cp ./bin/phorum-exe $TEMPORARY_PKG_DIR/usr/local/bin/phorum
cp ./packaging/config.toml $TEMPORARY_PKG_DIR/etc/phorum/
cp ./packaging/phorum.service $TEMPORARY_PKG_DIR/etc/systemd/system/

# Run fpm to create the Debian package.
#
# NOTE: I'm not sure why I use both --deb-systemd AND include the .service, .socket files
# like below.
fpm -s dir -t deb -n phorum -v $VERSION \
  --description "Phorum Gopher server" \
  --maintainer "someodd <someodd@pm.me>" \
  --url "http://www.someodd.zip/showcase/phorum" \
  --license "GPL" \
  --deb-systemd $TEMPORARY_PKG_DIR/etc/systemd/system/phorum.service \
  --before-install packaging/pre-install.sh \
  --after-install packaging/post-install.sh \
  -C $TEMPORARY_PKG_DIR \
  usr/local/bin/phorum \
  etc/systemd/system/phorum.service \
  etc/phorum/config.toml

# Clean up the temporary package directory
rm -rf $TEMPORARY_PKG_DIR
rm -rf ./bin
