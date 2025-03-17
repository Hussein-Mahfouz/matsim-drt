#!/bin/bash

# Define installation directory
MAVEN_VERSION=3.8.4  
INSTALL_DIR="$HOME/maven"
MAVEN_URL="https://downloads.apache.org/maven/maven-3/$MAVEN_VERSION/binaries/apache-maven-$MAVEN_VERSION-bin.tar.gz"

# Create the install directory
mkdir -p "$INSTALL_DIR"

# Download and extract Maven
echo "Downloading Maven $MAVEN_VERSION..."
wget "$MAVEN_URL" -O /tmp/apache-maven.tar.gz

echo "Extracting Maven..."
tar -xzf /tmp/apache-maven.tar.gz -C "$INSTALL_DIR" --strip-components=1

# Clean up
rm /tmp/apache-maven.tar.gz

# Update environment variables
echo "export MAVEN_HOME=$INSTALL_DIR" >> ~/.bashrc
echo "export PATH=\$MAVEN_HOME/bin:\$PATH" >> ~/.bashrc

# Apply changes
source ~/.bashrc

echo "Maven installation completed!"
mvn -version  # Check installation

