#!/usr/bin/env bash

echo "===================================="
echo "Requirement: sudo"
echo "===================================="



# Must in sudo mode


BASEDIR=$(dirname "$0")

sudo systemctl is-active --quiet restbatch.service && sudo systemctl stop restbatch.service

set -e

sudo true

sudo cp "$BASEDIR/restbatch.service" /lib/systemd/system/restbatch.service
sudo chmod 0644 /lib/systemd/system/restbatch.service
sudo cp "$BASEDIR/restbatch.conf" /etc/default/restbatch.conf
sudo chmod 0644 /etc/default/restbatch.conf
sudo cp "$BASEDIR/restbatch.sh" /usr/sbin/restbatch
sudo chmod 0755 /usr/sbin/restbatch

sudo systemctl enable restbatch.service
sudo systemctl start restbatch.service

echo "Service restbatch has been added. To start the service, use"
echo "  sudo systemctl start restbatch.service"
echo ""
echo "To stop the service, enter"
echo "  sudo systemctl stop restbatch.service"
echo ""
echo "To check the service status, enter"
echo "  restbatch monitor"
echo ""
echo "The settings file is at /etc/default/restbatch.conf"
echo ""





