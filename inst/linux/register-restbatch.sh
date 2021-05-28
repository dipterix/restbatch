#!/usr/bin/env bash

echo "===================================="
echo "Requirement: sudo"
echo "===================================="
# Must in sudo mode


BASEDIR=$(dirname "$0")

systemctl --user is-active --quiet restbatch.service && systemctl --user stop restbatch.service

set -e

mkdir -p ~/.config/systemd/user/
sudo mkdir -p /usr/local/etc/restbatch

sudo cp "$BASEDIR/restbatch.service" ~/.config/systemd/user/restbatch.service
sudo chmod 0644 ~/.config/systemd/user/restbatch.service

sudo cp "$BASEDIR/restbatch.conf" /usr/local/etc/restbatch/restbatch.conf
sudo chmod 0644 /usr/local/etc/restbatch/restbatch.conf

sudo cp "$BASEDIR/settings.yaml" /usr/local/etc/restbatch/settings.yaml
sudo chmod 0644 /usr/local/etc/restbatch/settings.yaml

sudo cp "$BASEDIR/restbatch.sh" /usr/local/sbin/restbatch
sudo chmod 0755 /usr/local/sbin/restbatch

systemctl --user daemon-reload
systemctl --user enable --now restbatch.service
# systemctl --user start restbatch.service

echo "Service restbatch has been added. To start the service, use"
echo "  systemctl --user start restbatch.service"
echo ""
echo "To stop the service, enter"
echo "  systemctl --user stop restbatch.service"
echo ""
echo "To check the service status, enter"
echo "  restbatch monitor"
echo ""
echo "The settings file is stored here /usr/local/etc/restbatch/settings.yaml"
echo ""





