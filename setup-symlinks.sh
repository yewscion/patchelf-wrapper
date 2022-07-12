#!/bin/sh

ln -sv Changelog.md ChangeLog
ln -sv README.md README
ln -sv LICENSE COPYING
ln -sv AUTHORS THANKS
rm -v ./setup-symlinks.sh
