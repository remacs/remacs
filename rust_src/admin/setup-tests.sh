#!/bin/sh

DASH_TAG=2.13.0
S_TAG=1.12.0

mkdir -p depends
[ -f depends/dash.el ] || \
    wget -O depends/dash.el https://raw.githubusercontent.com/magnars/dash.el/$DASH_TAG/dash.el
[ -f depends/s.el ] || \
    wget -O depends/s.el https://raw.githubusercontent.com/magnars/s.el/$S_TAG/s.el
