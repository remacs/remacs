#!/bin/sh

mkdir -p depends
[ -f depends/dash.el ] || \
    wget -O depends/dash.el https://raw.githubusercontent.com/magnars/dash.el/master/dash.el
[ -f depends/s.el ] || \
    wget -O depends/s.el https://raw.githubusercontent.com/magnars/s.el/master/s.el
