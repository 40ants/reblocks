#!/bin/bash

set -e
set -x

if [[ -e /projects/weblocks ]]; then
    echo 'Please, run docker container with option -v `pwd`:/projects/weblocks'
    cd /projects/weblocks
fi

qlot install
qlot exec ros install rove
qlot exec ros install cl-info
qlot exec ros run -e '(progn (ql:quickload :weblocks-test) (uiop:quit 0))'
qlot exec cl-info hamcrest rove dissect weblocks
qlot exec rove weblocks-test.asd
