#!/bin/bash

set -e
set -x

if [[ -e /projects/reblocks ]]; then
    echo 'Please, run docker container with option -v `pwd`:/projects/reblocks'
    cd /projects/reblocks
fi

qlot install
qlot exec ros install rove
qlot exec ros install cl-info
qlot exec ros run -e '(progn (ql:quickload :reblocks-test) (uiop:quit 0))'
qlot exec cl-info hamcrest rove dissect reblocks
qlot exec rove reblocks-test.asd
