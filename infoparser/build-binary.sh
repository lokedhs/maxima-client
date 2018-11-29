#!/bin/sh

sbcl --dynamic-space-size 4096 --non-interactive --disable-debugger --eval '(ql:quickload "infoparser")' --eval '(sb-ext:save-lisp-and-die "maxima-parser.bin" :toplevel #'"'"'infoparser::resolve-example-code-external :executable t)'
