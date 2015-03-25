#!/bin/sh

# Load BLD-SSWEB, start server, and provide SWANK interface for Emacs

sbcl --eval "(ql:quickload :bld-ssweb)" \
     --eval "(in-package :bld-ssweb)" \
     --eval "(start *sail-acceptor*)" \
     --eval "(ql:quickload :swank)" \
     --eval "(swank:create-server :port 4005 :dont-close t)"
