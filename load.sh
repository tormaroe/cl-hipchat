
# Load debugging environment for cl-hipchat
# ====================================================================
#
# Assumes cl-hipchat can be loaded using quicklisp.
#
# Will enable verbous header output for all API requests.
#
# The following files must exist:
# 		"~/.cl-hipchat-dev-user"
#		"~/.cl-hipchat-dev-token"
#
# (make sure files only contains the user identifier / the token, 
#  and no newline)

sbcl --eval '(ql:quickload :cl-hipchat)' \
     --eval '(setf drakma:*header-stream* *standard-output*)' \
     --load load-dev-env.lisp

