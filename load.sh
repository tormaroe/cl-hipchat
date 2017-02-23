
# Load debugging environment for cl-hipchat
# ====================================================================
#
# Assumes cl-hipchat can be loaded using quicklisp.
#
# Will enable verbous header output for all API requests.
#
# Will fetch and set HipChat Auth token from file "~/.cl-hipchat-dev"
# (make sure file only contains the token, and no newline)

sbcl --eval '(ql:quickload :cl-hipchat)' \
     --eval '(setf drakma:*header-stream* *standard-output*)' \
     --eval '(setf cl-hipchat.config:*AUTH-TOKEN* (with-open-file (stream "~/.cl-hipchat-dev") (let ((data (make-string (file-length stream)))) (read-sequence data stream) data)))'
