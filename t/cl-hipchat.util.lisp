(in-package :cl-user)

(defpackage cl-hipchat.util-test
  (:use cl prove cl-hipchat.util))
(in-package cl-hipchat.util-test)

(plan nil)

(ok (not (find 4 '(1 2 3))))
(is 4 4)
(isnt 1 #\1)

(finalize)