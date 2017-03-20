(in-package :cl-user)

(defpackage cl-hipchat.util-test
  (:use cl prove cl-hipchat.util))
(in-package cl-hipchat.util-test)

(plan 9)


(ok (in-range-p 1 1 1))
(ok (in-range-p 5 0 10))
(ok (not (in-range-p 10 0 5)))
(ok (not (in-range-p 0 1 10)))


(is (keyword-to-lowercase-string :foo) "foo")
(isnt (keyword-to-lowercase-string :foo) "FOO")


(is (append-query-params "index.html" '(("foo" . 1)))
    "index.html?foo=1")

(is (append-query-params "index.html" '(("foo" . 1)
                                        ("bar" . "hello")
                                        ("zot" . "world")))
    "index.html?foo=1&bar=hello&zot=world")


(is (room-resource "A Test Room")
    "room/A%20Test%20Room")


(finalize)