;;;; cl-hipchat.asd

(asdf:defsystem #:cl-hipchat
  :description "Client wrapper library for the HipChat API"
  :author "Torbjørn Marø"
  :license "MIT License"
  :homepage "https://github.com/tormaroe/cl-hipchat"
  :depends-on (#:drakma 
               #:cl-json 
               #:do-urlencode)
  :serial t
  :components ((:file "src/package")
               (:file "src/cl-hipchat.config")
               (:file "src/cl-hipchat.util")
               (:file "src/cl-hipchat"))
  :in-order-to ((test-op (test-op cl-hipchat-test))))

