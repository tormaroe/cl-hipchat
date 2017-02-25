;;;; cl-hipchat.asd

(asdf:defsystem #:cl-hipchat
  :description "Client wrapper library for the HipChat API"
  :author "Torbjørn Marø"
  :license "MIT License"
  :depends-on (#:drakma #:cl-json #:do-urlencode)
  :serial t
  :components ((:file "package")
               (:file "cl-hipchat.config")
               (:file "cl-hipchat.util")
               (:file "cl-hipchat")))

