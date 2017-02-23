;;;; cl-hipchat.asd

(asdf:defsystem #:cl-hipchat
  :description "Describe cl-hipchat here"
  :author "Torbjørn Marø"
  :license "MIT License"
  :depends-on (#:drakma #:cl-json)
  :serial t
  :components ((:file "package")
               (:file "cl-hipchat")))

