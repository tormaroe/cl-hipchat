
(asdf:defsystem #:cl-hipchat-test
  :description "Test system for cl-hipchat"
  :author "Torbjørn Marø"
  :license "MIT License"
  :homepage "https://github.com/tormaroe/cl-hipchat"
  :depends-on (#:cl-hipchat
               #:prove)
  :defsystem-depends-on (#:prove-asdf)
  :components ((:module "t"
                :components ((:test-file "cl-hipchat.util"))))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))