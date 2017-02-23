;;;; package.lisp

(defpackage #:cl-hipchat.config
  (:nicknames #:hipchat.config)
  (:use #:cl)
  (:export #:*API-BASE-URL*
           #:*AUTH-TOKEN*))

(defpackage #:cl-hipchat.util
  (:nicknames #:hipchat.util)
  (:use #:cl #:cl-hipchat.config)
  (:export #:make-hipchat-request
           #:append-query-params
           #:json-bool
           #:bool
           #:keyword-to-lowercase-string))

(defpackage #:cl-hipchat
  (:nicknames #:hipchat)
  (:use #:cl #:cl-hipchat.util)
  (:export 
           ;; Room API
           #:get-room
           #:get-all-rooms
           #:create-room
           #:delete-room
           #:set-topic

           ;; User API
           #:get-user
           #:get-all-users

           ;; Read API
           #:room-hitsory
           #:recent-room-history

           ;; Send API
           #:send-notification
           #:send-message
           #:send-private-message
           #:share-file-with-room
           #:share-link-with-room

           ;; Model helper macros
           #:room-id
           #:message-text
           ))