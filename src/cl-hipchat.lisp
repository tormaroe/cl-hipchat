(in-package #:cl-hipchat)


;;; -------------------------------------------------------------------------------
;;; --- MODEL HELPERS -------------------------------------------------------------
;;; -------------------------------------------------------------------------------

(defmacro room-id      (alist) `(cdr (assoc :id ,alist)))
(defmacro message-text (alist) `(cdr (assoc :message ,alist)))

;;; -------------------------------------------------------------------------------
;;; --- PUBLIC API ----------------------------------------------------------------
;;; -------------------------------------------------------------------------------

(defun get-room (room-id-or-name)
  (make-hipchat-request :GET (room-resource room-id-or-name)))

(defun get-all-rooms (&key (start 0) (max 100) (include-private t) include-archived)
  (let ((result (make-hipchat-request :GET
                  (append-query-params "room" `(("start-index" . ,start)
                                                ("max-results" . ,max)
                                                ("include-private" . ,(bool include-private)) 
                                                ("include-archived" . ,(bool include-archived)))))))
    (when result
      (cdr (assoc :items result)))))

(defun create-room (name &key guest-access owner-user-id (privacy :public) (topic ""))
  (assert (in-range-p (length name) 1 50) (name))
  (assert (in-range-p (length topic) 0 250) (topic))
  (assert (member privacy '(:public :private)) (privacy))
  (make-hipchat-request :POST "room" `(("name" . ,name)
                                       ("privacy" . ,(keyword-to-lowercase-string privacy))
                                       ("topic" . ,topic)
                                       ("guest_access" . ,(json-bool guest-access))
                                       ,(when owner-user-id
                                          '("owner_user_id" . owner-user-id)))))


(defun delete-room (room-id-or-name)
  (make-hipchat-request :DELETE (room-resource room-id-or-name)))

(defun set-topic (room-id-or-name topic)
  (assert (in-range-p (length topic) 0 250) (topic))
  (make-hipchat-request :PUT 
    (format nil "~A/topic" (room-resource room-id-or-name))
    `(("topic" . ,topic))))

(defun get-user (name)
  (make-hipchat-request :GET (format nil "user/~A" name)))

(defun get-all-users (&key (start 0) (max 100) include-guests include-deleted)
  (assert (in-range-p max 0 1000) (max))
  (let ((result (make-hipchat-request :GET
                  (append-query-params "user" `(("start-index" . ,start)
                                                ("max-results" . ,max)
                                                ("include-guests" . ,(bool include-guests)) 
                                                ("include-deleted" . ,(bool include-deleted)))))))
    (when result
      (cdr (assoc :items result)))))

(defun send-notification (room-id-or-name message &key (from "") (color :yellow) notify (message-format :html))
  ; TODO: Add optional Card
  (make-hipchat-request :POST
    (format nil "~A/notification" (room-resource room-id-or-name))
    `(("message" . ,message)
      ("message_format" . ,(keyword-to-lowercase-string message-format))
      ("from" . ,from)
      ("color" . ,(keyword-to-lowercase-string color))
      ("notify" . ,(json-bool notify)))))

(defun send-message (room-id-or-name message)
  (make-hipchat-request :POST
    (format nil "~A/message" (room-resource room-id-or-name))
    `(("message" . ,message))))

;;; user: The id, email address, or mention name (beginning with an '@') of the user to send a message to.
;;; message: The message body. Valid length range: 1 - 10000.
;;; notify: Whether this message should trigger a user notification (change the tab color, play a sound, notify mobile phones, etc). Each recipient's notification preferences are taken into account.
(defun send-private-message (user message &key notify (message-format :text))
  (make-hipchat-request :POST
    (format nil "user/~A/message" user)
    `(("message" . ,message)
      ("message_format" . ,(keyword-to-lowercase-string message-format))
      ("notify" . ,(json-bool notify)))))

(defun share-file-with-room (room filepath &optional message)
  (error "Not yet implemented!"))

(defun share-link-with-room (room-id-or-name link &optional (message ""))
  (make-hipchat-request :POST
    (format nil "~A/share/link" (room-resource room-id-or-name))
    `(("message" . ,message)
      ("link" . ,link))))


(defun room-history (room &key (date "recent") (timezone "UTC") (start 0) (max 100) (reverse t))
  (error "Not yet implemented!"))

(defun recent-room-history (room-id-or-name &key not-before (timezone "UTC") (max 100) (include-deleted t))
  (let ((result (make-hipchat-request :GET
                  (append-query-params 
                    (format nil "~A/history/latest" (room-resource room-id-or-name)) 
                    `(("timezone" . ,timezone)
                      ("max-results" . ,max)
                      ("include_deleted" . ,(bool include-deleted))
                      ,(when not-before
                        '("not-before" . not-before)))))))
    (when result
      (cdr (assoc :items result)))))


