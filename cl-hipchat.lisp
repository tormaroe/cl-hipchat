(in-package #:cl-hipchat)


;;; -------------------------------------------------------------------------------
;;; --- MODEL HELPERS -------------------------------------------------------------
;;; -------------------------------------------------------------------------------

(defmacro room-id (alist)
  `(cdr (assoc :id ,alist)))

(defmacro message-text (alist)
  `(cdr (assoc :message ,alist)))

;;; -------------------------------------------------------------------------------
;;; --- PUBLIC API ----------------------------------------------------------------
;;; -------------------------------------------------------------------------------

;;; Get room details
;;; Auth required with scope 'view_group'. 
;;; https://www.hipchat.com/docs/apiv2/method/get_room
(defun get-room (room-id-or-name)
  ;; TODO: Url-encode room-id-or-name ?!
  (make-hipchat-request :GET (format nil "room/~A" room-id-or-name)))

;;; Auth required with scope 'view_group'. 
;;; max: The maximum number of results. Valid length 0-100
;;; https://www.hipchat.com/docs/apiv2/method/get_all_rooms
(defun get-all-rooms (&key (start 0) (max 100) (include-private t) include-archived)
  (let ((result (make-hipchat-request :GET
                  (append-query-params "room" `(("start-index" . ,start)
                                                ("max-results" . ,max)
                                                ("include-private" . ,(bool include-private)) 
                                                ("include-archived" . ,(bool include-archived)))))))
    (when result
      (cdr (assoc :items result)))))

;;; Creates a new room
;;; Auth required with scope 'manage_rooms'. 
;;; https://api.hipchat.com/v2/room
(defun create-room (name &key guest-access owner-user-id (privacy :public) (topic ""))
  ; TODO: assert valid privacy value
  ; TODO: Add owner-user-id if has value
  (make-hipchat-request :POST "room" `(("name" . ,name)
                                       ("privacy" . ,(keyword-to-lowercase-string privacy))
                                       ("topic" . ,topic)
                                       ("guest_access" . ,(json-bool guest-access)))))


;;; Delete a room and kick the current particpants.
;;; Authentication required with scope 'manage_rooms'. 
;;; https://www.hipchat.com/docs/apiv2/method/delete_room
(defun delete-room (room-id-or-name)
  (make-hipchat-request :DELETE (format nil "room/~A" room-id-or-name)))

;;; Set a room's topic.
;;; Auth required with scope 'admin_room'.
;;; https://www.hipchat.com/docs/apiv2/method/set_topic
(defun set-topic (room-id-or-name topic)
  ; TODO: max length 250
  (make-hipchat-request :PUT 
    (format nil "room/~A/topic" room-id-or-name)
    `(("topic" . ,topic))))

;;; Gets information about the requested user
;;; Auth required with scope 'view_group'.
;;; https://www.hipchat.com/docs/apiv2/method/view_user
(defun get-user (name)
  (make-hipchat-request :GET (format nil "user/~A" name)))

(defun get-all-users (&key (start 0) (max 100) include-guests include-deleted))

;;; Send a message to a room
;;; Auth required with scope 'view_group'.
;;; https://www.hipchat.com/docs/apiv2/method/send_room_notification
(defun send-notification (room-id-or-name message &key (from "") (color :yellow) notify (message-format :html))
  ; TODO: Add optional Card
  (make-hipchat-request :POST
    (format nil "room/~A/notification" room-id-or-name)
    `(("message" . ,message)
      ("message_format" . ,(keyword-to-lowercase-string message-format))
      ("from" . ,from)
      ("color" . ,(keyword-to-lowercase-string color))
      ("notify" . ,(json-bool notify)))))

;;; https://www.hipchat.com/docs/apiv2/method/send_message
(defun send-message (room-id-or-name message)
  (make-hipchat-request :POST
    (format nil "room/~A/message" room-id-or-name)
    `(("message" . ,message))))

;;; Sends a user a private message.
;;; Auth required with scope 'send_message'.
;;; user: The id, email address, or mention name (beginning with an '@') of the user to send a message to.
;;; message: The message body. Valid length range: 1 - 10000.
;;; notify: Whether this message should trigger a user notification (change the tab color, play a sound, notify mobile phones, etc). Each recipient's notification preferences are taken into account.
;;; https://www.hipchat.com/docs/apiv2/method/private_message_user
(defun send-private-message (user message &key notify (message-format :text)))

;;; Share a file with a room
;;; Auth required with scope 'send_message'.
;;; https://www.hipchat.com/docs/apiv2/method/share_file_with_room
(defun share-file-with-room (room filepath &optional message))

;;; https://www.hipchat.com/docs/apiv2/method/share_link_with_room
(defun share-link-with-room (room-id-or-name link &optional (message ""))
  (make-hipchat-request :POST
    (format nil "room/~A/share/link" room-id-or-name)
    `(("message" . ,message)
      ("link" . ,link))))


;;; https://www.hipchat.com/docs/apiv2/method/view_room_history
(defun room-history (room &key (date "recent") (timezone "UTC") (start 0) (max 100) (reverse t)))

;;; https://www.hipchat.com/docs/apiv2/method/view_recent_room_history
(defun recent-room-history (room &key not-before (timezone "UTC") (max 100) (include-deleted t))
  ; TODO: Add not-before if specified...
  (let ((result (make-hipchat-request :GET
                  (append-query-params 
                    (format nil "room/~A/history/latest" room) 
                    `(("timezone" . ,timezone)
                      ("max-results" . ,max)
                      ("include_deleted" . ,(bool include-deleted)))))))
    (when result
      (cdr (assoc :items result)))))


