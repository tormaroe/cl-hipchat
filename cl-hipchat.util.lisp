(in-package #:cl-hipchat.util)

;;; -------------------------------------------------------------------------------
;;; --- PRIVATE UTILITIES ---------------------------------------------------------
;;; -------------------------------------------------------------------------------

(defun keyword-to-lowercase-string (k)
  (string-downcase (format nil "~A" k)))

(defun bool (x)
  (if x :|true| :|false|))

;; http://stackoverflow.com/questions/27679494/how-to-output-false-while-using-cl-json

(defclass json-false ()
  ())

(defmethod json:encode-json ((object json-false) &optional stream)
  (princ "false" stream)
  nil)

(defvar *json-false* (make-instance 'json-false))

(defun json-bool (val)
  (if val t *json-false*))

(defun json-bool-handler (token)
  (or (string= token "true")
      (and (string= token "false") *json-false*)))

(defmacro preserving-json-boolean (opts &body body)
  (declare (ignore opts))
  `(let ((json:*boolean-handler* #'json-bool-handler))
     ,@body))


(defun append-query-params (path xs)
  (flet ((join-param (pair) (format nil "~A=~A" (car pair) (cdr pair))))
    (format nil "~A?~{~A~^&~}" path (mapcar #'join-param xs))))

(defun get-resource-url (resource)
  (format nil "~A~A" *API-BASE-URL* resource))

(defun decode-json-stream (stream)
  (with-input-from-string 
      (s (flexi-streams:octets-to-string stream)) 
    (json:decode-json s)))

(defun make-hipchat-request (method resource &optional body)
  (let ((auth-header (cons "Authorization" (format nil "Bearer ~A" *AUTH-TOKEN*))))
    (multiple-value-bind (body status-code)
        (drakma:http-request (get-resource-url resource)
                             :method method
                             :content (when body (json:encode-json-to-string body))
                             :content-type "application/json"
                             :additional-headers (list auth-header))
      (cond
        ((eq 200 status-code) (decode-json-stream body))
        ((eq 201 status-code) (decode-json-stream body))
        ((eq 204 status-code) t) ; no content
        ((eq 400 status-code) (format *error-output* "400 BAD REQUEST~%~A~%" (decode-json-stream body)) nil)
        ; 401  - signal condition
        ; 404  - values nil resource-not-found
        ))))

