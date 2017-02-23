
(defun slurp (path)
  (with-open-file (stream path) 
    (let ((data (make-string (file-length stream)))) 
      (read-sequence data stream) 
      data)))

(setf cl-hipchat.config:*AUTH-TOKEN* (slurp "~/.cl-hipchat-dev-token"))

(defun print-user (u)
  (flet ((user-slot (x) (cdr (assoc x u))))
    (format t "~%User ID      : ~A~%" (user-slot :id))
    (format t "Name         : ~A~%" (user-slot :name))
    (format t "Mention name : @~A~%" (user-slot :mention--name))
    (format t "Title        : ~A~%" (user-slot :title))
    (format t "Email        : ~A~%" (user-slot :email))
    (format t "Last active  : ~A~%" (user-slot :last--active))
    (format t "Timezone     : ~A~%" (user-slot :timezone))
    (format t "Group admin  ? ~A~%~%" (cl-hipchat.util:bool (user-slot :is--group--admin)))))

(let ((u (cl-hipchat:get-user (slurp "~/.cl-hipchat-dev-user"))))
  (when u
    (print-user u)))