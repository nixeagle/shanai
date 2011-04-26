(in-package :shanai.po.bot)

(defun handle-cl-who-tests (string)
  (handler-case (eval
                 (let ((*package* (find-package :pokemon.po.client)))
                   `(cl-who:with-html-output-to-string (*standard-output*)
                      ,(read-from-string string nil "(:b \"Sorry malformed input. Did you forget a closing paren?\")"))))
    (error (condition) (princ-to-string condition))))

(define-bot-command who (con target user args)
  (when (member user (list "nixeagle" "zeroality" "Gilad") :test #'string-equal)
    (reply (handle-cl-who-tests args))))

(define-bot-command rawwho (con target user args)
  (when (member user (list "nixeagle" "zeroality" "Gilad") :test #'string-equal)
    (reply (cl-who:escape-string (handle-cl-who-tests args)))))




