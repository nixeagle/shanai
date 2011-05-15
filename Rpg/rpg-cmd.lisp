(in-package :shanai.rpg.commands)


(defvar *commands* #())

(defun ensure-command (symbol function &key (priority 0))
  (let ((name (alexandria:make-keyword symbol)))
    (let ((entrypos (position name *commands* :key 'car))
          (new-entry (list name function priority)))
      (if entrypos
          (setf (aref *commands* entrypos) new-entry)
          (setq *commands* (concatenate 'vector (vector new-entry) *commands*)))
      (setq *commands* (sort *commands* #'> :key #'third)))))

(defun normalize-arglist-for-ppcre (arglist)
  (mapcar (lambda (arg) (typecase arg
                          (cons (if (functionp (car arg))
                                    arg
                                    (list `(function ,(first arg)) (second arg))))
                          (otherwise arg)))
          arglist))

(defun frob-define-command-arglist (arglist)
  "Remove all references to functions if they exist and provide only the
  arguments will be provided to the lambda form later."
   (mapcar (lambda (arg) (typecase arg
                           (cons (second arg))
                           (otherwise arg)))
           arglist))

(defmacro define-command ((name regex &rest arglist) (&rest options &key (priority 0)) &body body)
  "Our job is to return a function of N arguments that will be later called."
  (declare (ignore options))
  `(ensure-command ',name
    (lambda (target-string)
      (ppcre:register-groups-bind ,(normalize-arglist-for-ppcre arglist)
          (,regex target-string)
        ,@body)) :priority ,priority))

(lambda (regex)
  (ppcre:register-groups-bind nil ("a" regex)
    :a))
(defun rpg-command-call (string)
  "Loop through all the defined rpg commands passing STRING.

Will stop calling rpg commands after one returns a non nil value."
  (loop for command across *commands*
     for result = (funcall (second command) string)
     when result
     return result))

(define-command (foo ";foo(.*)bar(.*)" mid rest) ()
  (list :raw-notice-reply (format nil "You sent me ~A and ~A"
                                  (who:escape-string mid)
                                  (who:escape-string rest))))

(define-command (reverse-words ";wordreverse (\\w+) (\\w+).*" word1 word2) ()
  (list :raw-notice-reply
        (format nil "Ok I'm reversing your words! <b>~A</b> <i>~A</i>"
                (who:escape-string word2)
                (who:escape-string word1))))

(define-command (reverse-words/1word ";wordreverse \\w+.*") (:priority -1)
  (list :raw-notice-reply
        "You passed only one word. This command requires two! Try:<br>
;wordreverse word1 word2") )

(define-command (reverse-words/noword ";wordreverse*") (:priority -2)
  (list :raw-notice-reply
        "You gave me no words to reverse! Try:<br>
;wordreverse word1 word2") )