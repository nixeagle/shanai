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
    (lambda (target-string user target connection)
      (declare (ignore connection))
      (flet ((sender () user)
             (target () target))
        (ppcre:register-groups-bind ,(normalize-arglist-for-ppcre arglist)
            (,regex target-string)
          ,@body))) :priority ,priority))

(lambda (regex)
  (ppcre:register-groups-bind nil ("a" regex)
    :a))

(defun rpg-command-call (string user target connection)
  "Loop through all the defined rpg commands passing STRING.

Will stop calling rpg commands after one returns a non nil value."
  (declare (ignore connection))
  (loop for command across *commands*
     for result = (funcall (second command) string user target connection)
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
;wordreverse word1 word2"))

(define-command (reverse-words/noword ";wordreverse*") (:priority -2)
  (list :raw-notice-reply
        "You gave me no words to reverse! Try:<br>
;wordreverse word1 word2"))

(define-command (help "[?;!]help") ()
  (list :raw-notice-reply "<b>TODO, actual help!</b>"))

(define-command (commands "[?;]commands") ()
  (list :raw-notice-reply "<table align=center><tr><td width=10em align=center>help</td><td width=10em align=center>commands</td><td width=10em align=center>look</td></tr></table>"))

(define-command (look ";look (?:at )?(.*)" (cl-who:escape-string args)) ()
  (list :raw-notice-reply
        (format nil "<b>TODO, here should display information about '~A' that can be gleaned by simply obvserving it/them</b>" args))
  (list :raw-channel-reply
        (format nil "/sendhtmlall <timestamp/><i>~A looks at ~A</i>"
                (cl-who:escape-string (generic:name (sender)))
                args)))

(define-command (signup ";signup") ()
  (let ((player (rpg-db:select-rpg-player-by-name (name (sender)))))
    (if player
     (list :raw-notice-reply
           (format nil "<timestamp/>~A: You are already signed up! Your account id is ~A." (esc (getf player :name))
                   (getf player :id)))
     (progn (rpg-db::insert-rpg-player
             (make-instance 'rpg-global::rpg-player :name
                            (name (sender))))
            (list :raw-notice-reply
                  (format nil "<timestamp/>~A: Thank you for signing up! An account has been created for you, please ;login." (esc (name (sender))))))))
  ;(rpg-global::add-rpg-player (name (sender)))
  )
(define-command (info-list-players "\\?\\?list all players") ()
  (list :raw-channel-reply
        (format nil "~A requested player list:<br>~A"
                (esc (name (sender)))
                (mapcar (lambda (n) (esc (name n)))
                        rpg-global::*trainers*))))

(define-command (choose ";choose (.*)" arg) ()
  (list :raw-channel-reply
        (format nil "~A chose ~A!" (esc (name (sender))) arg)))

(define-command (login ";login") ()
  (let ((player (handle-user-login (name (sender)))))
    (if player
        (list :raw-channel-reply
              (format nil "<timestamp/>~A has logged into the RPG!" (esc (name player))))
        (list :raw-notice-reply
              (format nil "<timestamp/>~A: You have already logged in!"
                      (esc (name (sender))))))))

(define-command (logout ";logout") ()
  (let ((player (find (name (sender)) rpg-global::*trainers*
                      :key #'name :test #'string-equal)))
    (if player
        (progn
          (handle-user-logout (name player))
          (list :raw-notice-reply
                (format nil "<timestamp/>~A: Thank you for playing, see you soon!" (esc (name player)))))
        (list :raw-notice-reply
              (format nil "<timestamp/>~A: You are already logged out!"
                      (esc (name (sender))))))))


(defun handle-user-login (username)
  (declare (type string username))
  (let ((player (rpg-db:select-rpg-player-by-name username)))
    (when (and player (not (find username rpg-global::*trainers* :key #'name :test #'string-equal)))
      (rpg-global::add-rpg-player (getf player :name)))))

(defun handle-user-logout (username)
  (declare (type string username))
  (setq rpg-global::*trainers*
        (remove username rpg-global::*trainers* :key #'name :test #'string=)))


(define-command (imp "!shimp (.*)" arg) ()
  (list :raw-channel-reply
        (format nil "~A" arg)))


