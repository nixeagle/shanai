(in-package :shanai.po.bot.vote)

(defvar *user-opinion-poll* '())

(defun getpoll (id)
  (declare (type fixnum id))
  (nth id *user-opinion-poll*))
(defun valid-poll-id-p (id)
  (and (integerp id) (< -1 id (length *user-opinion-poll*))))
(defclass basic-poll ()
  ((title :initarg :title)
   (allowed-voter-fn :initarg :allowed-voter-fn)
   (votes :initform '() :initarg :votes))
  (:default-initargs :allowed-voter-fn (lambda (voter) (declare (ignore voter)) t)))

(defun basic-poll-title (poll)
  (declare (type basic-poll poll))
  (slot-value poll 'title))
(defun make-basic-poll (title &key (allowed-voter-fn (lambda (voter) (declare (ignore voter)) t)))
  (make-instance 'basic-poll :title title :allowed-voter-fn allowed-voter-fn))

(defun addpoll (poll)
  (declare (type basic-poll poll))
  (alexandria:appendf *user-opinion-poll* (list poll))
  (1- (length *user-opinion-poll*)))

(defun cast-basic-vote (poll-id trainer vote)
  (let ((poll (getpoll poll-id)))
    (setf (getf (slot-value poll 'votes) (trainer-id trainer)) vote)))

(defun parse-yes-or-no-p (string)
  (if (member string (list "yes" "y") :test #'string-equal)
      t
      (if (member string (list "no" "n") :test #'string-equal)
          nil
          :invalid)))

(defun tally-poll (poll-id)
  (let ((votes (slot-value (getpoll poll-id) 'votes)))
    (let ((yes (count t votes))
          (no (count nil votes)))
      (values yes (+ yes no)))))

(in-package :shanai.po.bot)

(define-bot-command vote (con target user args)
  (let ((trainer-id (shanai.po.client:trainer-id user)))
    (ppcre:register-groups-bind ((#'parse-integer poll-id)
                                 (#'shanai.po.bot.vote::parse-yes-or-no-p vote))
        ("^(\\d+)\\s+(.+)$" args)
      (and trainer-id (if (eq :invalid vote)
               (reply (format nil "~A: your vote is invalid. Please say <b>yes</b> or <b>no</b>."
                              (cl-who:escape-string (generic:name user))))
               (if (shanai.po.bot.vote::valid-poll-id-p poll-id)
                   (progn (shanai.po.bot.vote::cast-basic-vote poll-id
                                                               user
                                                               vote)
                          (reply (format nil "Vote from ~A recieved!"
                                         (generic:name user))))
                   (reply "Please specify an id for a valid ongoing poll!")))))))

(define-bot-command poll-info (con target user args)
  (ppcre:register-groups-bind ((#'parse-integer poll-id)) ("(\\d+)$" args)
    (when (shanai.po.bot.vote::valid-poll-id-p poll-id)
      (multiple-value-bind (yes total) (shanai.po.bot.vote::tally-poll poll-id)
        (reply (cl-who:escape-string (format nil "Poll #~A: (for: ~A, against: ~A) :: ~A"
                                             poll-id
                                             yes
                                             (- total yes)
                                             (shanai.po.bot.vote::basic-poll-title (shanai.po.bot.vote::getpoll poll-id))
                                             )))))))


(define-bot-command create-poll (con target user args)
  (let ((poll-id (shanai.po.bot.vote::addpoll (shanai.po.bot.vote::make-basic-poll args))))
    (reply (format nil "Poll #~A created: ~A. <br>To vote in the poll type: <b>,vote ~A <i>yes|no</i></b>" poll-id (cl-who:escape-string args) poll-id))))