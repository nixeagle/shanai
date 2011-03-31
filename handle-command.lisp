(in-package :pokemon.po.client)

(defmethod handle-command ((cmd (eql :help)) (con connection) (msg message))
  (reply con msg "ooh look, a <a href=\"http://google.com/\">google</a>!"))
(defmethod handle-command ((cmd (eql :forums)) (con connection) (msg message))
  (reply con msg "<a href=\"http://pokemon-online.eu/forums/\">http://pokemon-online.eu/forums/</a>"))
(defmethod handle-command ((cmd (eql :tiers)) (con connection) (msg message))
  (reply con msg "<a href=\"http://pokemon-online.eu/forums/showthread.php?1706\">http://pokemon-online.eu/forums/showthread.php?1706</a>"))
(defmethod handle-command ((cmd (eql :say)) (con connection) (msg message))
  (reply con msg (subseq (message msg)
                         (+ (length ",say") (search ",say " (message msg))))))
(defmethod handle-command ((cmd (eql :client)) (con connection) (msg message))
  (reply con msg "Latest client binary can be downloaded at <a href=\"http://pokemon-online.eu/downloads/Client_17.html\">http://pokemon-online.eu/downloads/Client_17.html</a>"))
(defmethod handle-command ((cmd (eql :pokemon)) (con connection) (msg message))
  (let ((cmd (split-at-first #\ (cdr (parse-nickname-and-message msg)))))
    (when (cdr cmd)
      (let ((int (parse-integer (cdr cmd) :junk-allowed t)))
        (when int
          (let ((poke (gethash int pokemon::*pokedex*)))
            (reply con msg (if poke (html-escape (princ-to-string poke))
                                "<b>Could</b> not find it!
<br/>
<br/>What happens if we sprinkle some newlines in?"))))))))

(defmethod handle-command ((cmd (eql :movedex)) (con connection) (msg message))
  (let ((cmd (split-at-first #\ (cdr (parse-nickname-and-message msg)))))
    (when (cdr cmd)
      (let ((int (parse-integer (cdr cmd) :junk-allowed t)))
        (if (and int (< 0 int pokemon::+total-moves+))
            (reply con msg (html-escape (princ-to-string (aref pokemon::*movedex* int))))
            (reply con msg "Sorry that move number does not exist!"))))))

(defmethod handle-command ((cmd (eql :eval)) (con connection) (msg message))
  (let ((cmd (split-at-first #\ (cdr (parse-nickname-and-message msg)))))
    (when (cdr cmd)
      (reply con msg (cl-who:with-html-output-to-string (s nil :indent nil)
                       (:code (cl-who:esc (with-output-to-string (s) (pprint
                                                                      (handler-case (eval (let ((*package* (find-package :pokemon.po.client))) (read-from-string (cdr cmd) nil "Sorry malformed input. Did you forget a closing paren?"))) (error (condition) condition)) s)))))))))

(defmethod handle-command ((cmd (eql :describe)) (con connection) (msg message))
  (let ((cmd (split-at-first #\ (cdr (parse-nickname-and-message msg)))))
    (when (cdr cmd)
      (reply con msg (with-output-to-string (s) (pprint
                                                 (handler-case (describe-to-html-string (eval (let ((*package* (find-package :pokemon.po.client))) (read-from-string (cdr cmd) nil "Sorry malformed input. Did you forget a closing paren?")))) (error (condition) condition)) s))))))


(defun html-escape (input)
  (cl-who:escape-string input))



(defun describe-to-html-string (object)
  (cl-who:with-html-output-to-string (s)
    (:pre (cl-who:esc (with-output-to-string (s) (describe object s))))))

(defun move-to-html-string (move)
  (flet ((header-col (text)
           (cl-who:with-html-output (s *standard-output*)
             (:td :align :center :bgcolor :black
                  (:font :color "white" (:b (cl-who:esc text)))))))
    (cl-who:with-html-output-to-string (*standard-output*)
      (:table :width "80%" :align :center :cellspacing 0 :cellpadding 0 :border 1
              (:tr (header-col "Move Name")
                   (header-col "Move Type")
                   (header-col "Move Category")
                   (header-col "Base Power")
                   (header-col "Accuracy")
                   (header-col "PP MAX"))))))

(defmethod handle-command ((cmd (eql :test)) (con connection) (msg message))
  (let ((cmd (split-at-first #\ (cdr (parse-nickname-and-message msg)))))
    (when (cdr cmd)
      (let ((int (parse-integer (cdr cmd) :junk-allowed t)))
        (if (and int (< 0 int pokemon::+total-moves+))
            (reply con msg (move-to-html-string (aref pokemon::*movedex* int)))
            (reply con msg "Sorry that move number does not exist!"))))))