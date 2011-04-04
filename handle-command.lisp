(in-package :pokemon.po.client)

(defmethod handle-command ((cmd (eql :help)) (con connection) (msg message))
  (reply con msg "ooh look, a <a href=\"http://google.com/\">google</a>!"))
(defmethod handle-command ((cmd (eql :source)) (con connection) (msg message))
  (reply con msg "I'm licensed under the GNU GPL version 3 or later. Find me on <a href=\"http://github.com/nixeagle/shanai\">github</a>!"))
(defmethod handle-command ((cmd (eql :forums)) (con connection) (msg message))
  (reply con msg "<a href=\"http://pokemon-online.eu/forums/\">http://pokemon-online.eu/forums/</a>"))
(defmethod handle-command ((cmd (eql :tiers)) (con connection) (msg message))
  (reply con msg "<a href=\"http://pokemon-online.eu/forums/showthread.php?1706\">http://pokemon-online.eu/forums/showthread.php?1706</a>"))
(defmethod handle-command ((cmd (eql :say)) (con connection) (msg message))
  (when (or (string= "nixeagle" (car (parse-nickname-and-message msg)))
            (string= "zeroality" (car (parse-nickname-and-message msg))))
    (reply con msg (subseq (message msg)
                           (+ (length ",say") (search ",say " (message msg)))))))
(defmethod handle-command ((cmd (eql :client)) (con connection) (msg message))
  (reply con msg "Latest client binary can be downloaded at <a href=\"http://pokemon-online.eu/downloads/Client_17.html\">http://pokemon-online.eu/downloads/Client_17.html</a>"))
(defmethod handle-command ((cmd (eql :pokedex)) (con connection) (msg message))
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
            (reply con msg (move-to-html-string (pokemon::find-move int)))
            (let ((poke (pokemon::find-move (cdr cmd))))
              (if poke
                  (reply con msg (move-to-html-string poke))
                  (reply con msg "Sorry that move number does not exist!"))))))))

(defmethod handle-command ((cmd (eql :eval)) (con connection) (msg message))
  (let ((cmd (split-at-first #\ (cdr (parse-nickname-and-message msg)))))
    (when (or (string= "nixeagle" (car (parse-nickname-and-message msg)))
              (string= "zeroality" (car (parse-nickname-and-message msg))))
      (when (cdr cmd)
        (reply con msg (cl-who:with-html-output-to-string (s nil :indent nil)
                         (:code (cl-who:esc (with-output-to-string (s) (pprint
                                                                        (handler-case (eval (let ((*package* (find-package :pokemon.po.client))) (read-from-string (cdr cmd) nil "Sorry malformed input. Did you forget a closing paren?"))) (error (condition) condition)) s))))))))))

(defmethod handle-command ((cmd (eql :describe)) (con connection) (msg message))
  (let ((cmd (split-at-first #\ (cdr (parse-nickname-and-message msg)))))
    (when (or (string= "nixeagle" (car (parse-nickname-and-message msg)))
              (string= "zeroality" (car (parse-nickname-and-message msg))))
      (when (cdr cmd)
        (reply con msg (with-output-to-string (s) (pprint
                                                   (handler-case (describe-to-html-string (eval (let ((*package* (find-package :pokemon.po.client))) (read-from-string (cdr cmd) nil "Sorry malformed input. Did you forget a closing paren?")))) (error (condition) condition)) s)))))))


(defun html-escape (input)
  (cl-who:escape-string input))



(defun describe-to-html-string (object)
  (cl-who:with-html-output-to-string (s)
    (:pre (cl-who:esc (with-output-to-string (s) (describe object s))))))

(defun move-to-html-string (move)
  (cl-who:with-html-output-to-string (*standard-output*)
    (:TABLE :ALIGN :CENTER :CELLPADDING 2 :CELLSPACING 0 :STYLE
            "border-width:1px; border-style:solid; border-color:#000;"
            (:THEAD :STYLE "font-weight:bold;"
                    (:TR :STYLE "background-color:#b0b0b0;"
                         (:TD :ALIGN :CENTER "Name")
                         (:TD :ALIGN :CENTER "Type")
                         (:TD :ALIGN :CENTER "Category")
                         (:TD :ALIGN :CENTER "Power")
                         (:TD :ALIGN :CENTER "Acc.")
                         (:TD :ALIGN :CENTER "PP")
                         (:TD :ALIGN :CENTER "Priority")
                         (:TD :ALIGN :CENTER "Range")))
            (:TBODY :STYLE "font-weight:bold;"
                    (:TR :STYLE "background-color:#c0c0c0; vertical-align:middle;"
                         (:TD :ALIGN :CENTER (cl-who:esc (pokemon::name move)))
                         (:TD :ALIGN :CENTER (:IMG :SRC  (format nil "Themes/Classic/types/type~A.png" (position (alexandria:make-keyword (pokemon::poketype move)) pokemon::+pokemon-types+))))
                         (:TD :ALIGN :CENTER (cl-who:str (pokemon::damage-class move)))
                         (:TD :ALIGN :CENTER (cl-who:str (pokemon::power move)))
                         (:TD :ALIGN :CENTER (cl-who:str (pokemon::accuracy move)))
                         (:TD :ALIGN :CENTER (cl-who:str (pokemon::pp move)))
                         (:TD :ALIGN :CENTER (cl-who:str (pokemon::priority move)))
                         (:TD :ALIGN :CENTER (cl-who:str (pokemon::range move)))))
            (:TFOOT :STYLE "font-style:italic;"
                    (:TR :STYLE "background-color:#c2c2c2;"
                         (:TD :COLSPAN 8 (:STRONG :STYLE "font-style:normal;" "Description ")
                              (cl-who:esc (pokemon::effect-description move))))))))

(defmethod handle-command ((cmd (eql :test)) (con connection) (msg message))
  (let ((cmd (split-at-first #\ (cdr (parse-nickname-and-message msg)))))
    (when (cdr cmd)
      (let ((int (parse-integer (cdr cmd) :junk-allowed t)))
        (if (and int (< 0 int pokemon::+total-moves+))
            (reply con msg (move-to-html-string (aref pokemon::*movedex* int)))
            (reply con msg "Sorry that move number does not exist!"))))))


(defun print-channel-name (chan stream)
  "Print CHAN's name to STREAM.

Format is #foobar"
  (declare (type channel chan))
  (write-char #\# stream)
  (princ (name chan) stream))

(defun handle-wikilinks (stg)
  (with-output-to-string (*standard-output*)
      (cl-ppcre:do-scans (ms me rs re "\\[\\[([^\\]]*)\\]\\]" stg) (princ "<a href=\"http://en.wikipedia.org/wiki/") (princ (hunchentoot:url-encode (subseq stg (aref rs 0) (aref re 0)))) (princ "\">") (princ "en:") (princ (cl-who:escape-string (subseq stg (aref rs 0) (aref re 0)))) (princ "</a>")(write-char #\space))))


(defmethod handle-command ((cmd (eql :who)) (con connection) (msg message))
  (let ((cmd (split-at-first #\ (cdr (parse-nickname-and-message msg)))))
    (when (or (string= "nixeagle" (car (parse-nickname-and-message msg)))
              (string= "zeroality" (car (parse-nickname-and-message msg))))
      (when (cdr cmd)
        (reply con msg (handle-cl-who-tests (cdr cmd)))))))


(defmethod handle-command ((cmd (eql :rawwho)) (con connection) (msg message))
  (let ((cmd (split-at-first #\ (cdr (parse-nickname-and-message msg)))))
    (when (or (string= "nixeagle" (car (parse-nickname-and-message msg)))
              (string= "zeroality" (car (parse-nickname-and-message msg))))
      (when (cdr cmd)
        (reply con msg (cl-who:escape-string (handle-cl-who-tests (cdr cmd))))))))

(defun handle-cl-who-tests (string)
  (handler-case (eval
                 (let ((*package* (find-package :pokemon.po.client)))
                   `(cl-who:with-html-output-to-string (*standard-output*)
                      ,(read-from-string string nil "(:b \"Sorry malformed input. Did you forget a closing paren?\")"))))
    (error (condition) (princ-to-string condition))))

