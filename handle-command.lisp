(in-package :pokemon.po.client)

(defmethod handle-command ((cmd (eql :help)) (con connection) (msg message))
  (reply con msg "ooh look, a <a href=\"http://google.com/\">google</a>!"))
(defmethod handle-command ((cmd (eql :source)) (con connection) (msg message))
  (reply con msg "I'm licensed under the GNU GPL version 3 or later. Find me on <a href=\"http://github.com/nixeagle/shanai\">github</a>!"))
(defmethod handle-command ((cmd (eql :forums)) (con connection) (msg message))
  (reply con msg "<a href=\"http://pokemon-online.eu/forums/\">http://pokemon-online.eu/forums/</a>"))
(defmethod handle-command ((cmd (eql :tour)) (con connection) (msg message))
  (reply con msg "There is a tournement going on in #tournaments!"))
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
  (multiple-value-bind (nick cmd args) (parse-possible-command (message msg))
    (when cmd
      (let ((int (parse-integer args :junk-allowed t)))
        (if int
            (let ((poke (gethash int pokemon::*pokedex*)))
              (reply con msg (if poke (html-escape (princ-to-string poke))
                                 "Could not find it! Did you typo?")))
            (let ((poke (find-pokemon-by-name args)))
              (reply con msg (if poke (html-escape (princ-to-string poke))
                                 "Could not find it! Did you typo?"))))))))

(defmethod handle-command ((cmd (eql :movedex)) (con connection) (msg message))
  (multiple-value-bind (nick cmd args) (parse-possible-command (message msg))
    (declare (ignore nick))
    (when cmd
      (let ((int (parse-integer args :junk-allowed t)))
        (if (and int (< 0 int pokemon::+total-moves+))
            (reply con msg (move-to-html-string (pokemon::find-move int)))
            (let ((poke (pokemon::find-move args)))
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

(defun html-escape-string (input)
  "Prepare a string for safe display in a webbrowser."
  (declare (type string input)
           (optimize (speed 3) (debug 0)))
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
  ""
  #+ () (with-output-to-string (*standard-output*)
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

(defun match-level-and-base-stat (command-string)
  (cl-ppcre:register-groups-bind (a b) ("(\\d+)\\s(\\d+)" command-string)
    (let ((level (parse-integer a))
          (base-stat (parse-integer b)))
      (values level base-stat))))

(defun other-stat-formula (level base iv ev nature)
  (pokemon::_*_ (+ (pokemon::_*_ (+ iv (* 2 base) (pokemon::_/_ ev 4))
               level 1/100) 5) (/ nature 100)))


(defun other-formula-intervals (level base)
  (flet ((formula (iv ev nature)
           (other-stat-formula level base iv ev nature)))
    (let ((result (make-array '(2 2 3) )))
      (setf (aref result 0 0 0) (formula 0 0 90)
            (aref result 0 0 1) (formula 0 0 100)
            (aref result 0 0 2) (formula 0 0 110)
            (aref result 0 1 0) (formula 0 252 90)
            (aref result 0 1 1) (formula 0 252 100)
            (aref result 0 1 2) (formula 0 252 110)
            (aref result 1 0 0) (formula 31 0 90)
            (aref result 1 0 1) (formula 31 0 100)
            (aref result 1 0 2) (formula 31 0 110)
            (aref result 1 1 0) (formula 31 252 90)
            (aref result 1 1 1) (formula 31 252 100)
            (aref result 1 1 2) (formula 31 252 110))
      result)))

(defmethod handle-command ((cmd (eql :statranges)) (con connection) (msg message))
  (let ((cmd (split-at-first #\ (cdr (parse-nickname-and-message msg)))))
    (multiple-value-bind (lvl basestats) (match-level-and-base-stat (cdr cmd))
      (when (and lvl basestats)
        (reply con msg (handle-ev-iv-chart (other-formula-intervals lvl basestats)))))))

(defun find-pokemon-by-name (term)
  (loop for val being the hash-value of pokemon::*pokedex*
     when (string-equal (pokemon::name val) term) return val))
(defmethod handle-command ((cmd (eql :scripts)) (con connection) (msg message))
  (reply con msg "<a href=\"http://pokemon-online.eu/scripts.js\">PO Beta server Scripts.js</a>"))
(defun handle-ev-iv-chart (array)
  (cl-who:with-html-output-to-string (*standard-output*)
    (:table :align :center :cellpadding 2 :cellspacing 0 :style"border-width:1px; border-style:solid; border-color:#000;"
            (:thead :style"font-weight:bold;"
                    (:tr :style"background-color:#b0b0b0;"
                         (:td :align :center "--")
                         (:td :align :center "Negative")
                         (:td :align :center "Neutral")
                         (:td :align :center "Positive")))
            (:tbody (:tr :style"background-color:#c2c2c2;"
                         (:td :align :right :style"font-weight:bold;" "Min EVs")
                         (:td :align :center (str (aref array 1 0 0)))
                         (:td :align :center (str (aref array 1 0 1)))
                         (:td :align :center (str (aref array 1 0 2))))
                    (:tr :style"background-color:#c2c2c2;"
                         (:td :align :right :style"font-weight:bold;" "Max EVs")
                         (:td :align :center (str (aref array 1 1 0)))
                         (:td :align :center (str (aref array 1 1 1)))
                         (:td :align :center (str (aref array 1 1 2))))))
    
    #+ () (:table :align :center :cellpadding 2 :cellspacing 0 :style"border-width:1px; border-style:solid; border-color:#000;"
            (:thead :style"font-weight:bold;"
                    (:tr :style"background-color:#b0b0b0;"
                         (:td :rowspan 2 :align :center :style"vertical-align:middle;" (:img :src "Themes/Classic/client/uAvailable.png"))
                         (:td :colspan 2 :align :center "Negative Nature")
                         (:td :colspan 2 :align :center "Neutral Nature")
                         (:td :colspan 2 :align :center "Positive Nature"))
                    (:tr :style"background-color:#c0c0c0;"
                         (:td :align :center "Min EVs")
                         (:td :align :center "Max EVs")
                         (:td :align :center "Min EVs")
                         (:td :align :center "Max EVs")
                         (:td :align :center "Min EVs")
                         (:td :align :center "Max EVs")))
            (:tbody (:tr :style"background-color:#c2c2c2;"
                         (:td :align :right :style"font-weight:bold;" "Min IVs")
                         (:td :align :center (str (aref array 0 0 0)))
                         (:td :align :center (str (aref array 0 1 0)))
                         (:td :align :center (str (aref array 0 0 1)))
                         (:td :align :center (str (aref array 0 1 1)))
                         (:td :align :center (str (aref array 0 0 2)))
                         (:td :align :center (str (aref array 0 1 2))))
                    (:tr :style"background-color:#c2c2c2;"
                         (:td :align :right :style"font-weight:bold;" "Max IVs")
                         (:td :align :center (str (aref array 1 0 0)))
                         (:td :align :center (str (aref array 1 1 0)))
                         (:td :align :center (str (aref array 1 0 1)))
                         (:td :align :center (str (aref array 1 1 1)))
                         (:td :align :center (str (aref array 1 0 2)))
                         (:td :align :center (str (aref array 1 1 2))))))))


;;; for tours
(defvar *current-tour-suggestions* ())

(defmethod handle-command ((cmd (eql :clear-tour-suggestions)) (con connection) (msg message))
  (setq *current-tour-suggestions* '())
  (reply con msg "Cleared list of tier suggestions"))

(defmethod handle-command ((cmd (eql :show-tour-suggestions)) (con connection) (msg message))
  (reply con msg (format nil "Currently suggested tours: ~A" (princ-to-string *current-tour-suggestions*))))


(defmethod handle-command ((cmd (eql :suggest)) (con connection) (msg message))
  (multiple-value-bind (nick cmd args) (parse-possible-command (message msg))
    (if (some (lambda (con) (string-equal (car con) nick)) *current-tour-suggestions*)
        (progn (let ((ca (find nick *current-tour-suggestions* :key #'car :test #'string-equal)))
                 (setf (cdr ca) args)
                 (reply con msg (format nil "Changed ~A's suggested tier to ~A!"
                                        nick args))))
        (progn
         (setq *current-tour-suggestions* (cons (cons nick args) *current-tour-suggestions*))
         (reply con msg (format nil "~A suggested the next tournament play in the ~A tier!"
                                nick args))))))
(defmethod handle-command ((cmd (eql :random-tour-suggestion)) (con connection) (msg message))
  (if *current-tour-suggestions*
      (reply con msg (format nil "I drew the ~A tier out of my imaginary hat!" (princ-to-string (alexandria:random-elt *current-tour-suggestions*))))
      (reply con msg "Sorry! I don't have any suggestions to draw from in my hat!")))

(defmethod handle-command ((cmd (eql :start-tour)) (con connection) (msg message))
  (if (< 1 (length *current-tour-suggestions*))
      (reply con msg (format nil "I'm pretending to start a tour. Manually start it for me if you like with /tour ~A:8"  (cdr (alexandria:random-elt *current-tour-suggestions*))))
      (reply con msg "Sorry nobody has made any suggestions. I need at least <b>2</b> suggestions before I'll start a new tour!")))


(defmethod handle-command ((cmd (eql :typematchup)) (con connection) (msg message))
  (cl-ppcre:register-groups-bind (atk def def2)
    (".*,typematchup (\\S+)\\s+(?:on\\s)?(\\S+)(?:\\s+(\\S+))?" (message msg))
    (let ((atk (gethash atk shanai.pokemon.type::+pokemon-type-name-dictionary+))
          (def (gethash def shanai.pokemon.type::+pokemon-type-name-dictionary+))
          (def2 (gethash def2 shanai.pokemon.type::+pokemon-type-name-dictionary+ :???)))
      (if (and atk def def2)
          (reply con msg (princ-to-string (/ (shanai.pokemon.type::type-matchup atk def def2) 4)))
          (reply con msg "You have me confused! Try the following format: <b>attacking-type</b> on <b>defending-type1</b> <i>defending-type2<i>")))))


(defmethod handle-command ((cmd (eql :league)) (con connection) (msg message))
  (reply con msg "Current league members are: Omega(poison), Cannoli(water), Eva(dragon), <i>insert more</i>."))

(defmethod handle-command ((cmd (eql :commands)) (con connection) (msg message))
  (reply con msg "commands: ,help ,commands ,source ,forums ,tiers ,client ,movedex ,pokedex ,typematchup ,statranges ,tres ,tren"))

(defmethod handle-command ((cmd (eql :tres)) (con connection) (msg message))
  (multiple-value-bind (nick cmd args) (parse-possible-command (message msg))
    (reply con msg (concatenate 'string "<b>" (html-escape nick) ":</b> " (html-escape (google-translate args "en" "es"))))))

(defmethod handle-command ((cmd (eql :tren)) (con connection) (msg message))
  (multiple-value-bind (nick cmd args) (parse-possible-command (message msg))
    (reply con msg (concatenate 'string "<b>" (html-escape nick) ":</b> " (html-escape (google-translate args "es" "en"))))))


