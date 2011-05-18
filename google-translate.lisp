(in-package :pokemon.po.client)
(defun google-translate (word from to)
  "Translate the WORD FROM a language TO another one."
  (destructuring-bind ((data (translations ((translated . text)))))
      (with-input-from-string (s 
                               (let ((drakma:*text-content-types* '(("application" . "json"))))
                                 (drakma:http-request "https://www.googleapis.com/language/translate/v2"
                                                      :parameters `(("key" . ,conf::*google-translate-api-key*)
                                                                    ("q" . ,word)
                                                                    ("source" . ,from)
                                                                    ("target" . ,to))
                                                      )))
        (json:decode-json s))
    (declare (ignore data translations translated))
    text))