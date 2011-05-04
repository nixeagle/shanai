;;; Import all related data for the pokedex.
(in-package :pokemon)

(defun load-pokedex (&optional (po-directory (conf:po-root-directory)))
  "Load all related pokedex information from PO-DIRECTORY.

Will destructively modify *POKEDEX* by first clearing all entries then
reloading each entry after that piecemeal."
  (setq *pokedex* (make-hash-table :test #'eq))
  (let ((*po-directory* po-directory))
    (macrolet ((with-pokemon ((pokemon pline hash) &body body)
                 `(let ((,pokemon (gethash (pokemon-id-from-parsed-line ,pline) ,hash)))
                    ,@body)))
      (with-po-data-file ("pokes/poke_stats.txt"
                          *pokedex* pline hash)
        (setf (gethash (pokemon-id-from-parsed-line pline) hash)
              (make-instance 'shanai.pokemon:basic-pokemon :id (car pline) :type '(???)
                             :base-stats (apply #'shanai.pokemon:make-stats (nthcdr 2 pline)))))

      (with-po-data-file ("pokes/pokemons.txt"
                          *pokedex* pline hash)
        (let ((pokemon (gethash (pokemon-id-from-parsed-line pline) hash)))
          (when pokemon
            (setf pokemon (reinitialize-instance pokemon :name (third pline))))))

      (with-po-data-file ("pokes/poke_weight.txt" *pokedex* pline hash)
        (with-pokemon (pokemon pline hash)
          (when pokemon
            (setf pokemon (reinitialize-instance pokemon :weight (third pline))))))
      (with-po-data-file ("pokes/poke_type1-5G.txt" *pokedex* pline hash)
        (with-pokemon (pokemon pline hash)
          (when pokemon
            (setf pokemon (reinitialize-instance pokemon
                                                 :type (list (nth (third pline) *typedex*)))))))
      (with-po-data-file ("pokes/poke_type2-5G.txt" *pokedex* pline hash)
        (with-pokemon (pokemon pline hash)
          (when pokemon
            (setf pokemon (reinitialize-instance pokemon
                                                 :type (append (shanai.pokemon:pokemon-type pokemon)
                                                               (list (nth (third pline) *typedex*)))))))))))
(in-package :shanai.pokedex)

(defstruct (pokemon-uid (:predicate pokemon-uid-p)
                        (:copier)
                        (:constructor pokemon-uid
                                      (identifier &optional (sub-identifier 0)
                                                  &aux (id identifier)
                                                  (forme-id sub-identifier))))
  "Pokedex ids.

ID is the primary pokemon's identifier as found in the national pokedex for
the relevant pokemon generation.

FORME-ID indicates which forme is being identified. We default this to
zero, pokemon with multiple formes will have a positive number associated
with this."
  (id #xFFFF :type binary-data:u2)
  (forme-id 255 :type binary-data:u1))

(defmethod generic:object-id ((uid pokemon-uid))
  "Pokemon's id, ignoring formes."
  (slot-value uid 'id))

(defmethod generic:forme-id ((uid pokemon-uid))
  (slot-value uid 'forme-id))

(defun make-pokedex ()
  (make-hash-table :test #'equalp))

(defparameter *pokedex* (make-pokedex)
  "Global pokedex and repository of info.")


(defun getpoke (id &optional (generation 5))
  (declare (ignore generation)
           (type (or pokemon-uid string) id))
  (gethash id *pokedex*))

(defun (setf getpoke) (value id &optional (generation 5))
  (declare (ignore generation)
           (type pokemon-uid id)
           (type pokemon::pokemon value))
  (setf (gethash (pokemon::name value) *pokedex*) value
        (gethash id *pokedex*) value))

