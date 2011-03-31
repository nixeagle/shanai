;;; Import all related data for the pokedex.
(in-package :pokemon)

(defun load-pokedex (&optional (po-directory *po-directory*))
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
              (make-instance 'pokemon :number (car pline) :type '(???)
                             :base-stats (apply #'battle-stats (nthcdr 2 pline)))))

      (with-po-data-file ("pokes/pokemons.txt"
                          *pokedex* pline hash)
        (let ((pokemon (gethash (pokemon-id-from-parsed-line pline) hash)))
          (setf pokemon (reinitialize-instance pokemon :name (third pline)))))

      (with-po-data-file ("pokes/poke_weight.txt" *pokedex* pline hash)
        (with-pokemon (pokemon pline hash)
          (setf pokemon (reinitialize-instance pokemon :weight (third pline)))))
      (with-po-data-file ("pokes/poke_type1-5G.txt" *pokedex* pline hash)
        (with-pokemon (pokemon pline hash)
          (setf pokemon (reinitialize-instance pokemon
                                               :type (list (nth (third pline) *typedex*))))))
      (with-po-data-file ("pokes/poke_type2-5G.txt" *pokedex* pline hash)
        (with-pokemon (pokemon pline hash)
          (setf pokemon (reinitialize-instance pokemon
                                               :type (append (poketype pokemon)
                                                             (list (nth (third pline) *typedex*))))))))))

