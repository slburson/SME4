;;;;  Modified: Friday, August 6, 1999 at 17:02:41 by forbus


(in-package :sme)

(defmethod greedy-gatherer ((sme hpkb-sme) (candidates list))
  "Assumes candidates are non-NIL and sorted by score.  Produces a new
   solution and the list of the candidates not merged, in sorted
   order, for the next round of using it, if needed."
   (multiple-value-bind (solution remaining-candidates)
       (gather-kernels-w-overlapping-correspondences sme (car candidates) (cdr candidates))
      (let ((high-score (if solution (score solution) 0.0))
            (remaining nil))
         (dolist (candidate remaining-candidates (values solution (nreverse remaining)))
            (if (mappings-mutually-consistent? solution candidate)
               (if (significant-mapping-contribution? candidate solution high-score)
                  (setq solution (merge-mapping-pair sme solution candidate))
                  (push candidate remaining))
               (push candidate remaining))))))

(defun gather-kernels-w-overlapping-correspondences (sme seed candidates)
   "Merges, in greedy fashion, the subset of the candidates that shares
 entity correspondences with the seed.  This improves the odds of gathering
 larger structures, since going by score alone can add independent bits that
 are structurally consistent with lower-scored parts that would otherwise
 make a better picture."
   (let ((solution seed)
         (remainder nil))
      (dolist (candidate candidates (values solution (nreverse remainder)))
         (if (and (entity-correspondence-overlap? seed candidate)
                  ;; seed instead of solution to prevent drift
                  (mappings-mutually-consistent? solution candidate))
            (setq solution (merge-mapping-pair sme solution candidate))
            (push candidate remainder)))))

(defun entity-correspondence-overlap? (mapping1 mapping2)
   (let ((mhs2 (mhs mapping2)))
      (dolist (mh1 (mhs mapping1))
         (when (and (entity? mh1) (member mh1 mhs2))
            (return-from entity-correspondence-overlap? (values t))))))

      
