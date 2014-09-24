(in-package :bld-ssweb)

;; AJAX dispatcher to run sail prop

(defun make-controls (angles durations)
  (mapcar #'list
	  (mapcar #'read-from-string angles)
	  (mapcar #'read-from-string durations)))
	  
(defmethod norme ((v list))
  (sqrt
   (loop for vi in v
      sum (expt vi 2))))

(define-easy-handler (ssprop :uri "/ssprop")
    (lightness
     (angles[] :parameter-type 'list)
     (durations[] :parameter-type 'list))
  (setf (content-type*) "application/json")
  (let* ((controls (make-controls angles[] durations[]))
	 (traj
	  (sail-prop *sail-parameters* 
		     :lightness (read-from-string lightness)
		     :controls controls))
	 ;; Polar trajectory of end state
	 (tjf-polar (list (last (car (last traj)))))
	 ;; Final state of trajectory
	 (xp (second (caar tjf-polar)))
	 ;; cartesian state at end of trajectory
	 (tjf-cart (caar (polar-to-xytraj tjf-polar)))
	 (tm (first tjf-cart))
	 (x (rest tjf-cart))
	 (r (subseq x 0 2))
	 (v (subseq x 2 4))
	 (rm (norme r))
	 (vm (norme v))
	 (mu (gethash :mu *sail-parameters*))
	 (eng (- (/ (expt vm 2) 2)
		 (/ mu rm)))
	 (a (- (/ mu 2 eng)))
	 (tp (if (> a 0)
		 (* 2 pi (sqrt (/ (expt a 3) mu)))
		 tm))
	 (final-orbit
	  (sail-prop
	   *sail-parameters*
	   :lightness 0
	   :x0 xp
	   :t0 tm
	   :controls (list (list 0 tp)))))
    (with-output-to-string (s)
      (encode-json
	(polar-to-xytraj
	 (append traj final-orbit)
	 :key #'(lambda (n) (coerce n 'single-float))
	 :velocity nil)
	s))))
