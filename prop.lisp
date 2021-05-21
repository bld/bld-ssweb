(in-package :bld-ssweb)

(defparameter *sail-parameters*
  (make-hash
   :lightness 0.1d0
   :mu 1d0
   :x0 (vector 1d0 0d0 0d0 1d0)
   :t0 0d0
   :controls (list (list (* 0 *deg*) (* 1 *years*))
		   (list (* 0 *deg*) (* 1 *years*))
		   (list (* 0 *deg*) (* 1 *years*))
		   (list (* 0 *deg*) (* 1 *years*)))))

(defun sail-eom (tm x p)
  (lethash (lightness mu u) p
    (let ((r (aref x 0))
	  (th (aref x 1))
	  (vr (aref x 2))
	  (vt (aref x 3))
	  (co (cos u))
	  (si (sin u)))
      (vector vr
	      (/ vt r)
	      (+ (/ (expt vt 2) r)
		 (* mu (/ (1- (* lightness (expt co 2) (abs co))) (expt r 2))))
	      (- (/ (* mu lightness (expt co 2) si) (expt r 2))
		 (/ (* vr vt) r))))))

(defun sail-prop (sail &key 
			 (lightness (gethash :lightness sail))
			 (mu (gethash :mu sail))
			 (x0 (gethash :x0 sail))
			 (t0 (gethash :t0 sail))
			 (controls (gethash :controls sail)))
  (loop for (u dtf) in controls
     for t0i = t0 then tfi
     for tfi = (+ t0i dtf)
     for x0i = x0 then xfi
     for traji = (rka #'sail-eom t0i tfi x0i
		      :tol 1d-9 :hmax (/ *years* 12)
		      :param (make-hash :u u :lightness lightness :mu mu))
     for xfi = (second (car (last traji)))
     collect traji))

(defun polar-to-xytraj (traj &key (velocity t) key)
  (loop for segment in traj
     collect 
       (loop for (tm state) in segment
	  for (r th vr vt) = (map 'list #'identity state)
	  for c = (cos th)
	  for s = (sin th)
	  for x = (* r c)
	  for y = (* r s)
	  for vx = (when velocity (- (* vr c) (* vt s)))
	  for vy = (when velocity (+ (* vr s) (* vt c)))
	  collect (let ((state
			 (if velocity (list tm x y vx vy)
			     (list tm x y))))
		    (if key (mapcar key state) state)))))
