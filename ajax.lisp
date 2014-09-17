(in-package :bld-ssweb)

;; AJAX dispatcher to run sail prop

(defun make-controls (angles durations)
  (mapcar #'list
	  (mapcar #'read-from-string angles)
	  (mapcar #'read-from-string durations)))
	  

(define-easy-handler (ssprop :uri "/ssprop")
    (lightness
     (angles[] :parameter-type 'list)
     (durations[] :parameter-type 'list))
  (let ((controls (make-controls angles[] durations[])))
    (with-output-to-string (s)
      (encode-json
       (polar-to-xytraj
	(sail-prop *sail-parameters* 
		   :lightness (read-from-string lightness)
		   :controls controls)
	:key #'(lambda (n) (coerce n 'single-float))
	:velocity nil)
       s))))
