(in-package :bld-ssweb)

(define-easy-handler (ssweb :uri "/ssweb")
    (lightness
     (angles[] :parameter-type 'list)
     (durations[] :parameter-type 'list))
  (let ((lightness (if (zerop (length lightness)) "0.1" lightness))
	(angles (if (zerop (length angles[])) (list "0") angles[]))
	(durations (if (zerop (length durations[])) (list "1") durations[])))
    (with-html-output-to-string (s nil :indent t)
      (:html
       (:head
	:title "Solar sail on the web"
	(:link :href "ssweb.css" :rel "stylesheet")
	(:script :src "js/three.min.js")
	(:script :src "js/jquery-1.11.1.min.js")
	(:script :type "text/javascript"
		 (str (ps (lisp *ps-lisp-library*)))))
       (:body
	(:h1 "Navigate a solar sail")
	(:table
	 (:tr
	  (:td :id "trajectory")
	  (:td :id "sailinputs"
	       (:h2 "Sail performance")
	       (:table
		(:tr
		 (:td "Solar force / gravity")
		 (:td (:input :type "number" :min "0" :max "2" :step "0.05" :value lightness :name "lightness"))))
	       (:h2 "Controls")
	       (:div :id "controls"
		     (loop for angle in angles
			for duration in durations
			for count = 1 then (incf count)
			for countstr = (format nil "~a" count)
			do
			  (htm
			   (:div :id countstr
				 (:table
				  (:tr
				   (:td "Angle")
				   (:td (:input :type "number" :min "-90" :max "90" :step "5" :value angle :name "angles[]"))
				   (:td "Duration")
				   (:td (:input :type "number" :min "0" :max "10" :step "0.1" :value duration :name "durations[]"))
				   (when (> count 1)
				     (htm 
				      (:td (:div :class "delete" (:b "Delete")))))))))))
	       (:p (:div :id "addcontrol" (:b "Add new control"))))))
	(:script :src "ssweb.js"))))))
