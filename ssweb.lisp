(in-package :bld-ssweb)

(define-easy-handler (ssweb :uri "/ssweb") ()
  (with-html-output-to-string (s)
    (:html
     (:head
      :title "Solar sail on the web"
      (:script :src "js/three.min.js")
      (:script :src "js/jquery-1.11.1.min.js")
      (:script :type "text/javascript"
	       (str (ps (lisp *ps-lisp-library*)))))
     (:body
      (:h1 "Navigate a solar sail")
      (:form
       (:h2 "Sail performance")
       (:table
	(:tr
	 (:td "Solar force / gravity")
	 (:td (:input :type "number" :min "0" :max "2" :step "0.05" :value "0.1" :name "lightness"))))
       (:h2 "Controls")
       (:div :id "controls"
	     (:div
	      (:table
	       (:tr
		(:td "Angle")
		(:td (:input :type "number" :min "-90" :max "90" :step "5" :value "0" :name "angles[]"))
		(:td "Duration")
		(:td (:input :type "number" :min "0" :max "10" :step "0.1" :value "1" :name "durations[]")))))))
      (:p (:div :id "addcontrol" (:b "Add new control")))
      (:p (:div :id "trajectory"))
      (:script :src "ssweb.js")))))
