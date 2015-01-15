;; Sunlight reflected by a sail

(in-package :bld-ssweb)

(define-easy-handler (reflect :uri "/reflect.html") ()
  (with-html-output-to-string (s nil :indent t :prologue t)
    (:html
     (:meta :charset "utf-8")
     (:title "Sunlight Reflected by a Sail")
     (:link :href "sail.css" :rel "stylesheet")
     (:script :src "js/three.min.js")
     (:script :src "js/jquery-1.11.1.min.js")
     (:script :src "js/TrackballControls.js")
     (:script :type "text/javascript"
	      (str (ps (lisp *ps-lisp-library*))))
     (:script :src "js/lib.js")
     (:body
      (:div :id "info"
	    (:h1 "Sunlight Reflected by a Sail")
	    (:ul
	     (:li "Light reflects off of the mirrored sail at the same angle that it hit it.")
	     (:li "The yellow boxes show the light absorbed and reflected by the sail.")
	     (:li "Point the reflected light by tilting and rotating the sail.")
	     (:li "See how the width of the boxes changes when tilting the sail.")
	     (:li (:b "Challenge:") "Find the red target and reflect the light to hit it. Reload to change the target location."))
	    (:table
	     :id "tilt-controls"
	     (:tr (:td (:b "Sun incidence")) (:td :id "incidence" "0") (:td "deg"))
	     (:tr (:td (:b "Rotation about sun")) (:td :id "rotation" "0") (:td "deg"))
	     (:tr (:td (:b "Absorbed")) (:td :id "absorbed" "100") (:td "%"))))
      (:div :id "help"
	    (:h2 (:a :href "#" :onclick "toggleDiv(\"helpText\"); return false;" "Help"))
	    (:div :id "helpText"
		  (:li "Rotate the view by clicking or touching and dragging.")
		  (:li "To zoom:"
		       (:ul
			(:li "Roll a mouse wheel")
			(:li "Hold the middle mouse button and move up and down")
			(:li "Pinch and spread two fingers on a touch screen")))
		  (:li "To steer the sail:"
		       (:ul
			(:li "Up and down arrows to tilt")
			(:li "Right and left arrows to rotate")))))
      (:div :id "nav"
	    (:h2 (:a :href "http://www.patreon.com/bld" "Next: Help build it at Patreon"))
	    (:h2 (:a :href "/absorb.html" "Previous: Sunlight Absorbed by a Sail"))
	    (:h2 (:a :href "/index.html" "Home")))
            (:img :id "up" :src "img/arrow_up.svg")
      (:img :id "down" :src "img/arrow_down.svg")
      (:img :id "left" :src "img/arrow_left.svg")
      (:img :id "right" :src "img/arrow_right.svg")
      (:div :id "plot")
      (:script
       :type "text/javascript"
       (str
	(ps
	  (toggle-div "helpText")
	  (init document window)
	  ((@ camera position multiply-scalar) 2)
	  (init-orbit-controls)
	  (init-sail-parts)
	  (setq mirror (new (*mirror)))
	  ((@ sail add) mirror)
	  (setq corners (new (*corners mirror)))
	  (setq projection (new (*projection mirror)))
	  (setq incident (new (*incident)))
	  (setq normal (new (*normal mirror)))
	  (setq reflect (new (*reflect incident normal)))
	  (setq reflection (new (*reflection corners)))
	  (let ((tinc (* 5 (random (/ 360 5)) (/ pi 180)))
		(trot (* 5 (random (/ 360 5)) (/ pi 180)))
		(tdist (+ 20 (random 180))))
	    (setq target (new (*target tinc trot tdist 5))))
	  ((@ scene add) projection)
	  ((@ scene add) reflection)
	  ((@ scene add) target)
	  (setq tilt-update-fn #'update-tilt-reflect)
	  (init-tilt-controls)
	  (animate))))))))
