;;; Sunlight absorbed by a sail

(in-package :bld-ssweb)

(define-easy-handler (absorb :uri "/absorb.html") ()
  (with-html-output-to-string (s nil :indent t :prologue t)
    (:html
     (:meta :charset "utf-8")
     (:title "Sunlight Absorbed by a Sail")
     (:link :href "sail.css" :rel "stylesheet")
     (:script :src "js/three.min.js")
     (:script :src "js/jquery-1.11.1.min.js")
     (:script :src "js/TrackballControls.js")
     (:script :type "text/javascript"
	      (str (ps (lisp *ps-lisp-library*))))
     (:script :src "js/lib.js")
     (:body
      (:div :id "info"
	    (:h1 "Sunlight Absorbed by a Sail")
	    (:table
	     :id "tilt-controls"
	     (:tr (:td (:b "Sun incidence")) (:td :id "incidence" "0") (:td "deg"))
	     (:tr (:td (:b "Rotation about sun")) (:td :id "rotation" "0") (:td "deg"))
	     (:tr (:td (:b "Absorbed")) (:td :id "absorbed" "100") (:td "%")))
	    (:ul
	     (:li "The yellow box shows the sunlight that is absorbed by the sail.")
	     (:li "See how the volume of the box changes as the sail tilts (sun incidence), and the amount of sunlight absorbed changes.")
	     (:li "Compare the amount absorbed to the " (:em "cosine") " of the sun incidence angle.")))
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
	    (:h2 (:a :href "/parts.html" "Previous: Parts of a Solar Sail"))
	    (:h2 (:a :href "/index.html" "Home")))
      (:div :id "plot")
      (:script
       :type "text/javascript"
       (str
	(ps
	  (toggle-div "helpText")
	  (init document window)
	  (init-orbit-controls)
	  (init-sail-parts)
	  (setq mirror (new (*mirror)))
	  ((@ sail add) mirror)
	  (setq corners (new (*corners mirror)))
	  (setq projection (new (*projection mirror)))
	  ((@ scene add) projection)
	  (init-tilt-controls)
	  (animate))))))))
    