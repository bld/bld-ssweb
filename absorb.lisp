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
	    (:ul
	     (:li "The sunlight hitting a sail is highest when the sail faces the sun.")
	     (:li "As the sail tilts, the sunlight absorbed is reduced.")))
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
	  (animate))))))))
    
