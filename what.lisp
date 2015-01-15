;;; What is a solar sail? Intro screen

(in-package :bld-ssweb)

(define-easy-handler (what :uri "/what.html") ()
  (with-html-output-to-string (s nil :indent t :prologue t)
    (:html
     (:meta :charset "utf-8")
     (:title "What is a Solar Sail?")
     (:link :href "sail.css" :rel "stylesheet")
     (:script :src "js/three.min.js")
     (:script :src "js/jquery-1.11.1.min.js")
     (:script :src "js/TrackballControls.js")
     (:script :type "text/javascript"
	      (str (ps (lisp *ps-lisp-library*))))
     (:script :src "js/lib.js")
     (:body
      (:div :id "screen"
	    (:div :id "info"
		  (:h1 "What is a Solar Sail?")
		  (:ul
		   (:li "Solar sails are large, lightweight mirrors that are pushed by the pressure of reflected sunlight to propel a spacecraft throughout the solar system and beyond.")
		   (:li "The following simulations let you practice the skills needed to steer and navigate a solar sail spacecraft.")
		   (:li (:b "Challenge:") "Zoom in on the center, look at the backside, and find the sun.")))
	    (:div :id "help"
		  (:h2 (:a :href "#" :onclick "toggleDiv(\"helpText\"); return false;" "Help"))
		  (:div :id "helpText"
			(:ul
			 (:li "Rotate the view by clicking or touching and dragging.")
			 (:li "To zoom:"
			      (:ul
			       (:li "Roll a mouse wheel")
			       (:li "Hold the middle mouse button and move up and down")
			       (:li "Pinch and spread two fingers on a touch screen"))))))
	    (:div :id "plot"))
      (:div :id "nav"
	    (:h2 (:a :href "/parts.html" "Next: Parts of a Solar Sail"))
	    (:h2 (:a :href "/index.html" "Home")))
      (:script 
       :type "text/javascript"
       (str
	(ps
	  (toggle-div "helpText")
	  (init document window)
	  (init-orbit-controls)
	  (init-sail-parts)
	  (animate))))))))
