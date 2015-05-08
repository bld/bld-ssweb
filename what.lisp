;;; What is a solar sail? Intro screen

(in-package :bld-ssweb)

(define-easy-handler (what :uri "/what.html") ()
  (with-html-output-to-string (s nil :indent t :prologue t)
    (:html
     (:meta :charset "utf-8")
     (:meta :property "og:url" :content "http://flightschool.solarsails.info/what.html")
     (:meta :property "og:title" :content "What is a Solar Sail?")
     (:meta :property "og:image" :content "http://flightschool.solarsails.info/img/solar_sail_flight_school.png")
     (:meta :property "og:description" :content "Lesson 1: Learn what a solar sail is.")
     (:meta :property "og:site_name" :content "Solar Sail Flight School")
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
		  (:h1 (:a :href "#" :onclick "toggleDiv(\"infoText\"); return false;" "What is a Solar Sail?"))
		  (:ul :id "infoText"
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
	    (:p (:b (:a :href "/parts.html" "Next: Parts of a Solar Sail")))
	    (:p (:b (:a :href "/index.html" "Home"))))
      (:script 
       :type "text/javascript"
       (str
	(ps
	  (toggle-div "helpText")
	  (toggle-div "infoText")
	  (defvar app (what))
	  ((@ app init))
	  ((@ app animate)))))
      (:script :src "js/googleanalytics.js")))))
