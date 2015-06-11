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
		  (:ul :id "infoText" (:li "Solar sails are large, lightweight mirrors that are attached to spacecraft.")
		       (:li "Sunlight reflects off of the sails, which pushes the spacecraft around the solar system and beyond.")
		       (:li (:b "Challenges:")
			    (:ul
			     (:li "Click on " (:em "Help") " and learn how the interface works. Click on it again to make it disappear.")
			     (:li "Zoom in on the center, look at the back of the sail, and find the sun.")))))

	    (:div :id "help"
		  (:h2 (:a :href "#" :onclick "toggleDiv(\"helpText\"); return false;" "Help"))
		  (:div :id "helpText"
			(:ul
			 (:li "Click on the title, " (:em "What is a Solar Sail") ", to hide the text. Click again to show it.")
			 (:li "Rotate the view:"
			      (:ul
			       (:li "Mouse: click, hold, and move up, down, left, and right.")
			       (:li "Touchscreen: slide your finger up, down, left, and right.")))
			 (:li "Zoom in and out:"
			      (:ul
			       (:li "Mouse: roll the mouse wheel or hold the middle mouse button and move up and down.")
			       (:li "Touchscreen: with two fingers, pinch to zoom out, or spread them apart to zoom in."))))))
	    (:div :id "plot"))
      (:div :id "nav"
	    (:p (:b (:a :href "/parts.html" "Next: Parts of a Solar Sail")))
	    (:p (:b (:a :href "/index.html" "Home"))))
      (:script 
       :type "text/javascript"
       (str
	(ps
	  (toggle-div "helpText")
	  (defvar app (what))
	  ((@ app init))
	  ((@ app animate)))))
      (:script :src "js/googleanalytics.js")))))
