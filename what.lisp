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

      (:div :id "intro"
	    (:h1 :id "intro0" (:a :href "#" :onclick "hideDiv(\"intro0\"); showDiv(\"intro1\"); return false;" "What is a Solar Sail?"))
	    (:h1 :id "intro1" (:a :href "#" :onclick "hideDiv(\"intro1\"); showDiv(\"intro2\"); return false;"
				  "This lesson explains what a solar sail is, and introduces the camera controls used throughout flight school."))
	    (:h1 :id "intro2" (:a :href "#" :onclick "hideDiv(\"intro2\"); showDiv(\"intro3\"); return false;"
				 "A solar sail is a large, lightweight mirror that is attached to a spacecraft."))
	    (:h1 :id "intro3" (:a :href "#" :onclick "hideDiv(\"intro\"); showDiv(\"info\"); showDiv(\"help\"); return false;"
				  "Sunlight pushes the mirror, which propels the spacecraft around the solar system and beyond."))
	    ;; Skip intro
	    (:h3 :id "skip" (:a :href "#" :onclick "hideDiv(\"intro\"); showDiv(\"info\"); showDiv(\"help\"); return false;" "Skip intro")))
      
      (:div :id "info"
	    (:h1 "What is a solar sail?")
	    (:h3 "Challenges:")
	    (:ul
	     (:li "Click on " (:em "Help") " and learn how the interface works. Click on it again to make it disappear.")
	     (:li "Zoom in on the center, zoom back out, look at the back of the sail, and find the sun.")))
      
      (:div :id "help"
	    (:h2 (:a :href "#" :onclick "toggleDiv(\"helpText\"); toggleDiv(\"info\"); toggleDiv(\"nav\"); return false;" "Help"))
	    (:div :id "helpText"
		  (:ul
		  (:li "Keyboard and Mouse:"
		       (:ul
			(:li "Zoom by rolling the mouse wheel up and down, or holding the middle mouse button and moving up and down.")
			(:li "Rotate the view by holding the left mouse button, and moving the mouse left, right, up, and down.")))
		  (:li "Touchscreen:"
		       (:ul
			(:li "Zoom in and out by putting two fingers on the screen and pinching or spreading them apart.")
			(:li "Rotate the view by sliding one finger left, right, up, or down.")))
		  (:li "Reload to see the introduction again."))))

      (:div :id "plot")

      (:div :id "nav"
	    (:p (:b (:a :href "/parts.html" "Next: Parts of a Solar Sail")))
	    (:p (:b (:a :href "/index.html" "Home"))))

      (:script 
       :type "text/javascript"
       (str
	(ps
	  (hide-div "helpText")
	  (hide-div "help")
	  (hide-div "intro1")
	  (hide-div "intro2")
	  (hide-div "intro3")
	  (hide-div "info")
	  (defvar app (what))
	  ((@ app init))
	  ((@ app animate)))))

      (:script :src "js/googleanalytics.js")))))
