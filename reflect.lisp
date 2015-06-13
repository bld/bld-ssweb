;; Sunlight reflected by a sail

(in-package :bld-ssweb)

(define-easy-handler (reflect :uri "/reflect.html") ()
  (with-html-output-to-string (s nil :indent t :prologue t)
    (:html
     (:meta :charset "utf-8")
     (:meta :property "og:url" :content "http://flightschool.solarsails.info/reflect.html")
     (:meta :property "og:title" :content "Sunlight Reflected by a Solar Sail")
     (:meta :property "og:description" :content "Lesson 4: Learn how to reflect light off of a solar sail.")
     (:meta :property "og:image" :content "http://flightschool.solarsails.info/img/reflect_image.png")
     (:meta :property "og:site_name" :content "Solar Sail Flight School")
     (:title "Sunlight Reflected by a Solar Sail")
     (:link :href "sail.css" :rel "stylesheet")
     (:script :src "js/three.min.js")
     (:script :src "js/jquery-1.11.1.min.js")
     (:script :src "js/TrackballControls.js")
     (:script :type "text/javascript"
	      (str (ps (lisp *ps-lisp-library*))))
     (:script :src "js/decround.js")
     (:script :src "js/lib.js")
     (:body

      (:div :id "intro"
	    (:h1 :id "intro0" (:a :href "#" :onclick "hideDiv(\"intro0\"); showDiv(\"intro1\"); return false;" "Sunlight Reflected by a Solar Sail"))
	    (:h1 :id "intro1" (:a :href "#" :onclick "hideDiv(\"intro1\"); showDiv(\"intro2\"); return false;" "This lesson shows how to control the light that reflects off of a solar sail."))
	    (:h1 :id "intro2" (:a :href "#" :onclick "hideDiv(\"intro2\"); showDiv(\"intro3\"); return false;" "The yellow boxes show the portion of sunlight absorbed and reflected by the sail."))
	    (:h1 :id "intro3" (:a :href "#" :onclick "hideDiv(\"intro3\"); showDiv(\"intro4\"); return false;" "The arrows show the directions and amounts of sunlight absorbed and reflected by the sail."))
	    (:h1 :id "intro4" (:a :href "#" :onclick "hideDiv(\"intro\"); showDiv(\"info\"); showDiv(\"help\"); showDiv(\"arrows\"); return false;" "You can change where the reflected light shines by steering the sail."))
	    ;; Skip intro
	    (:h2 :id "skip" (:a :href "#" :onclick "hideDiv(\"intro\"); showDiv(\"info\"); showDiv(\"help\"); showDiv(\"arrows\"); return false;" "Skip intro")))

      (:div :id "info"
	    (:h1 "Sunlight Reflected by a Solar Sail")
	    (:h2 "Challenges:")
	    (:ul
	     (:li "Rotate the view to find the red target.")
	     (:li "Steer the sail to hit the target with the reflected sunlight. Reload to change the target location."))
	    (:table
	     :id "tilt-controls"
	     (:tr (:td (:b "Sun incidence")) (:td :id "incidence" "0") (:td "deg"))
	     (:tr (:td (:b "Rotation about sun")) (:td :id "rotation" "0") (:td "deg"))
	     (:tr (:td (:b "Absorbed")) (:td :id "absorbed" "100") (:td "%"))))

      (:div :id "help"
	    (:h2 (:a :href "#" :onclick "toggleDiv(\"helpText\"); return false;" "Help"))
	    (:div :id "helpText"
		  (:li "Keyboard and Mouse:"
		       (:ul
			(:li "Zoom in and out by rolling the mouse wheel up and down, or holding the middle mouse button and moving the mouse up and down.")
			(:li "Rotate the view by holding the left mouse button, and moving the mouse left, right, up, and down.")
			(:li "Tilt the sail by pressing the up and down arrow keys.")
			(:li "Rotate the sail by pressing the left and right arrow keys.")))
		  (:li "Touchscreen:"
		       (:ul
			(:li "Zoom in and out by putting two fingers on the screen and pinching or spreading them apart.")
			(:li "Rotate the view by sliding one finger left, right, up, or down.")
			(:li "Tilt the sail by tapping the yellow up and down arrows.")
			(:li "Rotate the sail by tapping the yellow left and right arrows.")))
		  (:li "Reload to see the introduction again and change the target.")))

      (:div :id "nav"
	    (:p (:b (:a :href "/absorb-force.html" "Next: Force from Absorbed Sunlight")))
	    (:p (:b (:a :href "/absorb.html" "Previous: Sunlight Absorbed by a Sail")))
	    (:p (:b (:a :href "/index.html" "Home"))))

      (:div :id "arrows"
	    (:img :id "up" :src "img/arrow_up.svg")
	    (:img :id "down" :src "img/arrow_down.svg")
	    (:img :id "left" :src "img/arrow_left.svg")
	    (:img :id "right" :src "img/arrow_right.svg"))
      
      (:div :id "plot")
      (:script
       :type "text/javascript"
       (str
	(ps
	  (hide-div "helpText")
	  (hide-div "help")
	  (hide-div "info")
	  (hide-div "arrows")
	  (hide-div "intro1")
	  (hide-div "intro2")
	  (hide-div "intro3")
	  (hide-div "intro4")
	  (defvar app (reflect))
	  ((@ app init))
	  ((@ app animate)))))
      (:script :src "js/googleanalytics.js")))))
