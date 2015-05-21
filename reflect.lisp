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
      (:div :id "info"
	    (:h1 (:a :href "#" :onclick "toggleDiv(\"infoText\"); return false;" "Sunlight Reflected by a Solar Sail"))
	    (:ul :id "infoText"
	     (:li "Light reflects off of the mirrored sail at the same angle that it hit it.")
	     (:li "The yellow boxes show the light absorbed and reflected by the sail.")
	     (:li "The arrows show the directions and amount of the absorbed and reflected light.")
	     (:li "Slightly less light is reflected than absorbed because of imperfections in the sail.")
	     (:li "Point the reflected light by tilting and rotating the sail."))
	    (:ul
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
	    (:p (:b (:a :href "/absorb-force.html" "Next: Force from Absorbed Sunlight")))
	    (:p (:b (:a :href "/absorb.html" "Previous: Sunlight Absorbed by a Sail")))
	    (:p (:b (:a :href "/index.html" "Home"))))
      (:img :id "up" :src "img/arrow_up.svg")
      (:img :id "down" :src "img/arrow_down.svg")
      (:img :id "left" :src "img/arrow_left.svg")
      (:img :id "right" :src "img/arrow_right.svg")
      (:div :id "plot")
      (:script
       :type "text/javascript"
       (str
	(ps
	  (toggle-div "infoText")
	  (toggle-div "helpText")
	  (defvar app (reflect))
	  ((@ app init))
	  ((@ app animate)))))
      (:script :src "js/googleanalytics.js")))))
