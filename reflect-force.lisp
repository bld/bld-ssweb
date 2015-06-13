;;; Force from reflected sunlight

(in-package :bld-ssweb)

(define-easy-handler (reflect-force :uri "/reflect-force.html") ()
  (with-html-output-to-string (s nil :indent t :prologue t)
    (:html
     (:meta :charset "utf-8")
     (:meta :property "og:url" :content "http://flightschool.solarsails.info/reflect-force.html")
     (:meta :property "og:title" :content "Force from Reflected Sunlight on a Solar Sail")
     (:meta :property "og:image" :content "http://flightschool.solarsails.info/img/reflect_force.png")
     (:meta :property "og:description" :content "Lesson 6: Learn how reflected sunlight pushes a solar sail.")
     (:meta :property "og:site_name" :content "Solar Sail Flight School")
     (:title "Force from Reflected Sunlight on a Solar Sail")
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
	    (:h1 :id "intro0" (:a :href "#" :onclick "hideDiv(\"intro0\"); showDiv(\"intro1\"); return false;" "Force from Reflected Sunlight on a Solar Sail"))
	    (:h1 :id "intro1" (:a :href "#" :onclick "hideDiv(\"intro1\"); showDiv(\"intro2\"); return false;" "This lesson shows how a solar sail accelerates when pushed by the reflected sunlight."))
	    (:h1 :id "intro2" (:a :href "#" :onclick "hideDiv(\"intro2\"); showDiv(\"intro3\"); return false;" "Reflected light pushes back on the sail in the opposite direction that it shines."))
	    (:h1 :id "intro3" (:a :href "#" :onclick "hideDiv(\"intro3\"); showDiv(\"intro4\"); return false;" "The " (:span :class "force" "force") " is proportional to the amount of " (:span :class "light" "light") " reflected."))
	    (:h1 :id "intro4" (:a :href "#" :onclick "hideDiv(\"intro4\"); showDiv(\"intro5\"); return false;" "Tilting the sail reduces the amount of sunlight reflected, and changes the direction of reflection."))
	    (:h1 :id "intro5" (:a :href "#" :onclick "hideDiv(\"intro\"); showDiv(\"info\"); showDiv(\"help\"); showDiv(\"arrows\"); return false;" "The arrows show the direction and amount of:"
				  (:br) (:span :class "light" "Sunlight")
				  (:br) (:span :class "force" "Acceleration & force")
				  (:br) (:span :class "velocity" "Velocity & speed")))
	    ;; Skip intro
	    (:h3 :id "skip" (:a :href "#" :onclick "hideDiv(\"intro\"); showDiv(\"info\"); showDiv(\"help\"); showDiv(\"arrows\"); return false;" "Skip intro")))

      (:div :id "info"
	    (:h1 "Force from Reflected Sunlight")
	    (:h3 "Challenges:")
	    (:ul
	     ;;(:li "Click on " (:em "Help") " and learn how the interface works. Click on it again to make it disappear.")
	     (:li "Rotate and zoom the view to find the red target.")
	     (:li "Steer the sail so the " (:span :class "force" "force") " points towards the target.")
	     (:li "Hit the spacebar or click \"Continue\" to start, and fly the sail into the target."))
	    (:table
	     :id "tilt-controls"
	     (:tr (:td (:b "Sun incidence")) (:td :id "incidence" "0") (:td "deg"))
	     (:tr (:td (:b "Rotation about sun")) (:td :id "rotation" "0") (:td "deg"))
	     (:tr (:td :class "force" (:b "Acceleration")) (:td :id "accel" "1") (:td "mm/s" (:sup "2")))
	     (:tr (:td :class "velocity" (:b "Speed")) (:td :id "speed" "0") (:td "mm/s"))
	     (:tr (:td (:b "Distance")) (:td :id "distance" "0") (:td "m"))
	     (:tr (:td (:b "Time") "(100X)") (:td :id "elapsed" "0") (:td "min:sec"))))

      (:div :id "help"
	    (:h2 (:a :href "#" :onclick "toggleDiv(\"helpText\"); toggleDiv(\"info\"); toggleDiv(\"nav\"); toggleDiv(\"arrows\"); return false;" "Help"))
	    (:div :id "helpText"
		  (:li "The sail resets after travelling 100 m, or hitting the \"Reset\" button.")
		  (:li "Hit the spacebar or \"Continue/Pause\" to start/stop the animation.")
		  (:li "Keyboard and Mouse:"
		       (:ul
			(:li "Zoom by rolling the mouse wheel up and down, or holding the middle mouse button and moving up and down.")
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
	    (:button :id "pause" "Pause")
	    (:button :id "reset" "Reset")
	    (:p (:b (:a :href "/force.html" "Next: Force from Sunlight on a Solar Sail")))
	    (:p (:b (:a :href "/absorb-force.html" "Previous: Force from Absorbed Sunlight on a Solar Sail")))
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
	  (hide-div "intro1")
	  (hide-div "intro2")
	  (hide-div "intro3")
	  (hide-div "intro4")
	  (hide-div "intro5")
	  (hide-div "info")
	  (hide-div "arrows")
	  (defvar app (reflect-force))
	  ((@ app init))
	  ((@ app animate)))))

      (:script :src "js/googleanalytics.js")))))
