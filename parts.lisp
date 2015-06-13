;;; Parts of a solar sail

(in-package :bld-ssweb)
  
(define-easy-handler (parts :uri "/parts.html") ()
  (with-html-output-to-string (s nil :indent t :prologue t)
    (:html
     (:meta :charset "utf-8")
     (:meta :property "og:url" :content "http://flightschool.solarsails.info/parts.html")
     (:meta :property "og:title" :content "Parts of a Solar Sail")
     (:meta :property "og:image" :content "http://flightschool.solarsails.info/img/solar_sail_flight_school.png")
     (:meta :property "og:description" :content "Lesson 2: Learn about the parts of a solar sail spacecraft.")
     (:meta :property "og:site_name" :content "Solar Sail Flight School")
     (:title "Parts of a Solar Sail")
     (:link :href "sail.css" :rel "stylesheet")
     (:script :src "js/three.min.js")
     (:script :src "js/jquery-1.11.1.min.js")
     (:script :src "js/TrackballControls.js")
     (:script :type "text/javascript"
	      (str (ps (lisp *ps-lisp-library*))))
     (:script :src "js/lib.js")

     (:body

      (:div :id "intro"
	    (:h1 :id "intro0" (:a :href "#" :onclick "hideDiv(\"intro0\"); showDiv(\"intro1\"); return false;"
				 "Parts of a Solar Sail"))
	    (:h1 :id "intro1" (:a :href "#" :onclick "hideDiv(\"intro\"); showDiv(\"info\"); showDiv(\"help\"); return false;"
				 "In this lesson, you will learn the parts of a solar sail spacecraft that make it work."))
	    ;; Skip intro
	    (:h3 :id "skip" (:a :href "#" :onclick "hideDiv(\"intro\"); showDiv(\"info\"); showDiv(\"help\"); return false;" "Skip intro")))

      (:div :id "info"
	    (:h1 "Parts of a Solar Sail")
	    (:h3 "Challenges:")
	    (:ul
	     ;;(:li "Click on " (:em "Help") " and learn how the interface works. Click on it again to make it disappear.")
	     (:li "Click on different areas of the sail to read about the 4 major parts."))
	    (:div :id "sails" :class "hidden"
		  (:h3 "Sails")
		  (:ul
		   (:li "The sails are large, thin, reflective sheets that are pushed by sunlight.")
		   (:li "Current designs use a thin film of space qualified plastic like Kapton that is coated with a mirrored layer of aluminum.")
		   (:li "For more information see: " (:a :target "_blank" :href "http://wiki.solarsails.info/index.php/Category:Sail_Film" "Sail Film"))))
	    (:div :id "booms" :class "hidden"
		  (:h3 "Booms")
		  (:ul
		   (:li "The booms are structural members that hold the sails flat so they can reflect sunlight without collapsing.")
		   (:li (:a :target "_blank" :href "http://wiki.solarsails.info/index.php/IKAROS" "IKAROS") " spins, using centripetal acceleration to hold the sail flat.")
		   (:li "For more information see: "
			(:ul
			 (:li (:a :target "_blank" :href "http://wiki.solarsails.info/index.php/Three-Axis_Stabilized" "Three-Axis Stabilized"))
			 (:li (:a :target "_blank" :href "http://wiki.solarsails.info/index.php/Category:Spin_Stabilized" "Spin Stabilized"))))))
	    (:div :id "bus" :class "hidden"
		  (:h3 "Spacecraft bus")
		  (:p "The spacecraft bus houses all the systems required to operate the sail and carry out the mission. It typically contains:")
		  (:ul
		   (:li "Flight computer")
		   (:li "Solar arrays and power system")
		   (:li "Radios and antennas for communication")
		   (:li "Scientific instruments and other payloads")
		   (:li "Navigation and steering sensors")))
	    (:div :id "vanes" :class "hidden"
		  (:h3 "Steering vanes")
		  (:ul
		   (:li "Steering vanes are small sails at the boom tips that can rotate.")
		   (:li "They push the main sail to point in the direction you want to go.")
		   (:li "They work like airplane flaps and ailerons to steer the sail. ")
		   (:li "For more information see: " (:a :target "_blank" :href "http://wiki.solarsails.info/index.php/Category:Attitude_Control" "Attitude Control")))))
      
      (:div :id "help"
	    (:h2 (:a :href "#" :onclick "toggleDiv(\"helpText\"); toggleDiv(\"info\"); toggleDiv(\"nav\"); return false;" "Help"))
	    (:div :id "helpText"
		  (:ul
		   (:li "Click or tap on the parts of the sail for a description.")
		  (:li "Keyboard and Mouse:"
		       (:ul
			(:li "Zoom by rolling the mouse wheel up and down, or holding the middle mouse button and moving up and down.")
			(:li "Rotate the view by holding the left mouse button, and moving the mouse left, right, up, and down.")))
		  (:li "Touchscreen:"
		       (:ul
			(:li "Zoom in and out by putting two fingers on the screen and pinching or spreading them apart.")
			(:li "Rotate the view by sliding one finger left, right, up, or down.")))
		  (:li "Reload to see the introduction again."))))
      
      (:div :id "nav" 
	    (:p (:b (:a :href "/absorb.html" "Next: Sunlight Absorbed by a Sail")))
	    (:p (:b (:a :href "/what.html" "Previous: What is a Solar Sail?")))
	    (:p (:b (:a :href "/index.html" "Home"))))

      (:div :id "plot")

      (:script 
       :type "text/javascript"
       (str
	(ps
	  (hide-div "helpText")
	  (hide-div "help")
	  (hide-div "intro1")
	  (hide-div "info")
	  (defvar app (parts))
	  ((@ app init))
	  ((@ app animate)))))

      (:script :src "js/googleanalytics.js")))))
