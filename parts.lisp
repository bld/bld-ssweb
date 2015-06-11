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
      (:div :id "info"
	    (:h1 (:a :href "#" :onclick "toggleDiv(\"infoText\"); return false;" "Parts of a Solar Sail"))
	    (:ul :id "infoText"
		 (:li (:b "Challenges:")
		      (:ul
		       (:li "Click on " (:em "Help") " and learn how the interface works. Click on it again to make it disappear.")
		       (:li "Try to find the 4 major parts of the sail by clicking on different areas. Read about each one."))))
	    (:div :id "sails" :class "hidden"
		  (:h2 "Sails")
		  (:ul
		   (:li "The sails are large, thin, reflective sheets that are pushed by sunlight.")
		   (:li "Current designs use a thin film of space qualified plastic like Kapton that is coated with a mirrored layer of aluminum.")
		   (:li "For more information see: " (:a :target "_blank" :href "http://wiki.solarsails.info/index.php/Category:Sail_Film" "Sail Film"))))
	    (:div :id "booms" :class "hidden"
		  (:h2 "Booms")
		  (:ul
		   (:li "Something needs to hold the sails flat so they don't collapse under the gentle pressure of sunlight.")
		   (:li "Many sail designs use structural booms in a kite-shape to keep the sail flat against the gentle pressure of sunlight.")
		   (:li (:a :target "_blank" :href "http://wiki.solarsails.info/index.php/IKAROS" "IKAROS") " spins, using centripetal acceleration to hold the sail flat.")
		   (:li "For more information see: "
			(:ul
			 (:li (:a :target "_blank" :href "http://wiki.solarsails.info/index.php/Three-Axis_Stabilized" "Three-Axis Stabilized"))
			 (:li (:a :target "_blank" :href "http://wiki.solarsails.info/index.php/Category:Spin_Stabilized" "Spin Stabilized"))))))
	    (:div :id "bus" :class "hidden"
		  (:h2 "Spacecraft bus")
		  (:p "The spacecraft bus houses all the systems required to operate the sail and carry out the mission. It typically contains:")
		  (:ul
		   (:li "Flight computer")
		   (:li "Solar arrays and power system")
		   (:li "Radios and antennas for communication")
		   (:li "Scientific instruments and other payloads")
		   (:li "Navigation and steering sensors")))
	    (:div :id "vanes" :class "hidden"
		  (:h2 "Steering vanes")
		  (:ul
		   (:li "Steering vanes are small sails at the boom tips that can rotate.")
		   (:li "They push the main sail to point in the direction you want to go.")
		   (:li "They work like airplane ailerons and trim tabs to steer and hold the orientation of the sail. ")
		   (:li "For more information see: " (:a :target "_blank" :href "http://wiki.solarsails.info/index.php/Category:Attitude_Control" "Attitude Control")))))
      (:div :id "help"
	    (:h2 (:a :href "#" :onclick "toggleDiv(\"helpText\"); return false;" "Help"))
	    (:div :id "helpText"
		  (:ul
		   (:li "Click or tap on the parts of the sail for a description.")
		   (:li "Click on the title, " (:em "Parts of a Solar Sail") ", to hide the text. Click again to show it.")
		   (:li "Rotate the view:"
			(:ul
			 (:li "Mouse: click, hold, and move up, down, left, and right.")
			 (:li "Touchscreen: slide your finger up, down, left, and right.")))
		   (:li "Zoom in and out:"
			(:ul
			 (:li "Mouse: roll the mouse wheel or hold the middle mouse button and move up and down.")
			 (:li "Touchscreen: with two fingers, pinch to zoom out, or spread them apart to zoom in."))))))
      (:div :id "nav" 
	    (:p (:b (:a :href "/absorb.html" "Next: Sunlight Absorbed by a Sail")))
	    (:p (:b (:a :href "/what.html" "Previous: What is a Solar Sail?")))
	    (:p (:b (:a :href "/index.html" "Home"))))
      (:div :id "plot")
      (:script 
       :type "text/javascript"
       (str
	(ps
	  (toggle-div "helpText")
	  (defvar app (parts))
	  ((@ app init))
	  ((@ app animate)))))
      (:script :src "js/googleanalytics.js")))))
