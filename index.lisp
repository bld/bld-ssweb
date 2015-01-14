;;; What is a solar sail? Intro screen

(in-package :bld-ssweb)

(define-easy-handler (index :uri "/index.html") ()
  (with-html-output-to-string (s nil :indent t :prologue t)
    (:html
     (:meta :charset "utf-8")
     (:title "Solar Sail Flight School")
     (:link :href "sail.css" :rel "stylesheet")
     (:script :src "js/three.min.js")
     (:script :src "js/jquery-1.11.1.min.js")
     (:script :src "js/TrackballControls.js")
     (:script :type "text/javascript"
	      (str (ps (lisp *ps-lisp-library*))))
     (:script :src "js/lib.js")
     (:body
      (:div :id "info" :style "width: 100%;"

	    (:h1 "Solar Sail Flight School")

	    (:p (:b "Learn to fly a solar sail through space on the pressure of sunlight."))
	    (:ul
	     (:li (:b "If you can see a solar sail and stars, then the school will work with with your browser. "))
	     (:li (:b "If not, you need a browser " (:a :href "http://get.webgl.org/" "with the WebGL feature enabled."))))
	    
	    (:h2 "Lessons")

	    (:ol
	     (:li (:h3 (:a :href "/what.html" "What is a solar sail?")))
	     (:li (:h3 (:a :href "/parts.html" "Parts of a solar sail"))))

	    (:br)
	    
	    (:p (:b (:a :href "http://www.patreon.com/bld"
			"Help build the school by supporting this project at "
			(:img :src "img/Ohmo4_white_logo_name.png" :width "98" :height "22" :style "vertical-align: middle;"))))
	    
	    (:p (:b "Learn more about solar sails at: " (:a :href "http://wiki.solarsails.info" "SolarSailWiki")))
	    (:p (:b "For questions, comments, and suggestions contact: " (:a :href "mailto:ben@solarsails.info" "ben@solarsails.info"))))
      
      (:div :id "plot")
      
      (:script 
       :type "text/javascript"
       (str
	(ps
	  (init document window)
	  ;;(init-orbit-controls)
	  (init-sail-parts))))))))