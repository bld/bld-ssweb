(in-package :bld-ssweb)

(define-easy-handler (accel :uri "/accel.html") ()
  (with-html-output-to-string (s nil :indent t :prologue t)
    (:html
     (:meta :charset "utf-8")
     (:title "Simple sail acceleration test")
     (:link :href "sail.css" :rel "stylesheet")
     (:script :src "js/three.min.js")
     (:script :src "js/jquery-1.11.1.min.js")
     (:script :src "js/TrackballControls.js")
     (:script :src "js/accel.js")
     (:body
      (:div :id "info"
	    (:h1 "Simple sail acceleration test"))
      (:div :id "help"
	    (:h2 (:a :href "#" :onclick "toggleDiv(\"helpText\"); return false;" "Help"))
	    (:div :id "helpText" "HELP TEXT HERE"))
      (:div :id "plot")
      (:script
       :type "text/javascript"
       (str
	(ps
	  (init)
	  (init-tilt-controls)
	  (animate))))))))
