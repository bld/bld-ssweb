(asdf:defsystem :bld-ssweb
  :name "bld-ssweb"
  :author "Benjamin L. Diedrich <ben@solarsails.info>"
  :license "MIT"
  :description "Solar Sail Web Application"
  :depends-on ("bld-utils" "bld-ode" "hunchentoot" "cl-who" "parenscript" "css-lite" "cl-json" "ht-simple-ajax" "cl-fad" "cl-csv")
  :serial t
  :components
  ((:file "package")
   (:file "prop")
   (:file "ajax")
   (:file "ssweb-js")
   (:file "ssweb")
   (:file "mirror")))
