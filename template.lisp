(in-package :bld-ssweb)

(define-easy-handler (template-html :uri "/template.html") ()
  (with-html-output-to-string (s nil :indent t :prologue t)
    (:html
     (:head
      (:title "three.js template")
      (:meta :name "viewport" :content "width=device-width, user-scalable=no, minimum-scale=1.0, maximum-scale=1.0")
      (:script :type "text/javascript" :src "js/three.min.js")
      (:script :type "text/javascript" :src "js/template.js"))
     (:body
      (:div :id "target")))))
