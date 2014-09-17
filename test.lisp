(in-package :bld-solarsailwebapp)

(define-easy-handler (test :uri "/test") ()
  (with-html-output-to-string (*standard-output* nil :prologue t :indent t)
    (:html
     (:head 
      (:title "Test")
      (:script :src "js/three.min.js")
      (:script :src "js/jquery-1.11.1.min.js")
      (:script :type "text/javascript" (str (ps (lisp *ps-lisp-library*)))))
     (:body
      (:h1 "Testing adding/deleting objects from scene")
      (:div :id "plot")
      (:p (:div :id "update" (:b "Update plot")))
      ;; Plotting & updating the trajectory
      (:script :type "text/javascript"
	       (str
		(ps
		  ;; Setup scene
		  (defvar *width* 640)
		  (defvar *height* 640)
		  (defvar *fov* 75)
		  (defvar *near* 1)
		  (defvar *far* 4000)
		  (defvar *origin* (new ((@ *three* *vector3) 0 0 0)))
		  (defvar renderer (new ((@ *three* *web-g-l-renderer))))
		  (renderer.set-size *width* *height*)
		  (defvar plot-div (document.get-element-by-id "plot"))
		  ((@ plot-div append-child) renderer.dom-element)
		  (defvar camera
		    (new ((@ *three* *perspective-camera) *fov* (/ *width* *height*) *near* *far*)))
		  (camera.position.set 0 0 20)
		  (camera.look-at *origin*)
		  (defvar scene (new ((@ *three* *scene))))
		  (scene.add camera)
		  (defvar object)
		  (defvar objects (list))
		  (defvar material)
		  (defvar materials (list))
		  (defvar geometry)
		  (defvar geometries (list))

		  (defun update-plot ()
		    ;; Clear objects
		    (when objects
		      (dotimes (i objects.length)
			(setq object (aref objects i))
			(scene.remove object)))
		    ;; Create new objects
		    (dotimes (i 10)
		      (setq material (new ((@ *three* *line-basic-material) (create color 0xff0000))))
		      (setq geometry (new ((@ *three* *circle-geometry) i 4)))
		      (geometry.vertices.shift)
		      (setq object (new ((@ *three* *line) geometry material)))
		      (materials.push material)
		      (geometries.push geometry)
		      (objects.push object)
		      (scene.add object)
		      (renderer.render scene camera)))

		  ((@ ($ document)
		      ready)
		   (update-plot))

		  ((@ ($ "#update")
		      click)
		   update-plot))))))))

