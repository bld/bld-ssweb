(in-package :bld-solarsailwebapp)

(defun xytraj-to-canvas (xytraj width height &optional (padding 10))
  "Transform XY trajectory coordinates to plot on canvas"
  (let ((cx (- width (* 2 padding)))
	(cy (- height (* 2 padding))))
    (loop for (tm x y) in xytraj
       maximize x into xmax
       maximize y into ymax
       minimize x into xmin
       minimize y into ymin
       finally
	 (return
	   (let ((ox (/ width 2))
		 (oy (/ height 2))
		 (tx (- xmax xmin))
		 (ty (- ymax ymin))
		 (tox (/ (+ xmax xmin) 2))
		 (toy (/ (+ ymax ymin) 2)))
	     (let ((scale
		    (if (< (/ tx ty) (/ cx cy))
			(/ cy ty)
			(/ cx tx))))
	       (values
		(loop for (tm x y) in xytraj
		   collect (list tm
				 (+ (round (* (- x tox) scale)) ox)
				 (+ (round (* (- (- y toy)) scale)) oy))))))))))

	;; Plot 2D trajectory on canvas
	#+null(:canvas :id "SailTrajectory" :width width :height height)
	#+null(:script :type "text/javascript"
		 (str
		  (ps
		    (let* ((canvas (document.get-Element-By-Id "SailTrajectory"))
			   (context (canvas.get-Context "2d")))
		      (context.begin-Path)
		      (context.move-To (aref trajectory 0 1) (aref trajectory 0 2))
		      (loop for (tm x y) in trajectory
			 do (context.line-To x y))
		      (setf context.line-Width 1)
		      (context.stroke)))))
