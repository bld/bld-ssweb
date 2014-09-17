(asdf:load-system :hunchentoot)
(asdf:load-system :cl-who)
(asdf:load-system :parenscript)
(asdf:load-system :cl-fad)

(defpackage :ps-tutorial
  (:use :cl :hunchentoot :cl-who :parenscript :cl-fad))

(in-package :ps-tutorial)

(setf *js-string-delimiter* #\")

(start (make-instance 'easy-acceptor :port 4242))

(define-easy-handler (tutorial1 :uri "/tutorial1") ()
  (with-html-output-to-string (s)
    (:html
     (:head (:title "Parenscript tutorial: 1st example"))
     (:body (:h2 "Parenscript tutorial: 1st example")
            "Please click the link below." :br
            (:a :href "#" :onclick (ps (alert "Hello World"))
                "Hello World")))))

(define-easy-handler (tutorial2 :uri "/tutorial2") ()
  (with-html-output-to-string (s)
    (:html
     (:head
      (:title "Parenscript tutorial 2: 2nd example")
      (:script :type "text/javascript"
	       (str (ps
		      (defun greeting-callback ()
			(alert "Hello World")))))
      (:body
       (:h2 "Parenscript tutorial: 2nd example")
       (:a :href "#" :onclick (ps (greeting-callback))
	   "Hello World"))))))

(define-easy-handler (webgl1 :uri "/webgl1") ()
  (with-html-output-to-string (s)
    (:html
     (:head
      (:title "Parenscript WebGL Test"))
     (:body
      (:h1 "Parenscript WebGL Test")
      (:canvas :id "webGLCanvas" :width "400" :height "400")
      ))))

(defvar *slideshows* (make-hash-table :test 'equalp))

(defun add-slideshow (slideshow-name image-folder)
  (unless (gethash slideshow-name *slideshows*)
    (push (create-folder-dispatcher-and-handler
	   (format nil "/slideshow-images/~a/" slideshow-name)
	   image-folder)
	  *dispatch-table*))
  (setf (gethash slideshow-name *slideshows*)
	(mapcar (lambda (pathname)
		  (url-encode (format nil "~a.~a"
				      (pathname-name pathname)
				      (pathname-type pathname))))
		(list-directory image-folder))))

(add-slideshow "motivate" "/home/ben/Documents/pics/motivate/")

(defmacro/ps slideshow-image-uri (slideshow-name image-file)
  `(concatenate 'string "/slideshow-images/" ,slideshow-name "/" ,image-file))

(defun slideshow-handler ()
  (cl-ppcre:register-groups-bind (slideshow-name) ("/slideshows/(.*)" (script-name*))
    (let* ((images (gethash slideshow-name *slideshows*))
           (current-image-index (or (position (get-parameter "image") images :test #'equalp)
                                    0))
           (previous-image-index (max 0 (1- current-image-index)))
           (next-image-index (min (1- (length images)) (1+ current-image-index))))
      (with-html-output-to-string (s)
        (:html
         (:head
          (:title "Parenscript slideshow")
          (:script :type "text/javascript"
                   (str (ps* `(progn
                                (var *slideshow-name* ,slideshow-name)
                                (var *images* (array ,@images))
                                (var *current-image-index* ,current-image-index)))))
          (:script :type "text/javascript" :src "/slideshow.js"))
         (:body
          (:div :id "slideshow-container"
                :style "width:100%;text-align:center"
                (:img :id "slideshow-img-object"
                      :src (slideshow-image-uri slideshow-name
                                                (elt images current-image-index)))
                :br
                (:a :href (format nil "/slideshows/~a?image=~a"
                                  slideshow-name (elt images previous-image-index))
                    :onclick (ps (previous-image) (return false))
                    "Previous")
                " "
                (:a :href (format nil "/slideshows/~a?image=~a"
                                  slideshow-name (elt images next-image-index))
                    :onclick (ps (next-image) (return false))
                    "Next"))))))))

(push (create-prefix-dispatcher "/slideshows/" 'slideshow-handler)
      *dispatch-table*)

(define-easy-handler (js-slideshow :uri "/slideshow.js") ()
  (setf (content-type*) "text/javascript")
  (ps
    (define-symbol-macro fragment-identifier (@ window location hash))
    
    (defun show-image-number (image-index)
      (let ((image-name (aref *images* (setf *current-image-index* image-index))))
        (setf (chain document (get-element-by-id "slideshow-img-object") src)
              (slideshow-image-uri *slideshow-name* image-name)
              fragment-identifier
              image-name)))
    
    (defun previous-image ()
      (when (> *current-image-index* 0)
        (show-image-number (1- *current-image-index*))))
      
    (defun next-image ()
      (when (< *current-image-index* (1- (getprop *images* 'length)))
        (show-image-number (1+ *current-image-index*))))

    ;; this gives bookmarkability using fragment identifiers
    (setf (getprop window 'onload)
          (lambda ()
            (when fragment-identifier
              (let ((image-name (chain fragment-identifier (slice 1))))
                (dotimes (i (length *images*))
                  (when (string= image-name (aref *images* i))
                    (show-image-number i)))))))))