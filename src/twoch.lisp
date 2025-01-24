;;;; twoch.lisp

(in-package #:twoch)

(n:with-route ("/" params)
  (declare (ignore params))
  (n:html-response
   "<p>hello world!</p>"))

(defparameter *static-root*
  (merge-pathnames #P"static/"
		   (uiop:pathname-directory-pathname
		    (or *load-pathname*
			*compile-file-pathname*))))

(n:start :static-root *static-root*
	 :address "0.0.0.0")
