;;;; twoch.lisp

(in-package #:twoch)

(with-route ("/" params)
  (declare (ignore params))
  (html-response
   (with-output-to-string (*html*)
     (with-html
       (:doctype)
       (:html
         (:head
           (:title "title"))
         (:body (:h1 "Hello Common Lisp!")))))))

(defparameter *static-root*
  (merge-pathnames #P"static/"
                   (uiop:pathname-directory-pathname
                     (or *load-pathname*
                         *compile-file-pathname*))))

(start :static-root *static-root*
       :address "0.0.0.0")
