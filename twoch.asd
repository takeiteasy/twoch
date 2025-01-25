;;;; twoch.asd

(asdf:defsystem #:twoch
    :description "Describe twoch here"
    :author "Your Name <your.name@example.com>"
    :license  "Specify license here"
    :version "0.0.1"
    :serial t
    :depends-on (:hunchentoot
                 :ningle
		         :lack
		         :clack
		         :spinneret
		         :parenscript
		         :spinneret/ps
		         :cl-css
		         :jonathan)
    :pathname "src"
    :components ((:file "ninglex")
                 (:file "package")
                 (:file "twoch")))
