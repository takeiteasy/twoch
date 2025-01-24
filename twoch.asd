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
		 :clack)
    :pathname "src"
    :components ((:file "trivial-rfc-1123")
		 (:file "ninglex")
                 (:file "package")
                 (:file "twoch")))
