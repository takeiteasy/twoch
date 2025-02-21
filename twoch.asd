;;;; twoch.asd

(asdf:defsystem #:twoch
    :description "2ch clone"
    :author "George Watson <gigolo@hotmail.co.uk>"
    :license "GPLv3"
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
                 :jonathan
                 :mito
                 :cl-fad
                 :log4cl)
    :pathname "src"
    :components ((:file "ninglex")
                 (:file "package")
                 (:file "twoch")))
