;;;; twoch.lisp

(in-package #:twoch)

(defparameter *style*
  (c:css
   '((body
      :display block
      :margin 8px
      :padding 0px
      :background "#C5AD99"
      :background-image "url(/static/bg.gif)"
      :font-size 13px
      :font-family "\"DejaVu Sans\", \"Liberation Sans\", Arial, Verdana, Tahoma, sans-serif"
      :padding "0 1.5%"
      :position relative
      :min-height 97%
      :color black)
     ("#title"
      :text-align center)
     (".header-inner > h1"
      :margin 0)
     (.header
      :margin-bottom 2em
      :border-width 1px
      :border-style outset
      :padding 6px
      :border-color gray
      :background "#CFC")
     (.header-inner
      :border-width 1px
      :border-style inset
      :padding 6px
      :border-color gray
      :display block)
     ("#thrdlist .links"
      :font-weight bold
      :font-size 14px
      :padding-bottom 5px)
     ("#thrdlist .thrd"
      :margin-right 0.3em))))

(defparameter *index-page*
  (with-html-string
      (with-html (:doctype)
        (:html
         (:head
          (:title "title"))
         (:style (:raw *style*))
         (:body
          (:div.header#title
           (:div.header-inner
            (:h1 "Programming")))
          (:div.header#thrdlist
           (:div.header-inner
            (:div.links
             (:raw
              (let ((links (list '("#newthrd" . "New Thread")
                                 '("/all" . "All Threads")
                                 '("/hot" . "Most Popular Threads"))))
                (format nil "~{~a~^ / ~}"
                        (mapcar (lambda (link)
                                  (with-html-string
                                      (:a :href (car link) (cdr link))))
                                links))))
             (:div
              (:p "test"))))))))))

(with-route ("/" params)
  (declare (ignore params))
  (html-response *index-page*))

(start :static-root "/twoch/static/"
       :address "0.0.0.0")
