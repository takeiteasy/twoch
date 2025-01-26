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
      :margin-right 0.3em)
     ("input.submit"
      :width initial
      :font-size 12px)
     (form
      :display block
      :margin-top 0em)
     ("input textarea"
      :box-sizing border-box
      :font-size 12px
      :width 95%)
     (.label
      :text-align right)
     ("input:not(.submit)"
      :background-color "#f7f7f7"
      :border "1px solid #ababab"
      :width 95%)
     (table
      :table-layout auto
      :width 100%)
     (textarea
      :max-width "89vw"
      :min-width "40vw"
      :min-height "20vh"
      :background-color "#f7f7f7"
      :border "1px solid #ababab")
     ("@media screen and (max-width: 480px)"
      (a
       :word-break break-all)))))

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
              (:p "latest posts ...")))))
          (:div.header#newthrd
           (:div.header-inner :style (c:inline-css '(:padding-right 20px))
                              (:span :style (c:inline-css '(:font-size 24px)) "New Thread")
                              (:form :method "post"
                                     :action "/post"
                                     (:table
                                      (:tbody
                                       (:tr
                                        (:td.label "Subject:")
                                        (:td
                                         (:input :name "subject"
                                                 :value ""))
                                        (:td.btns :colspan 2
                                                  :style (c:inline-css '(text-align right))
                                                  (:input.submit :type "submit"
                                                                 :value "Create New Thread")
                                                  (:input.submit :type "submit"
                                                                 :name "preview"
                                                                 :value "Preview")))
                                       (:tr
                                        (:td.label "Name:")
                                        (:td
                                         (:input :name "name"
                                                 :value ""))
                                        (:td.label "Email:")
                                        (:td
                                         (:input :name "email"
                                                 :value ""
                                                 :style (c:inline-css '(:width 100%)))))
                                       (:tr
                                        (:td)
                                        (:td :colspan 3
                                             (:textarea :name "comment"
                                                        :rows 8
                                                        :cols 72
                                                        :style (c:inline-css '(:width 100%)))))))))))))))

(with-route ("/" params)
  (declare (ignore params))
  (html-response *index-page*))

(start :static-root "/twoch/static/"
       :address "0.0.0.0")
