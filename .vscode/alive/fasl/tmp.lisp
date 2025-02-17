;;;; twoch.lisp

(in-package #:twoch)

(defvar *new-instance* (not (probe-file "twoch.db")))

(mito:connect-toplevel :sqlite3 :database-name "twoch.db")

(mito:deftable boards ()
  ((id :col-type :integer :auto-increment t :primary-key t)
   (name :col-type :text :not-null t :unique t)
   (title :col-type :text :not-null t)))

(mito:deftable threads ()
  ((id :col-type :integer :auto-increment t :primary-key t)
   (board :references (boards id))
   (subject :col-type :text :not-null t)
   (email :col-type :text)
   (name :col-type :text :not-null t :default "Anonymous")
   (comment :col-type :text :not-null t)))

(defconstant +boards+
  '(("prog" . "Programming")
    ("math" . "Mathematics")))

(when *new-instance*
      (mito:ensure-table-exists 'boards)
      (mito:ensure-table-exists 'threads)
      (let ((b +boards+))
        (loop for (name . title) in b
              do (progn
                  (let ((dao (mito:find-dao 'boards :name name)))
                    (when (not dao)
                          (mito:insert-dao (make-instance 'boards :name name :title title))))))))

;; TODO: Replies table

(defparameter *style*
  (style:css
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
     (a
      :word-break break-all)
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
      :margin-right 0.3em
      :text-decoration underline)
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
     (textarea
      :max-width "89vw"
      :min-width "40vw"
      :min-height "20vh"
      :background-color "#f7f7f7"
      :border "1px solid #ababab")
     (.outer
      :background-color "#efefef"
      :margin-bottom 2em
      :border-width 1px
      :border-style outset
      :padding 6px
      :border-color gray)
     (.inner
      :border-width 1px
      :border-style inset
      :padding 6px
      :border-color gray
      :display block)
     (.thrdmenu
      :font-family "Mona, \"MS PGothic\", Osaka, Meiryo, sans-serif"
      :line-height 1.1
      :display inline)
     (".thrdmenu a"
      :float right
      :text-decoration initial
      :font-size 14px
      :margin-left 2px)
     (.subject
      :display block)
     (".subject a"
      :text-decoration none
      :color "#F00")
     (".subject h2"
      :display inline-block
      :margin 0
      :line-height 120%
      :font-size 1.5em
      :padding-bottom 8px
      :color "#F00")
     (.post
      :overflow auto
      :background-color "#efefef")
     ("h3.posthead"
      :font-size 1em
      :font-weight normal
      :margin 0)
     (".post .num, .post button"
      :font-weight bold
      :cursor pointer
      :border none
      :background none
      :padding 0)
     (".post .name"
      :font-weight bold
      :color green)
     (.body
      :margin ".5em 0 1em 2em"
      :word-wrap break-word
      :position relative)
     (.container
      :display block
      :padding 2px)
     (".container textarea"
      :max-width none
      :min-width 0
      :min-height 0
      :box-sizing border-box
      :width 100%
      :background-color transparent
      :border none
      :resize none
      :overflow auto
      :outline none
      :box-shadow none)
     (table
      :font-size 12px
      :margin-top 5px)
     ("textarea .reply"
      :box-sizing border-box)
     ("@media screen and (max-width: 480px)"
      (a
       :word-break break-all)))))

(defparameter *new-thread-box*
  (with-html-string
    (:div.header#newthrd
     (:div.header-inner :style (style:inline-css '(:padding-right 20px))
                        (:span :style (style:inline-css '(:font-size 24px)) "New Thread")
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
                                            :style (style:inline-css '(text-align right))
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
                                           :style (style:inline-css '(:width 100%)))))
                                 (:tr
                                  (:td)
                                  (:td :colspan 3
                                       (:textarea :name "comment"
                                                  :rows 8
                                                  :cols 72
                                                  :style (style:inline-css '(:width 100%))))))))))))

(defparameter *reply-thread-box*
  (with-html-string
    (:form :method "post"
           :action "/reply"
           (:table
            (:tbody
             (:tr
              (:td.label "Name:")
              (:td
               (:input :name "Name"
                       :value ""))
              (:td.label "Email:")
              (:td
               (:input :name "email"
                       :value ""))
              (:td.btns :colspan 2
                        :style (style:inline-css '(text-align right))
                        (:input.submit :type "submit"
                                       :value "Create New Thread")
                        (:input.submit :type "submit"
                                       :name "preview"
                                       :value "Preview")))
             (:tr
              (:td.postfieldleft)
              (:td :colspan 5
                   (:textarea.reply :name "comment" :rows 8 :cols 72 :style (style:inline-css '(:width 100%))))
              (:tr
               (:td :colspan 6
                    (:a :href "#" "Entire Thread")
                    " "
                    (:a :href "#" "Thread List")))))))))

(defun index-page (board header)
  (with-html-string
    (with-html (:doctype)
      (:html
       (:head
        (:title (format nil "Twoch - ~a" header))
       (:style (:raw *style*))
       (:link :rel "stylesheet"
              :href "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.15.6/styles/default.min.css")
       (:script :src "/static/texme.js"))
       (:body
        (:div.header#title
         (:div.header-inner
          (:h1 header)))
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
                                  (:a :href (format nil "/~a/~a" board (car link)) (cdr link))))
                              links)))))
          (:div
           (:raw
            (let ((threads (mito:select-dao 'threads
                             (sxql:order-by (:desc :created-at))
                             (sxql:limit 10)
                             (sxql:where (:= :board board)))))
              (format nil "~{~a~^ / ~}"
                      (if threads
                          (mapcar (lambda (thread)
                                    (with-slots (id subject) thread
                                      (with-html-string
                                        (:span.thread (:a :href (format nil "/thread/~a" id) (format nil "#~a: ~a" id subject))))))
                                  threads)
                          (list "No threads yet"))))))))
        (:raw
         (let ((threads (mito:select-dao 'threads
                          (sxql:order-by (:desc :updated-at))
                          (sxql:limit 10)
                          (sxql:where (:= :board board)))))
           (format nil "~{~a~^ / ~}"
                   (if threads
                       (let ((i 0))
                         (mapcar (lambda (thread)
                                   (incf i)
                                   (with-slots (id subject name email comment) thread
                                     (with-html-string
                                       (:div.outer
                                        (:div.inner
                                         (:div.thrdmenu
                                          (:a :href "#" "▼")
                                          (:a :href "#" "▲")
                                          (:a :href "#" "■"))
                                         (:div.subject
                                          (:b (format nil "[~a" i))
                                          ":"
                                          (:b "0]") ;; TODO: Number of comments
                                          (:h2
                                           (:a :href "#" subject)))
                                         (:div.post
                                          (:h3.posthead
                                           (:button.num :onclick "#" "1")
                                           " Name: "
                                           (:span.name (format nil " ~a " name))
                                           (:span.posttime "2014-04-01 16:16")) ;; TODO: `created-at` to timestamp
                                          (:div.body
                                           (:div.container
                                            (:textarea.texme :readonly t comment))))
                                         (:raw *reply-thread-box*))))))
                                 threads))
                       (list "No threads yet")))))
        (:raw *new-thread-box*))))))

(with-route ("/" params)
  (declare (ignore params))
  (with-html-string
    (with-html (:doctype)
      (:html
       (:head
        (:title "Twoch"))
       (:body
        (:raw
         (format nil "~{~a~^ / ~}"
           (loop for (name . title) in +boards+
                 collect (with-html-string
                           (:a :href (format nil "/~a" name) (format nil "/~a/ - ~a" name title)))))))))))

(defmacro create-board-routes ()
  `(progn
     ,@(mapcar (lambda (board)
                 (let ((name (car board)) (title (cdr board)))
                   `(set-route ,(format nil "/~a" name)
                              (lambda (params)
                                (declare (ignore params))
                                (html-response (index-page ,name ,title))))))
          +boards+)))

(create-board-routes)

(with-route ("/post" params :method :POST)
  (with-request-params params ((subject "subject")
                               (name "name")
                               (email "email")
                               (comment "comment"))
    (if (or (zerop (length comment))
            (zerop (length subject)))
        (html-response "Comment and Subject are required")
        (let ((name (if (zerop (length name)) "Anonymous" name)))
          (mito:insert-dao (make-instance 'threads :subject subject :name name :email email :comment comment))
          (string-response (format nil "~a ~a ~a ~a" subject name email comment))))))

(with-route ("/reply" params :method :POST)
  (with-request-params params ((name "name")
                               (email "email")
                               (comment "comment"))
    (let ((name (if (zerop (length name))
                    "Anonymous"
                    name)))
      ;; TODO: Insert reply ...
      )))

(start :static-root "/twoch/static/"
       :address "0.0.0.0")
