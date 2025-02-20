;;;; twoch.lisp

(in-package #:twoch)

(connect-toplevel :sqlite3 :database-name "twoch.db")

(deftable boards ()
  ((id :col-type :serial :primary-key t)
   (name :col-type :text :not-null t :unique t)
   (title :col-type :text :not-null t)))

(deftable threads ()
  ((id :col-type :serial :primary-key t)
   (board :references (boards id))
   (subject :col-type :text :not-null t)
   (email :col-type :text)
   (name :col-type :text :not-null t :default "Anonymous")
   (comment :col-type :text :not-null t)))

(deftable replies ()
  ((id :col-type :serial :primary-key t)
   (board :references (boards id))
   (thread :references (threads id))
   (name :col-type :text :not-null t :default "Anonymous")
   (email :col-type :text)
   (comment :col-type :text :not-null t)))

(ensure-table-exists 'boards)
(ensure-table-exists 'threads)
(ensure-table-exists 'replies)

(defun get-boards ()
  (mapcar (lambda (board)
            (cons (slot-value board 'name)
                  (slot-value board 'title)))
          (select-dao 'boards)))

(defparameter *static-root* "/twoch/static/")
(defparameter *uploads-dir* (merge-pathnames "uploads/" *static-root*))
(defparameter *max-upload-size* 10485760)
(defparameter *boards* (get-boards))

(defconstant +style+
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
     ("a:link, a:visited"
      :color blue)
     (.noshow
      :display none)
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
     (".post .num, .post button, .reply .num, .reply button"
      :font-weight bold
      :cursor pointer
      :border none
      :background none
      :padding 0)
     (".post .name, .reply .name"
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
     ("textarea .replybox"
      :box-sizing border-box)
     (.reply
      :overflow auto
      :background-color "#efefef")
     (.recent
      :margin "0.5em 0px")
     ("#boards"
      :text-align center)
     (.upload
      :background-color "transparent !important"
      :border "none !important"
      :width "100% !important")
     ("@media screen and (max-width: 480px)"
      (a
       :word-break break-all)))))

(defun new-thread-box (board-id)
  (with-html-string
    (:div.header#newthrd
     (:div.header-inner
      :style (style:inline-css '(:padding-right 20px))
      (:span :style (style:inline-css '(:font-size 24px)) "New Thread")
      (:form
       :method "post" :action "/post"
       :enctype "multipart/form-data"
       (:div.noshow
        (:input :name "board" :value board-id :type "hidden"))
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
                          :style (style:inline-css '(:width 100%)))))
        (:tr
          (:td.label "File:")
          (:td :colspan 3
               (:input.upload :name "file" :type "file" :multiple t))))))))))

(defun reply-thread-box (board-id thread-id)
  (with-html-string
    (:form
     :method "post" :action "/reply"
     :enctype "multipart/form-data"
     (:div.noshow
      (:input :name "board" :value board-id :type "hidden")
      (:input :name "thread" :value thread-id :type "hidden"))
     (:table
      (:tbody
       (:tr
        (:td.label "Name:")
        (:td
         (:input :name "name"
                 :value ""))
        (:td.label "Email:")
        (:td
         (:input :name "email"
                 :value ""))
        (:td.btns :colspan 2
                  :style (style:inline-css '(text-align right))
                  (:input.submit :type "submit"
                                 :value "Reply")
                  (:input.submit :type "submit"
                                 :name "preview"
                                 :value "Preview")))
       (:tr
        (:td.postfieldleft)
        (:td :colspan 5
             (:textarea.replybox
              :name "comment"
              :rows 8 :cols 72
              :style (style:inline-css '(:width 100%)))))
       (:tr
        (:td.label "File:")
        (:td :colspan 3)
        (:input.upload :type "file" :name "file")
        (:td :colspan 2
             ;; TODO: Fix broken links
             (:a :href "#" "Entire Thread")
             " "
             (:a :href "#" "Thread List"))))))))

(defmacro timestamp-to-string (timestamp)
  `(local-time:format-timestring nil ,timestamp :format '(:year "-" :month "-" :day " " :hour ":" :min ":" :sec)))

(defun replies-truncated (board-id thread-id)
  (let ((reply-count (count-dao 'replies :board board-id :thread thread-id)))
    (let ((html-out (if (> reply-count 5)
                        (with-html-string
                          (:div.recent
                           "The 5 newest replies are shown below."
                           (:br)
                           (:a :href "#" "Read this thread from the beginning.")))
                        "")))
      (let ((rpls (select-dao 'replies
                    (sql:where (:= :board board-id))
                    (sql:where (:= :thread thread-id))
                    (sql:order-by (:desc :created-at))
                    (sql:limit 5)))
            (i -1))
        (setf html-out
          (concatenate 'string html-out
            (format nil "~{~a~^~}"
              (mapcar (lambda (rpl)
                        (incf i)
                        (with-slots ((created-at mito.dao.mixin::created-at)
                                     id name comment) rpl
                          (with-html-string
                           (:div.reply
                            (:h3.posthead
                             (:button.num :onclick "#" (format nil "~a" (- (+ reply-count (if (> reply-count 5) 3 2)) (abs (- i reply-count)))))
                             " Name: "
                             (:span.name (format nil " ~a " name))
                             (:span.posttime (timestamp-to-string created-at)))
                            (:div.body
                             (:div.container
                              (:textarea.texme :readonly t comment)))))))
                  (reverse rpls)))))
        html-out))))

(defun board-links ()
  (format nil "~{~a~^ / ~}"
    (loop for (name . title) in *boards*
          collect (with-html-string
                    (:a :href (format nil "/~a" name) (format nil "/~a/ - ~a" name title))))))

(defun index-page (board-name header)
  (let ((brd (find-dao 'boards :name board-name)))
    (when (not brd)
          (string-response "Board not found"))
    (when (not (eql (slot-value brd 'title) header))
          (string-response "Board not found"))
    (with-html-string
      (with-html (:doctype)
        (:html
         (:head
          (:title (format nil "/~a/ - ~a" board-name header))
          (:style (:raw +style+))
          (:link :rel "stylesheet"
                 :href "https://unpkg.com/@highlightjs/cdn-assets@11.4.0/styles/default.min.css")
          (:script :src "/static/texme.js"))
          ;; TODO: Add script to search for backlinks
          ;; TODO: API/route to check backlinks client-side
         (:body
          (:div.header#title
           (:div.header-inner
            (:h1 header)))
          (:div.header#boards
           (:div.header-inner
            (:raw
             (board-links))))
          (:div.header#thrdlist
           (:div.header-inner
            (:div.links
             (:raw
              (let ((links '(("#newthrd" . "New Thread") ;; TODO
                             ("all" . "All Threads") ;; TODO
                             ("hot" . "Most Popular Threads")))) ;; TODO
                (format nil "~{~a~^ / ~}"
                  (mapcar (lambda (link)
                            (with-html-string
                              (:a :href (format nil "/~a/~a" board-name (car link)) (cdr link))))
                      links)))))
            (:div
             (:raw
              (let ((threads (select-dao 'threads
                               (sql:order-by (:desc :created-at))
                               (sql:limit 10)
                               (sql:where (:= :board (slot-value brd 'id))))))
                (format nil "~{~a~^ ~}"
                  (if threads
                      (let ((i 0))
                        (mapcar (lambda (thrd)
                                  (incf i)
                                  (with-html-string
                                    (:span.thread
                                     (:a :href (format nil "/thread/~a" (slot-value thrd 'id))
                                         (format nil "#~a: ~a (~a)" i (slot-value thrd 'subject) (+ (count-dao 'replies :board (slot-value brd 'id) :thread (slot-value thrd 'id)) 1))))))
                            threads))
                      (list ""))))))))
          (:raw
           (let ((threads (select-dao 'threads
                            (sql:order-by (:desc :updated-at))
                            (sql:limit 10)
                            (sql:where (:= :board (slot-value brd 'id))))))
             (format nil "~{~a~^~}"
               (if threads
                   (let ((i 0))
                     (mapcar (lambda (thread)
                               (incf i)
                               (with-slots ((created-at mito.dao.mixin::created-at)
                                            id subject name email comment) thread
                                 (with-html-string
                                   (:div.outer
                                    (:div.inner
                                     (:div.thrdmenu
                                      ;; TODO: Fix broken links
                                      (:a :href "#" "▼")
                                      (:a :href "#" "▲")
                                      (:a :href "#" "■"))
                                     (:div.subject
                                      (:b (format nil "[~a" i))
                                      ":"
                                      (:b (format nil "~a]" (+ (count-dao 'replies :board (slot-value brd 'id) :thread id) 1)))
                                      (:h2
                                       ;; TODO: Fix broken links
                                       (:a :href "#" subject)))
                                     (:div.post
                                      (:h3.posthead
                                       ;; TODO: Fix broken links
                                       (:button.num :onclick "#" "1")
                                       " Name: "
                                       (:span.name (format nil " ~a " name))
                                       (:span.posttime (timestamp-to-string created-at)))
                                      (:div.body
                                       (:div.container
                                        (:textarea.texme :readonly t comment))))
                                     (:raw (replies-truncated (slot-value brd 'id) id))
                                     (:raw (reply-thread-box (slot-value brd 'id) id)))))))
                         threads))
                   (list "")))))
          (:raw (new-thread-box (slot-value brd 'id)))))))))

(with-route ("/" params)
  (declare (ignore params))
  (with-html-string
    (with-html (:doctype)
      (:html
       (:head
        (:title "Twoch"))
       (:body
        (:h2 "Twoch")
        (:raw
         (board-links))
         (:hr)
         (:form
          :method "post" :action "/board"
          (:input :name "name" :value "" :type "text")
          (:input :name "title" :value "" :type "text")
          (:input :type "submit" :value "Create Board")))))))

(with-route ("/:name" params)
  (let ((brd (find-dao 'boards :name (cdr (assoc :name params)))))
    (if brd
        (index-page (slot-value brd 'name) (slot-value brd 'title))
        (string-response "Board not found"))))

(with-route ("/board" params :method :POST)
  (with-request-params params ((name "name")
                               (title "title"))
    (if (find-dao 'boards :name name)
        (string-response "Board already exists")
        (progn
         (insert-dao (make-instance 'boards
                       :name name
                       :title title))
         (setf *boards* (get-boards))
         (log:info "Board created" name title)
         (string-response "Board created successfully")))))

(with-route ("/post" params :method :POST)
  (with-request-params params ((board "board")
                               (subject "subject")
                               (name "name")
                               (email "email")
                               (comment "comment"))
    (when (or (zerop (length comment))
              (zerop (length subject)))
      (html-response "Comment and Subject are required"))
    (let ((brd (find-dao 'boards :id board)))
      (when (not brd)
            (string-response "Board not found"))
      (block ass-block
        (loop for row in params
              for name = (first row)
                when (string= name "file")
              ;; TODO: Move this to a separate function + use with reply
              do (destructuring-bind (stream filename content-type) (rest row) 
                       (progn
                        (when (> (length filename) 0)
                              (when (not stream)
                                    (return-from ass-block (string-response "No file uploaded")))
                              (let ((byte-count 0)
                                    (buffer (make-array 8192 :element-type '(unsigned-byte 8)))
                                    (full-path (merge-pathnames filename *uploads-dir*)))
                                (log:info "Uploading file" filename content-type byte-count)
                                (ensure-directories-exist full-path)
                                (handler-case
                                    (with-open-file (out full-path
                                                         :direction :output
                                                         :element-type '(unsigned-byte 8)
                                                         :if-exists :supersede)
                                      (loop for bytes-read = (read-sequence buffer stream)
                                            while (> bytes-read 0)
                                            do (progn
                                                (incf byte-count bytes-read)
                                                (when (> byte-count *max-upload-size*)
                                                      ;; TODO: Delete orphaned uploads
                                                      (return-from ass-block (string-response "File too large, 10mb limit")))
                                                (write-sequence buffer out :end bytes-read)))
                                      (log:info "Uploaded file" filename content-type byte-count))
                                  (error (e)
                                    ;; TODO: Delete orphaned uploads
                                    (log:error "Failed uploading" filename e)
                                    (return-from ass-block (string-response (format nil "Error uploading file: ~a" (princ-to-string e)))))))))))
        (let ((name (if (zerop (length name)) "Anonymous" name)))
          (insert-dao (make-instance 'threads
                        :board (slot-value brd 'id)
                        :subject subject
                        :name name
                        :email email
                        :comment comment))
        (string-response "Post created successfully"))))))
    
(with-route ("/reply" params :method :POST)
  (with-request-params params ((board "board")
                               (thread "thread")
                               (name "name")
                               (email "email")
                               (comment "comment")
                               (file "file"))
    ;; TODO: Handle file upload
    (when (zerop (length comment))
      (string-response "Comment is required"))
    (let ((name (if (zerop (length name)) "Anonymous" name))
          (brd (find-dao 'boards :id board)))
      (when (not brd)
            (string-response "Board not found"))
      (let ((thrd (find-dao 'threads :id thread :board (slot-value brd 'id))))
        (when (not thrd)
              (string-response "Thread not found"))
        (insert-dao (make-instance 'replies
                      :board (slot-value brd 'id)
                      :thread (slot-value thrd 'id)
                      :name name
                      :email email
                      :comment comment))
        (string-response "Reply created successfully")))))

(start :static-root *static-root*
       :address "0.0.0.0")
