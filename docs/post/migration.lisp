;;;
;;; Convert Muse sources for tapoueh.org into Markdown documents
;;;

(defpackage #:migration (:use #:cl))

(in-package #:migration)

(defparameter *current-article-path* nil)
(defparameter *current-article-cover-image* nil)

(defparameter *md-article-root*
  (asdf:system-relative-pathname :migration "./"))

(defparameter *static-image-root*
  (asdf:system-relative-pathname :migration "../../static/img/old/"))

(defparameter *static-pdf-root*
  (asdf:system-relative-pathname :migration "../../static/images/confs/"))

(defun walk-tapoueh-blog-directory (&optional (root tapoueh::*blog-directory*))
  "Walk Tapoueh blog directory and reproduce the content in *md-article-root*."
  (flet ((process-files (directory)
           (loop :for article :in (uiop:directory-files directory)
              :when (string= "muse" (pathname-type article))
              :do (let* ((relpath (uiop:enough-pathname article root))
                         (target-reldir (directory-namestring relpath))
                         (target-dir    (uiop:merge-pathnames*
                                         target-reldir
                                         *md-article-root*))
                         (target  (uiop:make-pathname*
                                   ;; we drop the day of month
                                   :name (subseq (pathname-name article) 3)
                                   :type "md"
                                   :defaults target-dir)))
                    (ensure-directories-exist target-dir)
                    (muse-to-markdown article target)))))
    (uiop:collect-sub*directories root
                                  (constantly t)
                                  (constantly t)
                                  #'process-files)))

(defun muse-to-markdown (article-pathname md-pathname &key (header t))
  "Read the ARTICLE-PATHNAME, and write a markdown document in MD-PATHNAME."
  (let* ((chap (tapoueh::muse-parse-chapeau article-pathname))
         (muse (tapoueh::muse-parse-article article-pathname))
         (url  (tapoueh::muse-url muse))
         (*print-pretty* t)
         (*current-article-path* (tapoueh::muse-pathname muse))
         (*current-article-cover-image*
          (maybe-copy-image
           (tapoueh::muse-extract-article-image-source chap)))
         (*current-article-cover-image*
          ;; in some cases we might want to actually change our mind.
          (or (maybe-pick-new-cover-image) *current-article-cover-image*)))
    (format t "~a~%" article-pathname)
    (with-open-file (s md-pathname
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create
                       :external-format :utf-8)
      (when header
        ;; Markdown header, in TOML format
        (format s "+++~%")
        (format s "date = \"~a\"~%" (tapoueh::parse-date
                                     (tapoueh::muse-date muse)))
        (format s "title = ~s~%" (tapoueh::muse-title muse))
        (format s "tags = [~{~s~^, ~}]~%" (tapoueh::muse-tags muse))
        (format s "~<categories = [~;~s~^,~_~i~s~;]~:>~%"
                (pick-categories (tapoueh::muse-tags muse)))
        (format s "thumbnailImage = ~s~%" *current-article-cover-image*)
        (format s "thumbnailImagePosition = ~s~%" "left")
        (format s "coverImage = ~s~%" *current-article-cover-image*)
        (format s "coverSize = ~s~%" "partial")
        (format s "coverMeta = ~s~%" "out")
        ;; let's pretty print here, with auto indenting and all
        (format s "~<aliases = [~;~s~^,~:@_~i~s~;]~:>~%"
                (list url (format nil "~a.html" url)))
        (format s "+++~%"))

      (unparse-muse-article-to-markdown s (tapoueh::muse-contents muse)))

    ;; and return the pathname of the markdown output file
    md-pathname))

(defun unparse-muse-article-to-markdown (stream contents)
  "Produce the Markdown document directly."
  (loop :for previous := nil :then node
     :for node :in contents
     :do (who-to-md stream node previous)))

(defun who-to-md (stream who previous)
  "Walk a cl-who tree and output its content as Markdown."
  (typecase who
    (string  (format stream "~a" (unencode-html-entities who)))
    (list
     (when who
       (destructuring-bind (tag . rest) who
         (ecase tag
           (:p      (format stream "~&~%")
                    (loop :for node :in rest
                       :do (who-to-md stream node who)))

           (:h1     (format stream "~%~%# ")
                    (loop :for node :in rest
                       :do (who-to-md stream node who)))

           (:h2     (format stream "~%~%## ")
                    (loop :for node :in rest
                       :do (who-to-md stream node who)))

           (:h3     (format stream "~%~%### ")
                    (loop :for node :in rest
                       :do (who-to-md stream node who)))

           (:pre    (format stream "~%~~~~~~~%")
                    ;; (:pre (:code ...))
                    (assert (eq :code (caar rest)))
                    (format stream "~a" (unencode-html-entities (cadar rest)))
                    (format stream "~~~~~~~%~%"))

           (:blockquote
            (format stream "~%")
            (let ((quote-string (with-output-to-string (s)
                                  ;; (:blockquote (:p ...)) we skip the
                                  ;; :p to get directly at the content
                                  (who-to-md s (cadar rest) who))))
              (with-input-from-string (quote-stream quote-string)
                (loop :for line := (read-line quote-stream nil nil)
                   :while line
                   :do (format stream "> ~a~%" line))))
            (format stream "~%"))

           ;; yes the internal representation is wrong, but we won't fix the
           ;; parser now, will we?
           (:ul     (format stream "~%")
                    (loop :for node :in rest :do (who-to-md stream node who)))

           (:li     (format stream "  - ")
                    (loop :for node :in rest :do (who-to-md stream node who))
                    (format stream "~%"))

           (:img    (unless (skip-node-p who)
                      (let ((img (maybe-copy-image who)))
                        (format stream "~%{{< image classes=~s src=~s >}}~%"
                                "fig50 fancybox dim-margin" img))))

           (:center ;; skip *legend* subtext of skipped images
                    (unless (or (skip-node-p who)
                                (skip-node-p previous)
                                (and (= 1 (length rest))
                                     (skip-node-p (car rest))))
                      (format stream "~%<center>")
                      (loop :for node :in rest :do (who-to-md stream node who))
                      (format stream "</center>~%")))

           (:a      (destructuring-bind (href link text) rest
                      (declare (ignore href)) ; :href
                      (typecase text
                        (string (format stream "[~a](~a)" text link))
                        (list   (format stream "
<div class=~s>
  <a href=~s>
    <img src=~s>
  </a>
</div>
"
                                        "figure dim-margin"
                                        (maybe-copy-pdf link)
                                        (maybe-copy-image text))))))


           (:code   (write-string "`" stream)
                    (loop :for node :in rest :do (who-to-md stream node who))
                    (write-string "`" stream))

           (:em     (write-string "*" stream)
                    (loop :for node :in rest :do (who-to-md stream node who))
                    (write-string "*" stream))

           (:strong (write-string "**" stream)
                    (loop :for node :in rest :do (who-to-md stream node who))
                    (write-string "**" stream))))))))

(defun tapoueh::tapoueh-list-articles-tagged (tag-name)
  "el cheap'o code toggle"
  (declare (ignore tag-name))
  nil)

(defun skip-node-p (node
                    &optional (cover *current-article-cover-image*))
  "Return a non-nil generalized boolean when we are going to skip NODE."
  (typecase node
    (string nil)
    (list
     (destructuring-bind (tag . rest) node
       (case tag
         (:img    (let ((img (maybe-copy-image node)))
                    (string= img cover)))

         (:center ;; skip (:center (:img ...)) when we skip the image anyway
          (when (eq :img (car rest))
            (skip-node-p rest))))))))

(defun maybe-copy-image (image
                         &optional (article-pathname *current-article-path*))
  "IMAGE is an absolute URL based at tapoueh.org, convert it to a pathname
   where to install it in the new website, and return its location to insert
   in the article variables."
  (destructuring-bind (img src url) image
    (declare (ignore img src))
    ;; url looks like "../../../images/wi-killservers.jpg"
    (let* ((source-image (uiop:merge-pathnames* url article-pathname))
           (nodir        (make-pathname :name (pathname-name source-image)
                                        :type (pathname-type source-image)))
           (target-image (uiop:merge-pathnames* nodir *static-image-root*))
           (location     (format nil "/img/old/~a" (uiop::unix-namestring nodir))))
      (unless (probe-file target-image)
        (uiop:copy-file source-image target-image))
      location)))

(defun maybe-copy-pdf (link
                       &optional (article-pathname *current-article-path*))
  "Ig the link happens to be to a local PDF file, then copy it over to
  static/pdf and generate a proper link to that new static resource."
  (let ((local-file (uiop:merge-pathnames* link article-pathname)))
    (if (and (probe-file local-file)
             (string= "pdf" (pathname-type local-file)))
        (let* ((nodir      (make-pathname :name (pathname-name local-file)
                                          :type (pathname-type local-file)))
               (target-pdf (uiop:merge-pathnames* nodir *static-pdf-root*))
               (location   (format nil "/images/confs/~a"
                                   (uiop:unix-namestring nodir))))
          (unless (probe-file target-pdf)
            (uiop:copy-file local-file target-pdf))

          ;; return the new link
          location)
        ;; otherwise that wasn't a local file PDF, just return exactly what
        ;; we got here in input.
        link)))

(defun unencode-html-entities (string)
  "Quick & dirty unencode feature..."
  (let ((result string))
    (loop
       :for (regexp . new) :in '(("&quot;" . "\"")
                                 ("&#039;" . "'")
                                 ("&lt;"   . "<")
                                 ("&gt;"   . ">")
                                 ("&amp;"  . "&")
                                 ("&#xA0;" . " ")
                                 ("&#xB5;" . "µ"))
       :do (setf result (cl-ppcre:regex-replace-all regexp result new))
       :finally (return result))))

(defun pick-categories (tag-list)
  "From a list of tags, build a category list."
  (cond ((member "Conferences" tag-list :test #'string-equal)
         (cond ((member "Meetup" tag-list :test #'string-equal)
                (list "Conferences" "Meetup"))
               ((member "PostgreSQL" tag-list :test #'string-equal)
                (list "Conferences" "PostgreSQL Confs"))
               ((member "PostgreSQLFr" tag-list :test #'string-equal)
                (list "Conferences" "PostgreSQLFr Confs"))
               ((member "Emacs" tag-list :test #'string-equal)
                (list "Conferences" "Emacs Confs"))
               (t
                (list "Conferences"))))

        ((member "pgloader" tag-list :test #'string-equal)
         (list "Projects" "pgloader"))

        ((and (member "prefix" tag-list :test #'string-equal)
              (member "PostgreSQL" tag-list :test #'string-equal))
         (list "Projects" "prefix"))

        ((member "switch-window" tag-list :test #'string-equal)
         (list "Projects" "switch-window"))

        ((member "pg_staging" tag-list :test #'string-equal)
         (list "Projects" "pg_staging"))

        ((member "preprepare" tag-list :test #'string-equal)
         (list "Projects" "preprepare"))

        ((member "debian" tag-list :test #'string-equal)
         (list "debian"))

        ((member "YeSQL" tag-list :test #'string-equal)
         (list "PostgreSQL" "YeSQL"))

        ((member "Catalogs" tag-list :test #'string-equal)
         (list "PostgreSQL" "Catalogs"))

        ((member "PostgreSQLFr" tag-list :test #'string-equal)
         (list "PostgreSQL" "PostgreSQLFr"))

        ((member "Extensions" tag-list :test #'string-equal)
         (list "PostgreSQL" "Extensions"))

        ((and (member "PostgreSQL" tag-list :test #'string-equal)
              (member "Encoding" tag-list :test #'string-equal))
         (list "PostgreSQL" "Encoding"))

        ((and (member "PostgreSQL" tag-list :test #'string-equal)
              (member "release" tag-list :test #'string-equal))
         (list "PostgreSQL" "Release"))

        ((and (member "PostgreSQL" tag-list :test #'string-equal)
              (member "Backups" tag-list :test #'string-equal))
         (list "PostgreSQL" "Backups and Recovery"))

        ((member "pgbouncer" tag-list :test #'string-equal)
         (list "PostgreSQL" "pgbouncer"))

        ((or (member "Skytools" tag-list :test #'string-equal)
             (member "PGQ" tag-list :test #'string-equal))
         (list "PostgreSQL" "Skytools"))

        ((member "muse" tag-list :test #'string-equal)
         (list "Software Programming" "Emacs Lisp" "Muse"))

        ((member "Common-Lisp" tag-list :test #'string-equal)
         (list "Software Programming" "Common Lisp"))

        ((intersection tag-list '("cssh" "ack" "grep" "mailq"
                                  "psql-linum-format")
                       :test #'string-equal)
         (list "Software Programming" "Emacs Lisp"))

        ((member "el-get" tag-list :test #'string-equal)
         (list "Emacs" "el-get"))

        ((member "Emacs" tag-list :test #'string-equal)
         (list "Emacs" "Emacs Tips"))

        (t
         (unless (= 1 (length tag-list))
          (format t "FIRST TAG: ~s~%" (first tag-list)))
         (list (first tag-list)))))

(defvar *new-cover-articles*
  '(("07-Open-World-Forum"                . "/img/owf-banner.jpg")
    ("16-AllYourBase"                     . "/confs/ayb-design.jpg")
    ("18-getting-out-of-sql_ascii-part-1" . "/img/encodings.png")
    ("23-getting-out-of-sql_ascii-part-2" . "/img/encodings.png")
    ("29-Postgres-Open"                   .  "/confs/pgopen.jpg")
    ("16-PostgreSQL-data-recovery"        . "/img/tazzine-barman-fondogrigio.png")
    ("10-PHP-Tour-La-Video"               . "/confs/php-tour-public.jpg")
    ("27-phptour-2014"                    . "/confs/php-tour-public.jpg")
    ("27-back-from-pgcon2010"             . "/img/pgcon2010.jpg")
    ("01-extensions-in-91"                . "/img/tom-lane-t-shirt.jpg")))

(defvar *new-cover-images*
  '(("/img/old/postgresql-elephant.small.png" . "/img/postgresql-512.jpg")))

(defun maybe-pick-new-cover-image (&optional
                                     (pathname *current-article-path*)
                                     (cover *current-article-cover-image*))
  "Return another cover image for given article."
  (let* ((name  (pathname-name pathname))
         (match
             (or
              (member name *new-cover-articles* :test #'string= :key #'car)
              (member cover *new-cover-images* :test #'string= :key #'car))))
    (when match
      (cdr (first match)))))
