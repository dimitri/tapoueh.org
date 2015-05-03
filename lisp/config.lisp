;;;
;;; Default config for the Tapoueh website.
;;;

(in-package #:tapoueh)

(defvar *config-filename* "~/.tapoueh.ini"
  "Where to store pgcharts configuration.")

(defparameter *pidfile* "~/.tapoueh.pid"
  "tapoueh pid file")

(defparameter *logfile* "/tmp/tapoueh.log"
  "Main logfile for tapoueh")

(defparameter *root-directory*
  (asdf:system-relative-pathname :tapoueh "../"))

(defparameter *blog-directory*
  (asdf:system-relative-pathname :tapoueh "../blog/"))

(defparameter *confs-directory*
  (asdf:system-relative-pathname :tapoueh "../confs/"))

(defparameter *pgsql-directory*
  (asdf:system-relative-pathname :tapoueh "../pgsql/"))

(defparameter *emacs-directory*
  (asdf:system-relative-pathname :tapoueh "../emacs/"))

(defparameter *html-directory*
  "/Users/dim/dev/temp/tapoueh.org/"
  "Where to publish the compiled static website")

(defparameter *header*
  (asdf:system-relative-pathname :tapoueh "../static/header.html"))

(defparameter *footer*
  (asdf:system-relative-pathname :tapoueh "../static/footer.html"))

(defparameter *conferences*
  (asdf:system-relative-pathname :tapoueh "../conferences.muse"))

(defparameter *projects*
  (asdf:system-relative-pathname :tapoueh "../projects.muse"))

(defparameter *pgsql*
  (asdf:system-relative-pathname :tapoueh "../pgsql.muse"))

(defparameter *emacs*
  (asdf:system-relative-pathname :tapoueh "../emacs.muse"))

(defparameter *port* 8042)
(defparameter *access-log-file* "/tmp/tapoueh-access.log")



;;;
;;; System integration: configuration file.
;;;
(defun expand-user-homedir-pathname (namestring)
  "Expand NAMESTRING replacing leading ~ with (user-homedir-pathname)"
  (cond ((or (string= "~" namestring) (string= "~/" namestring))
         (user-homedir-pathname))

        ((and (<= 2 (length namestring))
              (char= #\~ (aref namestring 0))
              (char= #\/ (aref namestring 1)))
         (uiop:merge-pathnames* (uiop:parse-unix-namestring (subseq namestring 2))
                                (user-homedir-pathname)))

        (t
         (uiop:parse-unix-namestring namestring))))

(defun set-config-filename (namestring)
  (setf *config-filename* (expand-user-homedir-pathname namestring)))

(defun read-config (&optional (filename *config-filename*))
  "Read the INI configuration file at *config-filename*"
  (when (probe-file filename)
    (let* ((ini  (ini:make-config))
           (conf (ini:read-files ini (list filename))))
      (when (ini:has-section-p conf "tapoueh")
        ;; root, sources for the website
        (when (ini:has-option-p conf "tapoueh" "root")
          (setf *root-directory*
                (expand-user-homedir-pathname
                 (ini:get-option conf "tapoueh" "root"))))

        ;; html, target where to compile the static version of the website
        (when (ini:has-option-p conf "tapoueh" "html")
          (setf *html-directory*
                (expand-user-homedir-pathname
                 (ini:get-option conf "tapoueh" "html"))))

        ;; listen-port
        (when (ini:has-option-p conf "tapoueh" "port")
          (setf *port*
                (parse-integer (ini:get-option conf "tapoueh" "port"))))

        ;; pidfile
        (when (ini:has-option-p conf "tapoueh" "pidfile")
          (setf *pidfile*
                (expand-user-homedir-pathname
                 (ini:get-option conf "tapoueh" "pidfile"))))

        ;; logfile
        (when (ini:has-option-p conf "tapoueh" "logfile")
          (setf *logfile*
                (expand-user-homedir-pathname
                 (ini:get-option conf "tapoueh" "logfile"))))

        (when (ini:has-option-p conf "tapoueh" "access-log")
          (setf *access-log-file*
                (expand-user-homedir-pathname
                 (ini:get-option conf "tapoueh" "access-log")))))

      ini)))

(defun write-config (&optional (pathname
                                (expand-user-homedir-pathname *config-filename*)))
  "Write current configuration into FILENAME."
  (with-open-file (s pathname
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create
                     :external-format :utf8)
    (let ((conf (ini:make-config)))
      (ini:add-section conf "tapoueh")
      (ini:set-option conf "tapoueh" "root" *root-directory*)
      (ini:set-option conf "tapoueh" "html" *html-directory*)
      (ini:set-option conf "tapoueh" "port"  *port*)
      (ini:set-option conf "tapoueh" "pidfile"
                      (uiop:native-namestring *pidfile*))
      (ini:set-option conf "tapoueh" "logfile"
                      (uiop:native-namestring *logfile*))
      (ini:set-option conf "tapoueh" "access-log"
                      (uiop:native-namestring *access-log-file*))

      (ini:write-stream conf s)

      (values pathname conf))))

(defun config-value (key)
  "Return configuration value for KEY."
  (cond ((string-equal key "root") *root-directory*)
        ((string-equal key "html") *html-directory*)
        ((string-equal key "port") *port*)
        ((string-equal key "pidfile")  (uiop:native-namestring *pidfile*))
        ((string-equal key "logfile")  (uiop:native-namestring *logfile*))
        ((string-equal key "access-log")
         (uiop:native-namestring *access-log-file*))))

(defun (setf config-value) (val key)
  "Set configuration variable NAME to NEWVALUE."
  (cond ((string-equal key "root")
         (let ((*root-directory* (expand-user-homedir-pathname val)))
           (write-config)))

        ((string-equal key "html")
         (let ((*html-directory* (expand-user-homedir-pathname val)))
           (write-config)))

        ((string-equal key "port")
         (let ((*port* (parse-integer val)))
           (write-config)))

        ((string-equal key "pidfile")
         (let ((*pidfile* (expand-user-homedir-pathname val)))
           (write-config)))

        ((string-equal key "logfile")
         (let ((*logfile* (expand-user-homedir-pathname val)))
           (write-config)))

        ((string-equal key "access-log")
         (let ((*access-log-file* (expand-user-homedir-pathname val)))
           (write-config)))

        (t (error "Unknown parameter ~s.~%" key)))
  val)


;;;
;;; pidfile reading
;;;
(defun read-pid (&optional (pidfile *pidfile*))
  "Read the server's pid from *pidfile* and return it as a string."
  (with-open-file (s pidfile) (read-line s)))

(defun kill-pid (pid &optional (sig "TERM"))
  "Send given SIG to Unix process PID."
  (multiple-value-bind (output error code)
      (uiop:run-program `("/bin/kill" ,(format nil "-~a" sig) ,pid)
                        :output :string
                        :error :string
                        :ignore-error-status t)
    (declare (ignore output error))
    (= 0 code)))
