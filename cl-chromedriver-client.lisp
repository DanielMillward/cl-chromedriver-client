;;;; Make sure 1.) Chrome is installed, and 2.) ChromeDriver is on PATH

;;;; dex:get, dex:post
;;;; https://github.com/fukamachi/dexador/issues/81

(in-package #:cl-chromedriver-client)

(defparameter *TEST-URI* "http://localhost:4444")

(defun json_post ()
  (dex:post *TEST-URI*
          :headers '(("content-type" . "application/json"))
          :content (cl-json:encode-json-to-string `(("username" . ,"testuser")
                                  ("password" . ,"testpassword")))
          :verbose t))

;;; https://lispcookbook.github.io/cl-cookbook/os.html#capturing-standard-and-error-output
(defun check_for_chromedriver ()
  "Runs chromedriver --version to check if chromedriver is available. Throws an error if it fails."
  (handler-case
      (uiop:run-program (list "chromedriver" "--version"))
    (simple-error ()
      (error "Can't run chromedriver. Is it on PATH?"))))


(defun main ()
  (check_for_chromedriver))
