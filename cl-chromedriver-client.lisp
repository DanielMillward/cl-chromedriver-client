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


(defmacro debug_call_driver (http_call)
"Makes the passed dexador call. On 4xx or 5xx errors, displays helpful information."
  `(handler-case
       ,http_call
     (dex:http-request-bad-request ()
       (error "400 Error: A bad request was made to ChromeDriver!"))
     (dex:http-request-failed (e)
    ;; For other 4xx or 5xx
    (format *error-output* "ChromeDriver returned ~D" (dex:response-status e)))))
    
;;; https://lispcookbook.github.io/cl-cookbook/os.html#capturing-standard-and-error-output
(defun start_chromedriver ()
  "Runs chromedriver --version to check if chromedriver is available. Throws an error if it fails."
  (handler-case
      (uiop:launch-program (list "chromedriver" "--port=4444"))
    (simple-error ()
      (error "Can't run chromedriver. Is it on PATH?"))
    (uiop/run-program:subprocess-error ()
      (error "Probably won't reach this because it just exits if port already bound."))))


(defun main ()
  (start_chromedriver))
