;;;; Make sure 1.) Chrome is installed, and 2.) ChromeDriver is on PATH

;;;; dex:get, dex:post
;;;; https://github.com/fukamachi/dexador/issues/81

;;;; TODO: https://github.com/jlipps/simple-wd-spec?tab=readme-ov-file#error-codes
;;;; Put these error codes in debug_call_driver
;;;; Add parser for response body > Add switch case for each dex:http-response-x block with descriptors

;;;; TODO: Add headless option to session: https://webdriver.io/docs/capabilities/#run-browser-headless

(in-package #:cl-chromedriver-client)

(defparameter *TEST-URI* "http://localhost:4444")
(defparameter *CD-RUNNING-P* nil)

(defun main ()
  (start_chromedriver))

;;; https://lispcookbook.github.io/cl-cookbook/os.html#capturing-standard-and-error-output
(defun start_chromedriver ()
  "Runs chromedriver --version to check if chromedriver is available. Throws an error if it fails."
  (handler-case
      (uiop:launch-program (list "chromedriver" "--port=4444"))
    (simple-error ()
      (error "Can't run chromedriver. Is it on PATH?"))
    (uiop/run-program:subprocess-error ()
      (error "Probably won't reach this because it just exits if port already bound.")))
  (setf *CD-RUNNING-P* t))

(defun start-session ()
  "Starts a session with Chromedriver, starting Chromedriver if it's not started yet. Returns the session id."
  (unless *CD-RUNNING-P* (start_chromedriver))
  (extract_session_id_from_json 
     (json_post "/session" (session_json))))

(defmacro automate (base-url &body body)
  `(let ((base-url# ,base-url) (session-id (start-session)))
     ,@(mapcar (lambda (form)
                 (if (and (listp form) (eq (first form) 'go-to-page))
                     `(go-to-page base-url# session-id ,@(cdr form))
		      (error (format nil "Parameter ~a is invalid in the context of automate." form))))
               body)))

(defun go-to-page (base-url session-id path) 
  (format t "~a and also ~a with session id ~a" base-url path session-id))

(defun json_post (endpoint body)
  "Makes POST to Chromedriver on port 4444. Endpoint should be a string of format '/myendpoint' and body being a backtick'd object."
  (dex:post (format nil "~a~a" *TEST-URI* endpoint)
	    :headers '(("content-type" . "application/json"))
	    :content (cl-json:encode-json-to-string body)
	    :verbose t))

(defun json_get (sessionId endpoint body)
  (dex:get (format nil "~a/session/~a" *TEST-URI* endpoint)
	   :headers '(("content-type" . "application/json"))
	   :content (cl-json:encode-json-to-string body)
	   :verbose t))  


(defmacro debug_call_driver (http_call)
"Makes the passed dexador call. On 4xx or 5xx errors, displays helpful information."
  `(handler-case
       ,http_call
     ;; For 400
     (dex:http-request-bad-request ()
       (error "400 Error: A bad request was made to ChromeDriver!"))
     (dex:http-request-failed (e)
    ;; For other 4xx or 5xx
    (format *error-output* "ChromeDriver returned ~D" (dex:response-status e)))))

(defun session_json ()
  `(("capabilities" . ,(make-hash-table))))

(defun go_url_json (url)
  `(("url" . ,url)))

(defun extract_session_id_from_json (response_json)
  "Extracts session Id as string from Chromedriver response to POST /session in the form {'value': {'capabilities':{...},'sessionId': 'xxxxxx'}}"  (cdr (first (cdr (cdr (assoc :value (cl-json:decode-json-from-string response_json)))))))




;;;; Make call with (extract_session_id_from_json (json_post "/session" (session_json)))

;; .get -> opens the session browser to the specified URL
;; .title -> gets the title of the current tab?

;; Implement this API: https://www.selenium.dev/selenium/docs/api/py/webdriver_chrome/selenium.webdriver.chrome.webdriver.html#module-selenium.webdriver.chrome.webdriver
