;;;; package.lisp

(defpackage #:cl-chromedriver-client
  (:use #:cl #:dexador #:cl-json)
  (:shadowing-import-from :dexador :delete :get))
