;;;; cl-chromedriver-client.asd

(asdf:defsystem #:cl-chromedriver-client
  :description "Describe cl-chromedriver-client here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:dexador #:cl-json)
  :components ((:file "package")
               (:file "cl-chromedriver-client")))
