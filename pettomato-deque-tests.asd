(asdf:defsystem #:pettomato-deque-tests
  :description "Test suite for pettomato-deque."
  :author "Austin Haas <austin@pettomato.com>"
  :licence "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (#:pettomato-deque
               #:fiveam)
  :components ((:file "test-package")
               (:file "tests")))
