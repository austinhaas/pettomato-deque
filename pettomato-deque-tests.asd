(asdf:defsystem #:pettomato-deque-tests
  :description "Test suite for pettomato-deque."
  :author "Austin Haas <austin@pettomato.com>"
  :licence "MIT"
  :version "0.1.0"
  :depends-on (#:pettomato-deque
               #:fiveam)
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "tests")))))
