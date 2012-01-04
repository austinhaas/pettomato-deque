(asdf:defsystem #:pettomato-deque
  :description "A set of double-ended queue implementations."
  :author "Austin Haas <austin@pettomato.com>"
  :licence "MIT"
  :version "0.1.0"
  :serial t
  :components ((:file "package")
               (:file "deque")
               (:file "ideque")))
