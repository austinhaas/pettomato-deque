(defpackage #:pettomato-deque
  (:use #:cl)
  (:export
   #:deque
   #:push-front
   #:push-back
   #:pop-front
   #:pop-back
   #:peek-front
   #:peek-back
   #:empty-p
   #:element-count
   #:nth-element
   #:do-all-elements
   #:deque->list
   ;; ideque only
   #:ideque
   #:grow-back-to
   #:expire-front-to))
