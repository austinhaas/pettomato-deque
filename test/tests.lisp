(in-package #:pettomato-deque-tests)

(defun run-test (test-spec)
  "Same as run!, but adds an extra message."
  (format t "~&Testing: ~S" test-spec)
  (run! test-spec))

(defun run-tests ()
  (run-test 'deque-suite)
  (run-test 'ideque-suite))

;; Many tests apply equally to the different deques, so here are some
;; parameterized tests.

(defun test-empty (type)
  (let ((q (make-instance type)))
    (is (typep q type))
    (is (empty-p q))
    (signals error (pop-front q))
    (signals error (pop-back q))
    (signals error (nth-element q 0))
    (signals error (setf (nth-element q 0) 0))
    (is (= 0 (element-count q)))
    (is (null (deque->list q)))))

(defun test-normal (type)
  (let ((q (make-instance type)))
    (is (typep q type))
    (is (eq q (push-front q #\B)))
    (is (eq q (push-front q #\A)))
    (is (eq q (push-back q #\C)))
    (is (eq q (push-back q #\D)))
    (is (not (empty-p q)))
    (is (= 4 (element-count q)))
    (is (char= #\A (pop-front q)))
    (is (char= #\B (pop-front q)))
    (is (char= #\D (pop-back q)))
    (is (char= #\C (pop-back q)))
    (is (empty-p q))
    (is (= 0 (element-count q)))))

(defun test-nth (type)
  (let ((q (make-instance type)))
    (is (typep q type))
    (signals error (setf (nth-element q 0) 1))
    (push-back q #\A)
    (push-back q #\B)
    (push-back q #\C)
    (push-back q #\D)
    (is (char= #\A (nth-element q 0)))
    (is (char= #\B (nth-element q 1)))
    (is (char= #\C (nth-element q 2)))
    (is (char= #\D (nth-element q 3)))
    (is (char= #\E (setf (nth-element q 0) #\E)))
    (is (char= #\F (setf (nth-element q 1) #\F)))
    (is (char= #\G (setf (nth-element q 2) #\G)))
    (is (char= #\H (setf (nth-element q 3) #\H)))
    (is (char= #\E (nth-element q 0)))
    (is (char= #\F (nth-element q 1)))
    (is (char= #\G (nth-element q 2)))
    (is (char= #\H (nth-element q 3)))
    (is (= 4 (element-count q)))
    (is (char= #\E (pop-front q)))
    (is (char= #\F (pop-front q)))
    (is (char= #\G (pop-front q)))
    (is (char= #\H (pop-front q)))))

(defun test-oversized (type)
  ;; Tests that the queue will automatically grow to accomodate more
  ;; elements than were originally indicated.
  (let ((q (make-instance type :size 4)))
    (is (typep q type))
    (is (eq q (push-front q #\C)))
    (is (eq q (push-front q #\B)))
    (is (eq q (push-front q #\A)))
    (is (eq q (push-back q #\D)))
    (is (eq q (push-back q #\E)))
    (is (eq q (push-back q #\F)))
    (is (not (empty-p q)))
    (is (= 6 (element-count q)))
    (is (char= #\A (pop-front q)))
    (is (char= #\B (pop-front q)))
    (is (char= #\C (pop-front q)))
    (is (char= #\F (pop-back q)))
    (is (char= #\E (pop-back q)))
    (is (char= #\D (pop-back q)))
    (is (empty-p q))
    (is (= 0 (element-count q)))
    (dotimes (i 25)
      (is (eq q (push-back q i))))
    (is (= 25 (element-count q)))
    (dotimes (i 25)
      (is (= i (pop-front q))))))

(defun test-iteration (type)
  (let ((q (make-instance type)))
    (dotimes (i 50)
      (push-back q i))
    (do-all-elements (element q)
      (setf element (+ element 100)))
    (is (equal (loop for i from 0 below 50 collecting (+ 100 i))
               (deque->list q)))))

(def-suite deque-suite :description "deque suite")

(in-suite deque-suite)

(test basic
  (finishes (make-instance 'deque)))

(test empty
  (test-empty 'deque))

(test normal
  (test-normal 'deque))

(test nth-element
  (test-nth 'deque))

(test oversized
  (test-oversized 'deque))

(test iteration
  (test-iteration 'deque))

(def-suite ideque-suite :description "ideque suite")

(in-suite ideque-suite)

(test basic
  (finishes (make-instance 'ideque)))

(test empty
  (test-empty 'ideque))

(test normal
  (test-normal 'ideque))

(test nth-element
  (test-nth 'ideque))

(test oversized
  (test-oversized 'ideque))

(test iteration
  (test-iteration 'ideque))

(test ideque-grow
  (let ((q (make-instance 'ideque :size 5)))
    (signals error (grow-back-to q -1 0))
    (is (eq q (grow-back-to q 9 #\A)))
    (is (equal (loop repeat 10 collect #\A)
               (deque->list q))))
  ;; Once more with some elements already in the queue.
  (let ((q (make-instance 'ideque :size 5)))
    (push-back q #\A)
    (push-back q #\A)
    (grow-back-to q 3 #\B)
    (push-back q #\C)
    (is (equal (list #\A #\A #\B #\B #\C)
               (deque->list q)))))

(test ideque-expire
  (let ((q (make-instance 'ideque :size 5)))
    (signals error (expire-front-to q -1))
    (dotimes (i 10)
      (push-back q i))
    (is (eq q (expire-front-to q 5)))
    (is (equal (loop for i from 5 below 10 collect i)
               (deque->list q)))
    ;; Test expiring beyond the queue's upper bound (which is legal).
    (is (eq q (expire-front-to q 100)))
    (is (empty-p q))))

(test ideque-indexing
  (let ((q (make-instance 'ideque)))
    (dotimes (i 20)
      (push-back q (+ i 100)))
    (dotimes (i 20)
      (is (= (+ i 100) (nth-element q i))))
    (expire-front-to q 10)
    (dotimes (i 10)
      (signals error (nth-element q q)))
    (loop for i from 10 below 20 do
      (is (= (+ i 100) (nth-element q i)))))
  (let ((q (make-instance 'ideque)))
    (push-front q -100)
    (push-front q -200)
    (push-back q 0)
    (push-back q 100)
    (is (= -100 (nth-element q -1)))
    (is (= -200 (nth-element q -2)))
    (is (= 0 (nth-element q 0)))
    (is (= 100 (nth-element q 1)))
    (is (equal '(-200 -100 0 100)
               (deque->list q)))))
