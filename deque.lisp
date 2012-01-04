(in-package #:pettomato-deque)

;; utils

(defmacro incf-mod (place divisor)
  `(setf ,place (mod (1+ ,place) ,divisor)))

(defmacro decf-mod (place divisor)
  `(setf ,place (mod (1- ,place) ,divisor)))

;; deque

(defclass deque ()
  ((head-index :accessor head-index)
   (tail-index :accessor tail-index)
   (buffer :accessor buffer))
  (:documentation "A double ended queue."))

(defmethod initialize-instance :after ((deque deque) &key (size 20) (element-type t) &allow-other-keys)
  (assert (> size 1) (size) "Initial size of a deque must be greater than 1.")
  (setf (head-index deque) 0
        (tail-index deque) 0
        (buffer deque) (make-array (1+ size) :element-type element-type)))

(defgeneric push-front (queue value)
  (:documentation "Push value on to the front of queue. Returns queue."))
(defmethod push-front ((deque deque) value)
  (when (deque-full-p deque)
    (grow-deque deque))
  (move-head-back deque)
  (setf (aref (buffer deque) (head-index deque)) value)
  deque)

(defgeneric push-back (queue value)
  (:documentation "Push value on to the back of queue. Returns queue."))
(defmethod push-back ((deque deque) value)
  (when (deque-full-p deque)
    (grow-deque deque))
  (setf (aref (buffer deque) (tail-index deque)) value)
  (move-tail-forward deque)
  deque)

(defgeneric pop-front (queue)
  (:documentation "Removes the element at the front of the queue and returns it."))
(defmethod pop-front ((deque deque))
  (assert (not (empty-p deque)))
  (prog1
      (peek-front deque)
    (move-head-forward deque)))

(defgeneric pop-back (queue)
  (:documentation "Removes the element at the back of the queue and returns it."))
(defmethod pop-back ((deque deque))
  (assert (not (empty-p deque)))
  (prog1
      (peek-back deque)
    (move-tail-back deque)))

(defgeneric peek-front (queue)
  (:documentation "Returns the element at the front of the queue without removing it from the queue."))
(defmethod peek-front ((deque deque))
  (aref (buffer deque) (head-index deque)))

(defgeneric peek-back (queue)
  (:documentation "Returns the element at the back of the queue without removing it from the queue."))
(defmethod peek-back ((deque deque))
  (aref (buffer deque) (mod (1- (tail-index deque)) (length (buffer deque)))))

(defgeneric empty-p (queue)
  (:documentation "Returns a generalized boolean: true if the queue is empty, otherwise false."))
(defmethod empty-p ((deque deque))
  (= (head-index deque) (tail-index deque)))

(defgeneric element-count (queue)
  (:documentation "Returns the number of elements currently in the queue."))
(defmethod element-count ((deque deque))
  (let ((head-index (head-index deque))
        (tail-index (tail-index deque)))
    (cond
      ((= head-index tail-index)
       0)
      ((< head-index tail-index)
       (- tail-index head-index))
      ((> head-index tail-index)
       (- (+ (length (buffer deque)) tail-index)
          head-index)))))

(defgeneric nth-element (queue index)
  (:documentation "Accesses the element of the queue specified by index."))
(defmethod nth-element ((deque deque) index)
  (assert (< index (element-count deque)) nil "index must be < element-count")
  (aref (buffer deque) (mod (+ (head-index deque) index) (length (buffer deque)))))

(defgeneric (setf nth-element) (val queue index))
(defmethod (setf nth-element) (val (deque deque) index)
  (assert (< index (element-count deque)) nil "index must be < element-count")
  (setf
   (aref (buffer deque) (mod (+ (head-index deque) index) (length (buffer deque))))
   val))

(defmacro do-all-elements ((element deque) &body body)
  "Iterate through deque in the order that pop-front would return
elements, and bind each element to element in body. element is bound
using a symbol-macrolet so that the value referenced by element can be
changed using setf."
  (let ((index (gensym)))
    `(block nil
       (symbol-macrolet ((,element (aref (buffer ,deque) ,index)))
         (if (< (tail-index ,deque) (head-index ,deque))
             (progn
               (loop
                 for ,index upfrom (head-index ,deque) below (length (buffer ,deque))
                 do ,@body)
               (loop
                 for ,index upfrom 0 below (tail-index ,deque)
                 do ,@body))
             (loop
               for ,index from (head-index ,deque) below (tail-index ,deque)
               do ,@body))))))

(defun deque->list (deque)
  "Returns the elements of the queue as a list. The original queue is
not modified."
  (check-type deque deque)
  (let ((res nil))
    (do-all-elements (element deque)
      (push element res))
    (nreverse res)))

;; internal

(defun deque-full-p (deque)
  (= (mod (head-index deque) (length (buffer deque)))
     (mod (1+ (tail-index deque)) (length (buffer deque)))))

(defun grow-deque (deque)
  (let ((new-buffer (make-array (* (length (buffer deque)) 2)
                                :element-type (array-element-type (buffer deque)))))
    (let ((index 0))
      (do-all-elements (element deque)
        (setf (aref new-buffer index) element)
        (incf index))
      (setf (head-index deque) 0
            (tail-index deque) index
            (buffer deque) new-buffer))
    deque))

(defun move-head-forward (deque)
  (incf-mod (head-index deque) (length (buffer deque))))

(defun move-tail-forward (deque)
  (incf-mod (tail-index deque) (length (buffer deque))))

(defun move-head-back (deque)
  (decf-mod (head-index deque) (length (buffer deque))))

(defun move-tail-back (deque)
  (decf-mod (tail-index deque) (length (buffer deque))))

(defmethod print-object ((deque deque) stream)
  (print-unreadable-object (deque stream :type t :identity t)
    (format stream "~D" (element-count deque))))
