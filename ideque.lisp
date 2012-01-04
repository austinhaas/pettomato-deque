(in-package #:pettomato-deque)

(defclass ideque (deque)
  ((min-index :initform 0 :accessor min-index)
   (max-index :initform 0 :accessor max-index))
  (:documentation "A double ended queue that allows indexing."))

(defmethod push-front :after ((deque ideque) value)
  (decf (min-index deque)))

(defmethod push-back :after ((deque ideque) value)
  (incf (max-index deque)))

(defmethod pop-front :after ((deque ideque))
  (incf (min-index deque)))

(defmethod pop-back :after ((deque ideque))
  (decf (max-index deque)))

(defmethod element-count ((deque ideque))
  (- (max-index deque) (min-index deque)))

(defmethod nth-element ((deque ideque) index)
  (assert (<= (min-index deque) index (1- (max-index deque))) (index)
          "index is out of range. (<= min:~A index:~A max:~A)" (min-index deque) index (max-index deque))
  (call-next-method deque (- index (min-index deque))))

(defmethod (setf nth-element) (val (deque ideque) index)
  (assert (<= (min-index deque) index (1- (max-index deque))) (index)
          "index is out of range. Must be >= ~A and < ~A" (min-index deque) (max-index deque))
  (call-next-method val deque (- index (min-index deque))))

(defgeneric grow-back-to (deque index value)
  (:documentation "Ensure that deque is large enough to be indexed at
index. If not, value will be pushed on until it is. If value is a
function, the function will be called and the returned value will be
used. Returns deque."))
(defmethod grow-back-to ((deque ideque) index value)
  (assert (>= index (min-index deque)) nil "Cannot grow-back-to ~A if min-index is ~A." index (min-index deque))
  (loop while (<= (max-index deque) index)
     do (push-back deque (if (functionp value)
                             (funcall value)
                             value)))
  deque)

(defgeneric expire-front-to (deque index)
  (:documentation "Move front up to index. It is permissible for index
to be greater than max-index; the queue will be cleared and min-index
and max-index will be set to index. Returns deque."))
(defmethod expire-front-to ((deque ideque) index)
  (assert (>= index (min-index deque)) nil "index must be >= ~A" (min-index deque))
  (cond ((>= index (max-index deque))
         (setf (head-index deque) 0
               (tail-index deque) 0
               (min-index deque) index
               (max-index deque) index))
        (t
         (let ((delta (- index (min-index deque))))
           (setf (head-index deque) (mod (+ (head-index deque) delta) (length (buffer deque))))
           (incf (min-index deque) delta))))
  deque)
