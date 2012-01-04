This package contains implementations of double-ended queues (aka,
deques). See: http://en.wikipedia.org/wiki/Deque

deque is a standard double-ended queue. You can push and pop from both
ends. Other features include indexing elements from their relative
position inside the deque, iterating through the elements, and
converting the deque to a list.

ideque is a specialized deque that keeps track of the indices of the
front and back of the queue so that we may reference items by an
absolute, rather than relative, index. The idea is that you can add
elements to an ideque (push-back), pop elements off the front
(pop-front), and still reference the nth item added directly by n. It
probably doesn't make much sense to use push-front with an ideque, but
you can.

Please see the docstrings for the exported symbols for more
information.

--------------------------------
Acknowledgments

This implementation was originally based off of the deque
implementation in the arnesi package:
http://common-lisp.net/project/bese/repos/arnesi_dev/src/queue.lisp
