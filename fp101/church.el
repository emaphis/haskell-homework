;;; pachage church.el

;;; Summary experiment in church numberals in Emacs Lisp

;; Comentary:

;;; Code:

(defvar cn-zero (lambda (f) (lambda (x) x)))

(defun cn-succ (n)
  (lambda (f)
    (lambda (x) (funcall f (funcall (funcall n f) x)))))

(defvar cn-one
  (lambda (f)
    (lambda (x) (funcall f x))))

(defvar cn-two
  (lambda (f)
    (lambda (x) (funcall f (funcall f x)))))

(defun cn-add (a b)
  (lambda (f)
	(lambda (x) (funcall (funcall a f) (funcall (funcall b f) x)))))

(defun c2i (n)
  (funcall (funcall n (lambda (x) (+ x 1))) 0))

(c2i (cn-succ cn-one))


(provide 'church)
;;; church.el ends here
