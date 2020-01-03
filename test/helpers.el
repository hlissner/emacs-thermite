;;; helpers.el --- test helpers -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'subr-x)
(require 'cl-lib)

;;; Hash table helpers
(defalias 'ht-keys #'hash-table-keys)
(defalias 'ht-values #'hash-table-values)

(defun ht-map (fn ht)
  (let (ret)
    (maphash (lambda (_key val)
               (push (funcall fn val) ret))
             ht)
    (nreverse ret)))

(defun ht-every-key (fn ht)
  (catch 'abort
    (maphash (lambda (key _val)
               (unless (funcall fn key)
                 (throw 'abort nil)))
             ht)
    t))

(defun ht-every (fn ht)
  "Return t if (FN VAL) is non-nil for all values in HT."
  (catch 'abort
    (maphash (lambda (_key val)
               (unless (funcall fn val)
                 (throw 'abort nil)))
             ht)
    t))

(defun ht-any (fn ht)
  "Return t if (FN VAL) is non-nil for any value in HT."
  (catch 'abort
    (maphash (lambda (_key val)
               (when (funcall fn val)
                 (throw 'abort t)))
             ht)
    nil))

;;; Helpers
(defun dummy-doom-theme-1 (name)
  (make-doom-theme name
    :colors '((red "red")
               (blue "#0000FF"))
    :faces '((a :background red)
             (d :background red)
             (z :foreground white)
             ((fake1 &pseudo t) :underline t)
             ((fake2 &pseudo t) :strike-through t))
    :vars '((a 0)
            (b -5))))

(defun dummy-doom-theme-2 (name)
  (make-doom-theme name
    :colors '((blue "blue"))
    :faces '((a :foreground white)
             (b :background red)
             ((c &extend z) :weight 'bold)
             ((d &extend t) :weight 'bold)
             ((e &extend fake1)))
    :vars '((a 20)
            (z 10))))

(provide 'helpers)
;;; common.el ends here
