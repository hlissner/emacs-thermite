;;; doom-themer.el --- less themin', more emacsin' -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2016-2020 Henrik Lissner
;;
;; Author: Henrik Lissner <http://github/hlissner>
;; Maintainer: Henrik Lissner <henrik@lissner.net>
;; Created: July 1, 2019
;; Modified: January 3, 2020
;; Version: 0.0.1
;; Keywords: themes faces customization
;; Homepage: https://github.com/hlissner/emacs-doom-themer
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; TODO
;;
;;; Code:

(require 'cl-lib)

(defcustom doom-themer-load-hook nil
  "Hook run after `load-theme' successfully loads and enables a theme.

This doesn't only apply to Doom themes. Consult `doom-themer-current-theme' to
access the name of the theme that was loaded."
  :type '(repeat function)
  :group 'doom-themer)


;;;; Library
(defvar doom-themer-current-theme nil
  "The name (symbol) of the last theme loaded with `load-theme'.")

(defvar doom-themer--table (make-hash-table :test 'eq)
  "A hash table containing all known Doom themes defined in this session.")

(defun doom-themer--get (name &optional noerror)
  "Get the theme object called NAME (a symbol).

If NOERROR, don't throw an error if the theme doesn't exist."
  (or (ignore-errors (gethash name doom-themer--table))
      (unless noerror
        (error "Couldn't find %S theme" name))))

(defun doom-themer--rpartial (fn &rest args)
  "Return a function that is a partial application of FN to right-hand ARGS.

ARGS is a list of the last N arguments to pass to FUN. The result is a new
function which does the same as FUN, except that the last N arguments are fixed
at the values with which this function was called."
  (lambda (&rest pre-args) (apply fn (append pre-args args))))

;; It would be useless to change these variables; they are only used at
; compile-time.
(eval-when-compile
  (defconst doom-themer--remap-alist
    '((:fg  . :foreground)
      (:bg  . :background)
      (:dfg . :distant-foreground))
    "An alist of face properties to remap, for convenience sake.")

  (defconst doom-themer--classes
    '(((class color) (min-colors #xFFFFFF)) ; 24-bit
      ((class color) (min-colors #xFF))     ; 256
      ((class color) (min-colors #xF)))))   ; tty


;;; Structs
(cl-defstruct (doom-theme-face
               (:constructor nil)
               (:constructor
                make-doom-theme-face
                (name &key directives plist
                      &aux
                      (directives
                       (doom-themer--normalize-plist
                        directives #'doom-themer--remap-aux-prop))
                      (plist
                       (doom-themer--normalize-plist plist)))))
  (name nil :read-only t)
  directives
  plist)

(cl-defstruct (doom-theme
               (:constructor nil)
               (:constructor
                make-doom-theme
                (name &key classes colors faces vars
                      &aux
                      (colors (doom-themer--alist-to-ht colors))
                      (faces (doom-themer--alist-to-ht faces #'make-doom-theme-face-from-aplist))
                      (vars (doom-themer--alist-to-ht vars)))))
  (name 'user :read-only t)
  (classes doom-themer--classes)
  colors
  faces
  vars)

(cl-defun make-derived-doom-theme (name parent &key colors faces vars)
  "Create a `doom-theme' named NAME merged with PARENT'"
  (declare (indent 2))
  (doom-themer--merge
   (make-doom-theme name
     :palette colors :faces faces :vars vars)
   (when parent
     (if (doom-theme-p parent)
         parent
       (doom-themer--get parent)))))

(put #'make-doom-theme-face 'lisp-indent-function 1)
(put #'make-doom-theme 'lisp-indent-function 1)


;;; In-face spec helpers
(defun doom-themer--nth (i var)
  (if (listp var)
      ;; If a color variable is a list, then it implies that each element is for
      ;; a different color class. `i' should correlate to the indecies of
      ;; `doom-themer--classes'.
      (nth i var)
    ;; It's possible a color variable won't be a list, in which case we
    ;; shouldn't treat it as one.
    var))

;; Calls like (lighten ...) and (blend ... ... ...) in a face's plist will be
;; converted into calls to these helpers.
(defun doom-themer--helper-get (face property)) ; TODO
(defun doom-themer--helper-lighten (color amount)) ; TODO
(defun doom-themer--helper-darken (color amount)) ; TODO
(defun doom-themer--helper-blend (color1 color2 amount)) ; TODO
(defun doom-themer--helper-name-to-rgb (color)) ; TODO
(defun doom-themer--helper-approx (color)) ; TODO


(defvar doom-themer--quoted-p nil)
(defun doom-themer--replace-refs (forms theme &optional class ref-p)
  (declare (pure t) (side-effect-free t))
  (let ((doom-themer--quoted-p doom-themer--quoted-p))
    (cond
     ((atom forms)
      (if-let ((subform (and (not doom-themer--quoted-p)
                             (symbolp forms)
                             (not (keywordp forms))
                             (gethash forms (doom-theme-colors theme)))))
          (let ((refform (doom-themer--replace-refs subform theme class t)))
            (if ref-p
                refform
              `(doom-themer--nth ,class ,refform)))
        forms))
     ((cond ((eq (car forms) 'quote) forms)
            ((memq (car forms) '(backquote \`))
             (setq doom-themer--quoted-p t)
             (cons (car forms)
                   (list
                    (doom-themer--replace-refs
                     (cadr forms) theme class ref-p))))
            ((eq (car forms) '\,)
             (setq doom-themer--quoted-p nil)
             (cons (car forms)
                   (doom-themer--replace-refs
                    (cdr forms) theme class ref-p)))
            ((mapcar (doom-themer--rpartial
                      #'doom-themer--replace-refs theme class ref-p)
                     (cons (let ((sym
                                  (intern-soft
                                   (format "doom-themer--helper-%s" (car forms)))))
                             (if (fboundp sym)
                                 sym
                               (car forms)))
                           (cdr forms)))))))))


;;
;;; Internal helpers

(defun doom-themer--remap-prop (property value)
  (cons (if-let ((remap (assq property doom-themer--remap-alist)))
            (cdr remap)
          property)
        value))

(defun doom-themer--remap-aux-prop (prop value)
  (cons (if (string-prefix-p "&" (symbol-name prop))
            (intern (format ":%s" (substring (symbol-name prop) 1)))
          prop)
        value))

(defun doom-themer--normalize-plist (plist &optional mapfn)
  "Remap properties in PLIST according to `doom-themer--remap-alist'."
  (let ((mapfn (or mapfn #'doom-themer--remap-prop))
        newplist)
    (while plist
      (cl-destructuring-bind (prop . val)
          (funcall mapfn (pop plist) (pop plist))
        (when prop
          (push prop newplist)
          (push val newplist))))
    (nreverse newplist)))

(defun doom-themer--normalize-aux-plist (plist)
  "Convert all aux keywords (start with &) in PLIST to regular keywords."
  (declare (pure t) (side-effect-free t))
  (let ((plist (copy-sequence plist))
        newplist)
    (while plist
      (let ((prop (pop plist))
            (val  (pop plist)))
        (push (if (string-prefix-p "&" (symbol-name prop))
                  (intern (format ":%s" (substring (symbol-name prop) 1)))
                prop)
              newplist)
        (push val newplist)))
    (nreverse newplist)))

(defun make-doom-theme-face-from-aplist (aplist)
  "Convert an APLIST to a `doom-theme-face'.

An aplist is an associative plist. It's car is an identifier symbol and its cdr
is a plist."
  (declare (pure t) (side-effect-free t))
  (cond
   ((null aplist) nil)
   ((doom-theme-face-p aplist) aplist)
   ((let ((plist (copy-sequence aplist)))
      (cl-destructuring-bind (name &rest directives)
          (if (listp (car plist))
              (pop plist)
            (list (pop plist)))
        (make-doom-theme-face name
          :directives directives
          :plist plist))))))

(defun doom-theme-face-pseudo-p (face)
  "Return non-nil if FACE is a pseudo face."
  (plist-get (doom-theme-face-directives face) :pseudo))

(defun doom-theme-face-to-spec (face theme)
  "Convert FACE to a spec list compatible with `custom-theme-set-faces'.

FACE is a `doom-theme-face'. THEME is the current `doom-theme'."
  `(list (quote ,(doom-theme-face-name face))
         (list ,@(let* ((plist (doom-theme-face-plist face))
                        (first (doom-themer--replace-refs plist theme 0)))
                   (if (equal first plist)
                       `((list 't (list ,@first)))
                     (let ((classes (doom-theme-classes theme)))
                       (cons `(list (quote ,(car classes)) (list ,@first))
                             (cl-loop for class in (cdr classes)
                                      for i from 1
                                      collect
                                      `(list (quote ,class)
                                             (list ,@(doom-themer--replace-refs
                                                      plist theme i)))))))))))


;;; Theme mappers/mergers
(defun doom-themer--alist-to-ht (alist &optional mapfn)
  "Convert an ALIST to a hash table with MAPFN (defaults to `cadr')."
  (declare (pure t) (side-effect-free t))
  (if (hash-table-p alist)
      alist
    (let ((ht (make-hash-table :test 'eq :size (length alist)))
          (mapfn (or mapfn #'cadr)))
      (dolist (entry alist)
        (puthash (if (listp (car entry))
                     (caar entry)
                   (car entry))
                 (funcall mapfn entry)
                 ht))
      ht)))

(defun doom-themer--merge-faces (from base ht)
  "Merge two `doom-theme-face's and resolves inheritance."
  (let ((from (copy-doom-theme-face from)))
    (cl-destructuring-bind (&key extend _pseudo)
        (doom-theme-face-directives from)
      (if (null extend)
          from
        (unless (memq extend '(t &self))
          (setq base (or (copy-doom-theme-face (gethash extend ht))
                         (error "Could not find face %S to extend %S from"
                                extend (doom-theme-face-name from)))))
        (let ((plist (doom-theme-face-plist from))
              (newplist (if base (copy-sequence (doom-theme-face-plist base)))))
          (while plist
            (let ((prop (pop plist))
                  (val  (pop plist)))
              (setq newplist (plist-put newplist prop val))))
          (setf (doom-theme-face-plist from)
                newplist)
          from)))))

(defun doom-themer--merge-default (from _base _ht)
  "Simply copy FROM."
  (if (listp from)
      (copy-sequence from)
    from))

(defun doom-themer--merge-1 (from base &optional mergefn)
  "Return a hash table copy with FROM applied to BASE.

If MERGEFN is non-nil, use it to transform each entry. It takes three arguments:
the entry in FROM, the entry in TO, and the result hash-table thus far (for
back-referencing, if necessary)."
  (let ((ht (copy-hash-table base))
        (mergefn (or mergefn #'doom-themer--merge-default)))
    (maphash (lambda (key val)
               (puthash
                key (funcall mergefn val (gethash key ht) ht)
                ht))
             from)
    ht))

(defun doom-themer--merge (theme &optional parent)
  "Return a `doom-theme' that is THEME merged into PARENT."
  (let ((theme (copy-doom-theme theme)))
    (if (null parent)
        theme
      (setf (doom-theme-colors theme)
            (doom-themer--merge-1 (doom-theme-colors theme)
                                  (doom-theme-colors parent))
            (doom-theme-vars theme)
            (doom-themer--merge-1 (doom-theme-vars theme)
                                  (doom-theme-vars parent))
            (doom-theme-faces theme)
            (doom-themer--merge-1 (doom-theme-faces theme)
                                  (doom-theme-faces parent)
                                  #'doom-themer--merge-faces))
      theme)))



;;
;;; Theme assembly

(defun doom-themer-process-vars (theme)
  "Process THEME's vars into an alist compatible with
`custom-theme-set-variables'."
  (let (varforms)
    (maphash (lambda (var val)
               (push (list 'list (list 'quote var)
                           (doom-themer--replace-refs val theme 0))
                     varforms))
             (doom-theme-vars theme))
    (nreverse varforms)))

(defun doom-themer-process-faces (theme)
  "Process THEME's faces into a list of face specs compatible with
`custom-theme-set-faces'."
  (let (specs)
    (dolist (face (hash-table-values (doom-theme-faces theme)))
      (unless (doom-theme-face-pseudo-p face)
        (push (doom-theme-face-to-spec face theme)
              specs)))
    (nreverse specs)))

;;;###autoload
(defmacro define-doom-theme
    (name docstring colors &optional faces vars &rest body)
  "Define a doom theme.

TODO

\(fn (NAME &key EXTENDS BASE) DOCSTRING COLORS &optional FACES VARS &rest BODY)"
  (declare (doc-string 2) (indent 1))
  (cl-check-type name (or symbol list))
  (cl-check-type docstring (or string null))
  (cl-destructuring-bind (name &key extends base)
      (if (listp name)
          (cons (car name)
                (doom-themer--normalize-plist
                 (cdr name) #'doom-themer--remap-aux-prop))
        (list name))
    (let ((theme
           (cl-reduce #'doom-themer--merge
                      (append (list (make-doom-theme name
                                      :colors colors
                                      :faces faces
                                      :vars vars))
                              (mapcar #'doom-themer--get
                                      (if (listp extends)
                                          extends
                                        (list extends)))))))
      (puthash name theme doom-themer--table)
      (macroexp-progn
       (cons `(puthash ',name ,theme doom-themer--table)
             (unless base
               `((deftheme ,name ,docstring)
                 (custom-theme-set-faces
                  ',name ,@(doom-themer-process-faces theme))
                 (custom-theme-set-variables
                  ',name ,@(doom-themer-process-vars theme))
                 (provide (custom-make-theme-feature ',name)))))))))

;;;###autoload
(defalias 'defdoomtheme 'define-doom-theme)

;;;###autoload
(progn
  (defun doom-themer--load-theme (theme &optional _no-confirm no-enable)
    "Run `doom-themer-load-hook' & set `doom-themer-current-theme' if
successful."
    (unless no-enable
      (setq doom-themer-current-theme theme)
      (run-hooks 'doom-themer-load-hook)))
  (advice-add #'load-theme :after-while #'doom-themer--load-theme))

(provide 'doom-themer)
;;; doom-themer.el ends here
