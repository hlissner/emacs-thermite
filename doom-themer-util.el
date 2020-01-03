;;; doom-themer-util.el --- Utility functions & convenience commands -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'doom-themer)

(defvar doom-themer-screenshot-command
  (cond ((executable-find "scrot")
         "scrot -u %s%s.png")
        ((executable-find "maim")
         "maim -us %s%s.png"))
  "Command used to take automated screenshots for Doom.

Should contain 2 %s constructs to allow for theme name and directory/prefix")


;;
;;; Color helpers

;; Shamelessly *borrowed* from solarized
;;;###autoload
(defun doom-name-to-rgb (color)
  "Return the hexidecimal string repesentation of the named COLOR.
COLOR is a string (e.g. \"red\")."
  (cl-loop with div = (float (car (tty-color-standard-values "#ffffff")))
           for x in (tty-color-standard-values (downcase color))
           collect (/ x div)))

;;;###autoload
(defun doom-blend (color1 color2 alpha &optional index)
  "Return column INDEX of COLOR1 and COLOR2 blended by a coefficient ALPHA."
  (when (and color1 color2)
    (cond ((and color1 color2 (symbolp color1) (symbolp color2))
           (doom-blend (doom-color color1 index) (doom-color color2 index) alpha))

          ((or (listp color1) (listp color2))
           (cl-loop for x in color1
                    when (if (listp color2) (pop color2) color2)
                    collect (doom-blend x it alpha index)))

          ((and (string-prefix-p "#" color1) (string-prefix-p "#" color2))
           (apply (lambda (r g b) (format "#%02x%02x%02x" (* r 255) (* g 255) (* b 255)))
                  (cl-loop for it    in (doom-name-to-rgb color1)
                           for other in (doom-name-to-rgb color2)
                           collect (+ (* alpha it) (* other (- 1 alpha))))))

          (color1))))

;;;###autoload
(defun doom-darken (color alpha &optional index)
  "Return a COLOR (in column INDEX) darkened by a coefficient ALPHA.

COLOR is a hexidecimal string or a symbol referring to a palette color in your
active theme. ALPHA is a float between 0.0 and 1.0, inclusive. INDEX is an
integer representing the color column to use for COLOR (if it is a symbol). If
that column doesn't exist, it will use the next one."
  (cond ((and color (symbolp color))
         (doom-darken (doom-color color index) alpha))

        ((listp color)
         (cl-loop for c in color collect (doom-darken c alpha index)))

        ((doom-blend color "#000000" (- 1 alpha) index))))

;;;###autoload
(defun doom-lighten (color alpha &optional index)
  "Return a COLOR (in column INDEX) brightened by a coefficient ALPHA.

COLOR is a hexidecimal string or a symbol referring to a palette color in your
active theme. ALPHA is a float between 0.0 and 1.0, inclusive. INDEX is an
integer representing the color column to use for COLOR (if it is a symbol). If
that column doesn't exist, it will use the next one."
  (cond ((and color (symbolp color))
         (doom-lighten (doom-color color index) alpha))

        ((listp color)
         (cl-loop for c in color collect (doom-lighten c alpha index)))

        ((doom-blend color "#FFFFFF" (- 1 alpha) index))))

;;;###autoload
(defun doom-ref (face property)
  "Grab another FACE's PROPERTY."
  (let ((theme (doom-theme-current)))
    (unless (doom-theme-p theme)
      (user-error "No doom theme is active"))
    (if-let (spec (gethash face (doom-theme-faces theme)))
        (if (plist-member spec property)
            (plist-get spec property)
          (error "No %S property in %S face" property face))
      (error "No %S face in %S theme" face (doom-theme-name theme)))))

;;;###autoload
(defun doom-color (color &optional index)
  "Retrieve a specific COLOR for the current theme.
INDEX controls which class to fetch."
  (let ((colors (if (listp color)
                    color
                  (cdr (assq color doom-themer--current-palette)))))
    (when colors
      (if index
          (nth index colors)
        (car colors)))))


;;
;;; Customization helpers

;;;###autoload
(defmacro doom-themer-set-faces (theme &rest specs)
  "Additively change faces for THEME by SPECS.

SPECS is a list of either face specs, or alists mapping a package name to a list
of face specs. e.g.

  (custom-set-faces!
   (mode-line :foreground (doom-color 'blue))
   (mode-line-buffer-id :foreground (doom-color 'fg) :background \"#000000\")
   (mode-line-success-highlight :background (doom-color 'green))
   (org
    (org-tag :background \"#4499FF\")
    (org-ellipsis :inherit 'org-tag))
   (which-key
    (which-key-docstring-face :inherit 'font-lock-comment-face)))

Each face spec must be in the format of (FACE-NAME [:ATTRIBUTE VALUE]...).

Unlike `custom-set-faces', which destructively changes a face's spec, this one
adjusts pre-existing ones."
  (macroexp-progn
   (let (forms)
     (dolist (spec specs)
       (if (keywordp (cadr spec))
           (cl-destructuring-bind (face . attrs) spec
             (push `(set-face-attribute ,(if (symbolp face) `(quote ,face) face)
                                        nil ,@(doom-theme--colorize attrs 0))
                   forms))
         (push `(with-eval-after-load ',(car spec)
                  ,@(cl-loop for (face . attrs) in (cdr spec)
                             collect `(set-face-attribute ,(if (symbolp face) `(quote ,face) face)
                                                          nil ,@(doom-theme--colorize attrs 0))))
               forms)))
     (nreverse forms))))


;;
;;; Generators

;;;###autoload
(defun doom-themer-new (&optional palette) ; TODO doom-themer-new
  (interactive)
  (let ((file custom-theme-directory))
    ;; Generate a doom theme buffer
    ))

;;;###autoload
(defun doom-themer-screenshot (theme) ; TODO doom-themer-screenshot
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar 'symbol-name
                                     (custom-available-themes))))
    nil nil))
  (unwind-protect
      (let ((old-theme doom-themer-current))
        (unless (executable-find doom-themer-screenshot-command)
          (error "Couldn't find a screenshot program on your system. See `doom-themer-screenshot-command'"))
        (load-theme theme t)
        (redisplay t)
        (shell-command (format doom-themer-screenshot-command
                               prefix theme)))
    (disable-theme theme)
    (load-theme old-theme t)
    (redisplay t)))

(provide 'doom-themer-util)
;;; doom-themer-util.el ends here
