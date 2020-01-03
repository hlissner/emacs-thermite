;; -*- no-byte-compile: t; -*-
;;; test/themer-test.el

(require 'doom-themer)

(describe "doom-themer API"
  :var (doom-themer--table)
  (before-each
    (setq doom-themer--table (make-hash-table)))
  (after-all
    (setq doom-themer--table nil))

  (describe "make-doom-theme"
    (it "returns a doom-theme"
      (expect (doom-theme-p (dummy-doom-theme-1 'theme)))
      (expect (doom-theme-p (dummy-doom-theme-2 'theme))))

    (it "returns a sane data structure"
      (let ((theme (dummy-doom-theme-1 'themea)))
        (expect (hash-table-p (doom-theme-faces theme)))
        (expect (hash-table-p (doom-theme-vars theme)))
        (expect (hash-table-p (doom-theme-colors theme)))
        (expect (not (hash-table-empty-p (doom-theme-faces theme))))
        (expect (not (hash-table-empty-p (doom-theme-vars theme))))
        (expect (not (hash-table-empty-p (doom-theme-colors theme))))
        (expect (ht-every #'doom-theme-face-p (doom-theme-faces theme)))
        (expect (ht-every-key #'symbolp (doom-theme-vars theme)))
        (expect (ht-every-key #'symbolp (doom-theme-colors theme))))))

  (describe "make-derived-doom-themes"
    (it "returns a compound doom-theme"
      (expect (doom-theme-p
               (doom-themer--merge
                (dummy-doom-theme-2 'theme)
                (dummy-doom-theme-1 'parent)))))

    (it "returns a sane data structure"
      (let ((theme (doom-themer--merge
                    (dummy-doom-theme-2 'theme)
                    (dummy-doom-theme-1 'parent))))
        (expect (hash-table-p (doom-theme-faces theme)))
        (expect (hash-table-p (doom-theme-vars theme)))
        (expect (hash-table-p (doom-theme-colors theme)))
        (expect (not (hash-table-empty-p (doom-theme-faces theme))))
        (expect (not (hash-table-empty-p (doom-theme-vars theme))))
        (expect (not (hash-table-empty-p (doom-theme-colors theme))))
        (expect (ht-every #'doom-theme-face-p (doom-theme-faces theme)))
        (expect (ht-every-key #'symbolp (doom-theme-vars theme)))
        (expect (ht-every-key #'symbolp (doom-theme-colors theme)))))

    (it "overrides old vars, colors and simple faces with new ones"
      (let ((theme (doom-themer--merge
                    (make-doom-theme 'theme
                      :colors '((red "white"))
                      :vars '((a "x")))
                    (make-doom-theme 'theme
                      :colors '((red "red") (blue "blue") (green "green"))
                      :vars '((a "a") (b "b"))))))
        (expect (gethash 'red (doom-theme-colors theme)) :to-equal "white")
        (expect (gethash 'a (doom-theme-vars theme)) :to-equal "x")
        ;; And others should be unchanged
        (expect (gethash 'blue (doom-theme-colors theme)) :to-equal "blue")
        (expect (gethash 'green (doom-theme-colors theme)) :to-equal "green")
        (expect (gethash 'b (doom-theme-vars theme)) :to-equal "b")))

    (it "merges faces that extend others or themselves"
      (let* ((theme (doom-themer--merge
                     (make-doom-theme 'theme
                       :faces '(((a &extend x) :weight 'bold)
                                ((b &extend t) :slant 'italic)))
                     (make-doom-theme 'theme
                       :faces '((x :background blue)
                                (b :foreground red)))))
             (faces (doom-theme-faces theme)))
        (expect (doom-theme-face-plist (gethash 'a faces))
                :to-equal '(:background blue :weight 'bold))
        (expect (doom-theme-face-plist (gethash 'b faces))
                :to-equal '(:foreground red :slant 'italic))
        (expect (doom-theme-face-plist (gethash 'x faces))
                :to-equal '(:background blue))))

    (it "should error for faces extending non-existent parent faces"
      (expect (doom-themer--merge
               (make-doom-theme 'theme :faces '(((a &extend b))))
               (make-doom-theme 'parent))
              :to-throw)))

  (describe "struct doom-theme-face"
    :var (facea faceb)
    (describe "aplist->face conversion"
      (it "converts a simple aplist to a face object"
        (let ((face (make-doom-theme-face-from-aplist
                     '(default :background red :foreground blue))))
          (expect (doom-theme-face-p face))
          (expect (doom-theme-face-name face) :to-be 'default)
          (expect (doom-theme-face-directives face) :not :to-be-truthy)
          (expect (doom-theme-face-plist face)
                  :to-equal '(:background red :foreground blue))))

      (it "converts an aplist with directives to a face object"
        (let ((face (make-doom-theme-face-from-aplist
                     '((default &extend t) :background red :foreground blue))))
          (expect (doom-theme-face-p face))
          (expect (doom-theme-face-name face) :to-be 'default)
          (expect (doom-theme-face-directives face) :to-be-truthy)
          (expect (doom-theme-face-plist face)
                  :to-equal '(:background red :foreground blue))))

      (it "converts aux directive keywords to real keywords"
        (expect (doom-theme-face-directives
                 (make-doom-theme-face-from-aplist
                  '((default &extend t &psuedo t) :background red :foreground blue)))
                :to-equal '(:extend t :psuedo t))))

    (describe "face->spec conversion"
      (it "renders simple faces with one class"
        (expect (eval (doom-theme-face-to-spec
                       (make-doom-theme-face 'a :plist '(:weight 'bold))
                       (make-doom-theme 'temp
                         :classes '(((class color)
                                     (min-colors 16777215))))))
                :to-equal
                '(a (((class color) (min-colors 16777215))
                     (:weight bold)))))

      (it "renders simple faces with multiple classes"
        (expect (eval (doom-theme-face-to-spec
                       (make-doom-theme-face 'a :plist '(:weight 'bold))
                       (make-doom-theme 'temp
                         :classes '(((class color) (min-colors #xFFFFFF))
                                    ((class color) (min-colors #xFF))
                                    ((class color) (min-colors #xF))))))
                :to-equal
                '(a (((class color) (min-colors #xFFFFFF))
                     (:weight bold))
                    (((class color) (min-colors #xFF))
                     (:weight bold))
                    (((class color) (min-colors #xF))
                     (:weight bold)))))

      (describe "references"
        (it "resolves and repeats literal references across classes"
          (expect (eval (doom-theme-face-to-spec
                         (make-doom-theme-face 'a :plist '(:foreground red))
                         (make-doom-theme 'temp
                           :classes '(((class color) (min-colors #xFF))
                                      ((class color) (min-colors #xF)))
                           :colors '((red "red")))))
                  :to-equal
                  '(a (((class color) (min-colors #xFF))
                       (:foreground "red"))
                      (((class color) (min-colors #xF))
                       (:foreground "red")))))

        (it "resolves recursive, literal references across classes"
          (expect (eval (doom-theme-face-to-spec
                         (make-doom-theme-face 'a :plist '(:foreground red :background green))
                         (make-doom-theme 'temp
                           :classes '(t t)
                           :colors '((blue red)
                                     (red '("very red" "red"))
                                     (green blue)))))
                  :to-equal
                  '(a (t (:foreground "very red" :background "very red"))
                      (t (:foreground "red" :background "red")))))

        (it "resolves and destructures array references to their respective classes"
          (expect (eval (doom-theme-face-to-spec
                         (make-doom-theme-face 'a :plist '(:foreground red))
                         (make-doom-theme 'temp
                           :classes '(((class color) (min-colors #xFFFFFF))
                                      ((class color) (min-colors #xFF))
                                      ((class color) (min-colors #xF)))
                           :colors '((red '("super red" "very red" "kinda red"))))))
                  :to-equal
                  '(a (((class color) (min-colors #xFFFFFF))
                       (:foreground "super red"))
                      (((class color) (min-colors #xFF))
                       (:foreground "very red"))
                      (((class color) (min-colors #xF))
                       (:foreground "kinda red")))))

        (it "nil-pads resolved short array references"
          (expect (eval (doom-theme-face-to-spec
                         (make-doom-theme-face 'a :plist '(:foreground red))
                         (make-doom-theme 'temp
                           :classes '(((class color) (min-colors #xFF))
                                      ((class color) (min-colors #xF)))
                           :colors '((red '("red"))))))
                  :to-equal
                  '(a (((class color) (min-colors #xFF))
                       (:foreground "red"))
                      (((class color) (min-colors #xF))
                       (:foreground nil)))))

        (it "doesn't touch references that don't exist"
          (expect (eval (doom-theme-face-to-spec
                         (make-doom-theme-face 'a :plist '(:foreground blue))
                         (make-doom-theme 'temp
                           :classes '(((class color) (min-colors #xFF))
                                      ((class color) (min-colors #xF)))
                           :colors '((red "red"))))
                        '((blue . blue)))
                  :to-equal
                  '(a (((class color) (min-colors #xFF))
                       (:foreground blue))
                      (((class color) (min-colors #xF))
                       (:foreground blue)))))

        (it "doesn't touch quoted references"
          (expect (eval (doom-theme-face-to-spec
                         (make-doom-theme-face 'a :plist '(:box '(:color red)))
                         (make-doom-theme 'temp
                           :classes '(((class color) (min-colors #xFF)))
                           :colors '((red "red")))))
                  :to-equal
                  '(a (((class color) (min-colors #xFF))
                       (:box (:color red))))))

        (it "resolves backquote-interpolated references"
          (expect (eval (doom-theme-face-to-spec
                         (make-doom-theme-face 'a
                           :plist '(:box `(:color ,red)))
                         (make-doom-theme 'temp
                           :classes '(((class color) (min-colors #xFF)))
                           :colors '((red "red")))))
                  :to-equal
                  '(a (((class color) (min-colors #xFF))
                       (:box (:color "red")))))))))

  (describe "doom-themer-process-faces"
    (it "doesn't render pseudo faces"
      (let* ((specs (doom-themer-process-faces
                     (make-doom-theme 'theme
                       :classes '(((class color) (min-colors #xFF)))
                       :faces '((a :background "red")
                                ((b &pseudo t) :foreground "blue")))))
             (evalform (cons 'list specs)))
        (expect (length specs) :to-be 1)
        (expect (assq 'a (eval evalform)))
        (expect (null (assq 'b (eval evalform)))))))

  (describe "doom-themer-process-vars"
    (it "renders simple variables"
      (expect (eval
               `(list ,@(doom-themer-process-vars
                         (make-doom-theme 'theme
                           :vars '((a 5)
                                   (b "hello"))))))
              :to-equal '((a 5) (b "hello"))))

    (it "resolves variables with references"
      (expect (eval
               `(list ,@(doom-themer-process-vars
                         (make-doom-theme 'theme
                           :colors '((red "red")
                                     (blue red))
                           :vars '((a "red")
                                   (b red)
                                   (c blue))))))
              :to-equal '((a "red")
                          (b "red")
                          (c "red"))))))
