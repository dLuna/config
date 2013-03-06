; Created by Brian Templeton (bpt@hcoop.net)
; Extended by Adam Chlipala (adamc@hcoop.net)

(eval-when-compile (require 'cl))

(defvar domtool-indent 2)

(defvar domtool-mode-syntax-table
  (let ((table (make-syntax-table)))
    (loop for i from ?a to ?z
          do (modify-syntax-entry i "w" table))
    (loop for i from ?A to ?Z
          do (modify-syntax-entry i "w" table))
    (loop for i from ?0 to ?9
          do (modify-syntax-entry i "w" table))
    (mapc
     (lambda (pair)
       (loop for ch across (if (stringp (car pair))
                               (car pair)
                             (string (car pair)))
             do (modify-syntax-entry ch (cadr pair) table)))
     '((" \t\n\14" " ")                 ; \14 is ^L
       (?_  "_")
       (?*  ". 23n")
       (?\( "()1")
       (?\) ")(4")
       (?\" "\"")
       (?\\ "\\")
       (?\[ "(]")
       (?\] ")[")
       ;; We identify single-line comments using
       ;; font-lock-syntactic-keywords, because it's easier to
       ;; recognize documentation comments using the syntax table.
       (?\{ "(}12b")
       (?\} "){34b")
       ("->=<,:;^!&" ".")               ; -> => <- = , : ; ^ ! &
       ))
    table))

(defun domtool-syms-re (&rest syms)
  (concat "\\<" (regexp-opt syms t) "\\>"))

(require 'domtool-tables)

(defvar domtool-font-lock-keywords
  `(,(concat
      "\\_<"
      (regexp-opt '("let" "in" "begin" "end" "with" "where" "extern" "type"
                    "val" "context" "Root" "if" "then" "else")
                  t)
      "\\_>")

    (,domtool-actions-regexp . font-lock-builtin-face)
    (,domtool-vals-regexp . font-lock-variable-name-face)
    (,domtool-contexts-regexp . font-lock-constant-face)
    (,domtool-env-vars-regexp . font-lock-constant-face)
    (,domtool-types-regexp . font-lock-type-face)

    ("type[ \t]+\\(\\(\\sw\\|\\s_\\)+\\)" 1 font-lock-type-face)
    ("val[ \t]+\\(\\(\\sw\\|\\s_\\)+\\)" 1 font-lock-variable-name-face)))

(defvar domtool-font-lock-syntactic-keywords
  '(("\\(#\\).*\\(\n\\|\\'\\)"
     (1 "!")
     (2 "!"))))

(defun domtool-font-lock-syntactic-face-function (state)
  "Major mode for editing Domtool files."
  (cond ((nth 3 state) font-lock-string-face)
        ((eq (nth 7 state) t) font-lock-doc-face)
        (t font-lock-comment-face)))

(define-derived-mode domtool-mode fundamental-mode "Domtool"
  ;; For some reason, whatever twiddling `define-derived-mode' does
  ;; with the syntax table breaks recognition of (*...*) comments. So
  ;; we need to tell d-d-m to leave our syntax table alone.
  :syntax-table domtool-mode-syntax-table
  (set (make-local-variable 'indent-line-function) 'domtool-indent-line)
  (set (make-local-variable 'font-lock-defaults)
       '(domtool-font-lock-keywords
         nil nil nil nil
         (font-lock-syntactic-keywords
          . domtool-font-lock-syntactic-keywords)
         (font-lock-syntactic-face-function
          . domtool-font-lock-syntactic-face-function)))
  (set (make-local-variable 'comment-start-regexp) "(\\*\\|{{")
  (set (make-local-variable 'comment-end-regexp) "\\*)\\|}}")
  (set (make-local-variable 'comment-nested) t)

  (set (make-local-variable 'compile-command)
       (concat "domtool -tc " (file-relative-name buffer-file-name))))

(defun domtool-indent-line ()
  (let ((savep (> (current-column) (current-indentation)))
        (indent (domtool-calculate-indent)))
    (cond
     ((eq indent 'noindent) indent)
     (savep (save-excursion (indent-line-to indent)))
     (t (indent-line-to indent)))))

(defun until-closed-helper (level)
  (if
      (re-search-backward "\\_<\\(with\\|where\\|begin\\|end\\|let\\|val\\|type\\|if\\)\\_>"
			  nil t)
      (cond
       ((string= (match-string 0) "end")
	(until-closed-helper (+ level 1)))
       ((= level 0)
	(current-indentation))
       ((and
	 (string= (match-string 0) "with")
	 (save-excursion
	   (backward-char)
	   (looking-at "\\s-")))
	(until-closed-helper level))
       (t
	(until-closed-helper (- level 1))))

    0))

(defun until-closed ()
  (save-excursion
    (until-closed-helper 0)))

(defun domtool-calculate-indent ()
  (save-excursion
    (back-to-indentation)
    (multiple-value-bind (previous-keyword base-indent)
        (save-excursion
          (if (re-search-backward "\\_<\\(with\\|where\\|begin\\|end\\|let\\|in\\|val\\|type\\|if\\)\\_>\\|}}\\|{{"
                                  nil t)
              (values (match-string 0) (current-indentation))
            (values nil 0)))
      (let ((state (syntax-ppss)))
        (cond
         ((nth 3 state)
          'noindent)
         ((nth 4 state)
          (domtool-calculate-comment-indent state))
	 ((looking-at "{{\\|\\_<\\(extern\\|val\\|type\\|context\\)\\_>")
	  0)
         ((looking-at "\\_<\\(with\\|end\\|in\\|else\\)\\_>")
          (until-closed))
         ((not previous-keyword)
          base-indent)
         ((string= previous-keyword "end")
          base-indent)
         ((looking-at "\\_<\\(val\\|extern\\|context\\)\\_>")
          base-indent)
         (t
          (+ base-indent domtool-indent)))))))

(defun domtool-calculate-comment-indent (state)
  (ecase (nth 7 state)
    ((t) 'noindent)
    ((syntax-table) 'noindent)          ; can't happen
    ((nil) (let ((start (nth 8 state))
                 (depth 0))
             (while (> (point) start)
               (re-search-backward "(\\*\\|\\*)" start t)
               (if (looking-at "(\\*")
                   (incf depth)
                 (decf depth)))
             (+ (current-indentation) depth)))))

(provide 'domtool-mode)
