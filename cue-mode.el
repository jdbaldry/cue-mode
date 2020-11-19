;; Hook for user defined code.
(defvar cue-mode-hook nil)

;; Configure keymap.
(defvar cue-mode-map
  (let ((map (make-keymap)))
    ;; TODO: implement buffer formatting.
    ;; (define-key map "C-c C-r" 'cue-format-buffer)
    map)
  "Keymap for Cue major mode")

;; Keyword highlighting.
(defconst cue--identifier-regexp "\\(?:\\#\\|\\#_\\|_\\)?[a-zA-Z\$_[:digit:]]+")
(defconst cue--closed-identifier-regexp "#_?[a-zA-Z\$_[:digit:]]+")
(defconst cue--field-regexp (concat "\\(" cue--identifier-regexp "\\??:\\)"))
(defconst cue-font-lock-keywords
  (let (
        (keywords-regex (regexp-opt '("package" "import" "true" "false" "for" "in" "if" "let" "div" "mod" "quo" "rem") 'words))
        (type-regex (regexp-opt '("bool" "string" "int" "null" "float" "bytes" "number" "uint" "uint8" "int8" "uint16" "int16" "rune" "uint32" "int32" "uint64" "int64" "uint128" "int128" "float32" "float64") 'words)))
  (list
   `(,keywords-regex . font-lock-builtin-face)
   `(,cue--closed-identifier-regexp . font-lock-type-face)
   `(,type-regex . font-lock-constant-face)
   `(,cue--field-regexp . font-lock-function-name-face)
   ))
  "Minimal highlighting expressions for Cue major mode")

;; Indentation.
(defun cue-indent-line ()
  "Indent the current line close to how cuefmt would"
  (interactive)
  (beginning-of-line)
  (if (bobp)
      ;; First line is never indented.
      (indent-line-to 0)
    (let ((no-hint t) indent)
      ;; If we are looking at the end of a block, decrease the indent.
      (if (looking-at "\t*[]}]$")
          (progn
            (save-excursion
              (setq indent (- (current-indentation) 2)))
            ;; Don't go beyond left margin.
            (if (< indent 0)
                (setq indent 0)))
        (save-excursion
          ;; Iterate backwards until we find an indentation hint.
          (while no-hint
            (forward-line -1)
            ;; If the previous line opened a block, increase the indent.
            (if (looking-at ".*[{\\[]$")
                (progn
                  (setq indent (+ (current-indentation) 2))
                  (setq no-hint nil))
              ;; Else, if the previous line was a field, maintain the indent.
              (if (looking-at "\t*[[:graph:]]")
                  (progn
                    (setq indent (current-indentation))
                    (setq no-hint nil))
                ;; Else, if we reach the beginning of the file, break.
                (if (bobp)
                    (setq no-hint nil)))))))
      ;; If we set an indent, indent the current line to it.
      (if indent
          (indent-line-to indent)
        ;; Else, set it 0.
        (indent-line-to 0)))))

;; Syntax table.
(defconst cue-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Cue uses // to delimit comments
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?\n "> b" table)
    table)
  "Syntax table for Cue major mode.")

(defun cue-mode ()
  "Major mode for editing Cue (Cuelang) files"
  (interactive)

  (kill-all-local-variables)
  (use-local-map cue-mode-map)
  (set-syntax-table cue-mode-syntax-table)

  (set (make-local-variable 'font-lock-defaults) '(cue-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'cue-indent-line)
  (setq major-mode 'cue-mode)
  (setq mode-name "Cue")
  ;; Cue, like Go, uses tabs.
  (setq indent-tabs-mode t)
  (run-hooks 'cue-mode-hook))

(add-to-list 'auto-mode-alist (cons "\\.cue\\'" 'cue-mode))

(provide 'cue-mode)
