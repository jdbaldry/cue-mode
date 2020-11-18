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
(defconst cue--identifier-regexp "\\(?:\\#\\|\\#_\\|_\\)?[a-zA-Z[:digit:]]+")
(defconst cue--field-regexp (concat "\\(" cue--identifier-regexp "\\??:\\)"))
(defconst cue-font-lock-keywords
  (let (
        (keywords-regex (regexp-opt '("package" "import" "true" "false" "for" "in" "if" "let" "div" "mod" "quo" "rem") 'words))
        (type-regex (regexp-opt '("bool" "string" "int" "null" "float" "bytes" "number" "uint" "uint8" "int8" "uint16" "int16" "rune" "uint32" "int32" "uint64" "int64" "uint128" "int128" "float32" "float64") 'words)))
  (list
   `(,keywords-regex . font-lock-builtin-face)
   `(,type-regex . font-lock-type-face)
   `(,cue--field-regexp . font-lock-function-name-face)
   ))
  "Minimal highlighting expressions for Cue major mode")

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
  (setq major-mode 'cue-mode)
  (setq mode-name "Cue")
  ;; Cue, like Go, uses tabs.
  (setq indent-tabs-mode t)
  (setq-local indent-line-function 'indent-to-left-margin)
  (run-hooks 'cue-mode-hook))

(add-to-list 'auto-mode-alist (cons "\\.cue\\'" 'cue-mode))

(provide 'cue-mode)
