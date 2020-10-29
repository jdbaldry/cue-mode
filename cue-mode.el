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
(defconst cue-font-lock-keywords
  (let ((built-in-regex (regexp-opt '("package" "import") 'words)))
  (list
   `(,built-in-regex . font-lock-builtin-face)
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
  (run-hooks 'cue-mode-hook))

(add-to-list 'auto-mode-alist (cons "\\.cue\\'" 'cue-mode))

(provide 'cue-mode)
