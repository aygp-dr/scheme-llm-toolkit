;;; scheme-llm-toolkit.el --- Emacs configuration for Scheme LLM Toolkit development

;; Set up package repositories
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Install required packages if not present
(defvar scheme-llm-packages
  '(geiser
    geiser-guile
    paredit
    rainbow-delimiters
    company
    flycheck))

(dolist (pkg scheme-llm-packages)
  (unless (package-installed-p pkg)
    (package-refresh-contents)
    (package-install pkg)))

;; Load required packages
(require 'geiser)
(require 'geiser-guile)
(require 'paredit)
(require 'rainbow-delimiters)
(require 'company)
(require 'flycheck)

;; Project-specific settings
(defvar scheme-llm-project-root
  (file-name-directory load-file-name)
  "Root directory of the Scheme LLM Toolkit project")

;; Geiser configuration for Guile
(setq geiser-guile-binary "guile3")
(setq geiser-default-implementation 'guile)
(setq geiser-active-implementations '(guile))
(setq geiser-guile-load-path
      (list (expand-file-name "src" scheme-llm-project-root)))

;; Enable paredit for Scheme files
(add-hook 'scheme-mode-hook #'paredit-mode)
(add-hook 'geiser-repl-mode-hook #'paredit-mode)

;; Enable rainbow delimiters for better paren matching
(add-hook 'scheme-mode-hook #'rainbow-delimiters-mode)
(add-hook 'geiser-repl-mode-hook #'rainbow-delimiters-mode)

;; Company mode for completions
(add-hook 'scheme-mode-hook #'company-mode)
(add-hook 'geiser-repl-mode-hook #'company-mode)

;; Flycheck for syntax checking
(add-hook 'scheme-mode-hook #'flycheck-mode)

;; Org-mode configuration for documentation
(require 'org)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-edit-src-content-indentation 0)

;; Support for org-babel with Scheme
(org-babel-do-load-languages
 'org-babel-load-languages
 '((scheme . t)))

;; TRAMP configuration for remote development if needed
(require 'tramp)
(setq tramp-default-method "ssh")

;; Custom keybindings for Scheme development
(define-key scheme-mode-map (kbd "C-c C-l") 'geiser-load-file)
(define-key scheme-mode-map (kbd "C-c C-k") 'geiser-compile-current-buffer)
(define-key scheme-mode-map (kbd "C-c C-r") 'geiser-eval-region)
(define-key scheme-mode-map (kbd "C-c C-e") 'geiser-eval-last-sexp)

;; Project-specific functions
(defun scheme-llm-run-tests ()
  "Run the Scheme LLM Toolkit tests"
  (interactive)
  (let ((default-directory scheme-llm-project-root))
    (compile "make test")))

(defun scheme-llm-load-provider ()
  "Load the LLM provider modules"
  (interactive)
  (geiser-load-file (expand-file-name "src/llm/core/provider.scm" scheme-llm-project-root))
  (geiser-load-file (expand-file-name "src/llm/providers/ollama.scm" scheme-llm-project-root)))

;; Set up the initial window layout
(delete-other-windows)
(split-window-horizontally)
(other-window 1)
(split-window-vertically)
(other-window 1)
(geiser 'guile)
(other-window 1)
(find-file (expand-file-name "README.org" scheme-llm-project-root))
(other-window 1)

;; Start message
(message "Scheme LLM Toolkit development environment loaded. Project root: %s" scheme-llm-project-root)

(provide 'scheme-llm-toolkit)
;;; scheme-llm-toolkit.el ends here