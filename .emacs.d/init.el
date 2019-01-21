
;; Anand Krishnamoorthi's emacs setup.

(setq load-prefer-newer t)

;; Setup up MELPA.
;; Load emacs 24's package system. Add MELPA repository.
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.milkbox.net/packages/")
   t))

(package-initialize)

;; backup to this folder rather than littering all
;; visited directories
;; backup folder
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; fixes for use via putty
(load-file (concat user-emacs-directory "putty.el"))
(load-file (concat user-emacs-directory "color-names.el"))

;; Show column-numbers in all buffers
(column-number-mode 1)

;; Hide menu bar and tool bar
(tool-bar-mode -1)
(menu-bar-mode -1)

;; In 4K monitors, avoid creating many windows
;; by setting large threshold values.
(setq
 split-height-threshold 2160
 split-width-threshold 3840)

;; use-package.
;; If use-package is not yet installed,
;; refresh list of packages (MELPA has already been added)
;; and then install use-package.
(unless
    (require 'use-package nil 'no-error)
  (progn
    (package-refresh-contents)
    (package-install 'use-package)))

;; Install all packages
(setq use-package-always-ensure t)

;; Navigation between windows
(use-package windmove
  ;; Shift + arrow keys to navigate windows
  :config (windmove-default-keybindings))

;; Numbered naviation between windows
(use-package ace-window
  :bind (("M-o" . ace-window)))
  
;; ace-jump-mode
(use-package ace-jump-mode
  :bind (("C-c c" . ace-jump-char-mode)
	 ("C-c v" . ace-jump-word-mode)
	 ("C-c l" . ace-jump-line-mode)))

;; When cursor jumps to a window, show a beacon
;; as a visual aid.
(use-package beacon
  :config (beacon-mode 1))

;; Ivy and counsel for enhanced command-completion
;; help etc.
(use-package ivy
  :config (ivy-mode))

;; counsel enhances default emacs functions by rebinding them
;; to counsel- implementations
(use-package counsel
  :after ivy
  ;; Don't defer loading ivy; otherwise it will be loaded
  ;; only when the key binding is pressed.
  :demand
  ;; Ability to search for files in the current repository
  :bind ("C-c f" . counsel-git)
  :config (counsel-mode))

;; git
(use-package magit)

;; Choose atom-one-dark-theme.
;; Theme is chosen late after load-theme has been
;; rebound to counsel-load-theme
(use-package atom-one-dark-theme)

;; eshell
;; Commands specified in visual commands launch a terminal  to execute
(setq eshell-visual-commands
      '("gdb" "bash"))

;; Configure gdb
;; gdb-many-windows by default
;; Setup gdb layout
(setq
 gdb-many-windows t
 gdb-show-main t)

(defadvice gdb-setup-windows (around setup-more-gdb-windows activate)
  ad-do-it
  (other-window 2)
  (set-window-dedicated-p (selected-window) nil)
  (split-window-horizontally)
  (other-window 1)
  (gdb-set-window-buffer
   (gdb-get-buffer-create 'gdb-disassembly-buffer))
  (other-window 1)
  (set-window-dedicated-p (selected-window) nil)
  (balance-windows-1)
  ;;(split-window-horizontally)
  ;;(gdb-set-window-buffer
  ;; (gdb-get-buffer-create 'gdb-registers-buffer))
  (other-window 4))

;; lsp-mode
;; Enable lsp-mode in C and C++ buffers.
;; Disable flymake; use flycheck instead.
(use-package lsp-mode
  :demand
  :init (setq lsp-prefer-flymake nil)
  ;; Turn on lsp which starts the LSP server (cquery) as well.
  :hook (c-mode-common . lsp)
  ;; xref-find-references does not work correctly.
  ;; rebind to lsp-find-references
  :bind ("M-?" . lsp-find-references))

;; lsp-ui
;; enable ui integration in all LSP buffers
(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode))

;; Enable flycheck globally
;; flycheck provides error messages in all buffers
;; by leveraging available backends.
(use-package flycheck
  :after lsp-mode
  :hook (after-init . global-flycheck-mode))

;; company
;; company provides completion in all buffers
(use-package company
  :hook (after-init . global-company-mode))

;; company-lsp
;; leverages the LSP mode to provide completions.
(use-package company-lsp
  :after lsp-mode)

;; cquery
;; cquery provides the LSP server for C & C++
(use-package cquery
  :after lsp-mode
  :config (setq
	   cquery-executable "~/.install/cquery/bin/cquery"
	   cquery-cache-dir-function 'cquery-cache-dir-consolidated))
	   
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (company-lsp beacon atom-one-dark-theme use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
