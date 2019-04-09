
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
(defun putty-init ()
  "Fixes for putty-client."
  (interactive)
  (load-file (concat user-emacs-directory "putty.el"))
  (load-file (concat user-emacs-directory "color-names.el")))

(putty-init)

;; enable mouse in terminal
(xterm-mouse-mode)

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
(if (not (string-equal (shell-command-to-string "which git") ""))
    (use-package magit))

;; Choose atom-one-dark-theme.
;; Theme is chosen late after load-theme has been
;; rebound to counsel-load-theme
;; (use-package atom-one-dark-theme)


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
  (balance-windows-area)
  ;;(split-window-horizontally)
  ;;(gdb-set-window-buffer
  ;; (gdb-get-buffer-create 'gdb-registers-buffer))
  (other-window 3))

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

;; For 'some' function used below
(use-package cl)

;; cquery
;; cquery provides the LSP server for C & C++
(use-package cquery
   :after lsp-mode
  :config
  (setq
   cquery-executable "~/.tools/cquery/release/bin/cquery"
   cquery-cache-dir-function 'cquery-cache-dir-consolidated))

(defun my/install-packages ()
  "Install necessary packages."
  (let ((progs (list "git" "cmake" cquery-executable "ag"))
	(prog-exist-p (lambda (p)
			(string-equal
			 (shell-command-to-string (concat "which " p))
			 ""))))
    (if (some prog-exist-p progs)
	(progn
	  (term "/bin/bash")
	  (switch-to-buffer "*terminal*")
	  (term-send-string  "*terminal*" "~/.emacs.d/install-packages && exit\n")))))

;; Once initialization is done, proceed to install
;; packages if needed. This ensures that the terminal
;; window is correctly focused.
(add-hook 'emacs-startup-hook 'my/install-packages)

(defun my/set-kill-buffer-sentinel ()
  "Set the process sentinel to kill buffer when the process exits."
  (let ((p (get-buffer-process (current-buffer))))
    (set-process-query-on-exit-flag p nil)
    (set-process-sentinel p
			  (lambda (p e)
			    (kill-buffer (current-buffer))))))
   

;; Kill window when a terminal, shell or eshell exits.
(add-hook 'term-exec-hook 'my/set-kill-buffer-sentinel)
(add-hook 'shell-mode-hook 'my/set-kill-buffer-sentinel)

;; Use screen
(use-package elscreen
  :config
  (elscreen-start)
  (elscreen-toggle-display-tab)

  ;; terminals
  ;;(ansi-term "/bin/bash")
  ;;(switch-to-buffer "*ansi-term*")
  ;;  (local-set-key (kbd "C-z n") 'elscreen-next)
  (eshell)
  (elscreen-screen-nickname "terminals")

  ;; editor
  (elscreen-create)
  (elscreen-screen-nickname "editor"))

(defvar my/orig-background-color (background-color-at-point))

(defun my/toggle-transparency ()
  "Toggle transparency."
  (interactive)
  (set-background-color "transparent"))


;;(use-package color-theme-solarized
;;   :config
;;   (load-theme 'solarized-dark 1))

(use-package atom-one-dark-theme)

(toggle-truncate-lines)

(profiler-start 'cpu)

(setq c-default-style "linux"
      c-basic-offset 4)

;;
;; Auto-generated code follows
;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "abe5ee8858cd1fbe36304a8c3b2315d3e0a4ef7c8588fcc45d1c23eafb725bb6" "8885761700542f5d0ea63436874bf3f9e279211707d4b1ca9ed6f53522f21934" "d057f0430ba54f813a5d60c1d18f28cf97d271fd35a36be478e20924ea9451bd" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d600c677f1777c1e4bfb066529b5b73c0179d0499dd4ffa3f599a0fb0cfbd501" "980db7875457fcefa2af9bcfcd97ca16bd844b0c7eacd9243bcb567a55d8ed21" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "bf5bdab33a008333648512df0d2b9d9710bdfba12f6a768c7d2c438e1092b633" default)))
 '(package-selected-packages
   (quote
    (color-theme-solarized color-theme-sanityinc-solarized zenburn-theme solarized-theme omtose-phellack-theme noctilux-theme nimbus-theme ample-theme elscreen cquery company-lsp company flycheck lsp-ui lsp-mode atom-one-dark-theme magit counsel ivy workgroups2 beacon ace-jump-mode ace-window use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
