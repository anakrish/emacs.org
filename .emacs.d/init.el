;; anakrish's emacs setup

(require 'org)

;; MELPA
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
    Your version of Emacs does not support SSL connections,
    which is unsafe because it allows man-in-the-middle attacks.
    There are two things you can do about this warning:
    1. Install an Emacs version that does support SSL and be safe.
    2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; disable window split
(setq split-height-threshold 2000)
(setq split-width-threshold 2000)

;; themes
(package-install 'zenburn-theme)
(package-install 'atom-dark-theme)
(package-install 'atom-one-dark-theme)
;;(package-install 'solarized-dark-theme)
(load-theme 'zenburn t)


;; lsp
(package-install 'lsp-mode)
(require 'lsp-mode)
(add-hook 'c-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'lsp)

;; lsp-ui
(package-install 'lsp-ui)
(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

;; cquery
(package-install 'cquery)
(require 'cquery)
(setq cquery-executable "~/install/cquery/bin/cquery")

;; company
(package-install 'company)
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; flycheck
(package-install 'flycheck)
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)


;; windmove navigation. shift + array keys
(windmove-default-keybindings)


;; misc
(beacon-mode 1)

(require 'treemacs)

;; allow multiple daemons
;;(setq server-use-tcp t)
;;(require 'elscreen)
;;(elscreen-start)

;; color-theme-approximate
(require 'color-theme-approximate)
(color-theme-approximate-on)

;; ace-jump-mode
;; default binding
(package-install 'ace-jump-mode)
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)


(defun make-named-frame (name)
  "Make a frame with a given name"
  (interactive "sEnter frame name: ")
  (select-frame
   (make-frame (cons (cons 'name name) ()))))

(define-key global-map (kbd "C-x 5 2") 'make-named-frame)
(define-key global-map (kbd "C-z") 'other-frame)

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
  (split-window-horizontally)
  (gdb-set-window-buffer
   (gdb-get-buffer-create 'gdb-registers-buffer))
  (other-window 4))


(setq split-width-threshold 3840)
(setq split-height-threshold 2160)

(menu-bar-mode 0)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("ec5f697561eaf87b1d3b087dd28e61a2fc9860e4c862ea8e6b0b77bd4967d0ba" "2642a1b7f53b9bb34c7f1e032d2098c852811ec2881eec2dc8cc07be004e45a0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "adf5275cc3264f0a938d97ded007c82913906fc6cd64458eaae6853f6be287ce" default)))
 '(package-selected-packages
   (quote
    (elscreen color-theme-approximate treemacs beacon neotree scheme-complete slime ace-jump-mode powerline company lsp-mode zygospore zenburn-theme yasnippet xterm-color ws-butler w3 volatile-highlights use-package undo-tree theme-changer spacemacs-theme soothe-theme solarized-theme rtags pos-tip planet-theme pdf-tools omtose-phellack-theme noctilux-theme monokai-theme molokai-theme magit lsp-ui lsp-clangd iedit helm-swoop helm-projectile helm-gtags gruvbox-theme gotham-theme flycheck dtrt-indent darktooth-theme cycle-themes cquery company-lsp comment-dwim-2 color-theme-solarized cmake-mode cmake-ide clean-aindent-mode badger-theme auto-complete-clang atom-one-dark-theme atom-dark-theme anzu ample-theme afternoon-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
