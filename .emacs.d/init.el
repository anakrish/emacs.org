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
(add-hook 'cxx-mode-hook #'lsp)

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

;; keyboard shortcuts
;; use xref bindings instead.
;; M-x describe-bindings
;;(defun my-c-mode-shortcuts ()
;;  (interactive)
;;  ;;(local-set-key (kbd "C-f") 'lsp-find-definition) ;; M-. already does this.
;;  (local-set-key (kbd "C-r") 'lsp-find-references))

;;(add-hook 'c-mode-hook #'my-c-mode-shortcuts)
;;(add-hook 'cxx-mode-hook #'my-c-mode-shortcuts)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (company lsp-mode zygospore zenburn-theme yasnippet xterm-color ws-butler w3 volatile-highlights use-package undo-tree theme-changer spacemacs-theme soothe-theme solarized-theme rtags pos-tip planet-theme pdf-tools omtose-phellack-theme noctilux-theme monokai-theme molokai-theme magit lsp-ui lsp-clangd iedit helm-swoop helm-projectile helm-gtags gruvbox-theme gotham-theme flycheck dtrt-indent darktooth-theme cycle-themes cquery company-lsp comment-dwim-2 color-theme-solarized cmake-mode cmake-ide clean-aindent-mode badger-theme auto-complete-clang atom-one-dark-theme atom-dark-theme anzu ample-theme afternoon-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
