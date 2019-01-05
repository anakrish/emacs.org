

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("ec5f697561eaf87b1d3b087dd28e61a2fc9860e4c862ea8e6b0b77bd4967d0ba" default)))
 '(package-selected-packages
   (quote
    (cmake-mode magit ace-jump-mode planet-theme solarized-theme atom-one-dark-theme atom-dark-theme lsp-ui flycheck company-lsp company cquery lsp-mode zenburn-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'org)

;; Load customizations.
;; Note: Themest be marked safe (above) before loading so that
;; there is no prompt each time emacs is run.
;;(org-babel-load-file
;; (expand-file-name "emacs.org" user-emacs-directory))

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

;; Themes
(package-install 'zenburn-theme)
(load-theme 'zenburn)

;; lsp
(package-install 'lsp-mode)
(require 'lsp-mode)
(add-hook 'c-mode-hook #'lsp)
(add-hook 'cxx-mode-hook #'lsp)
;;(setq lsp-prefer-flymake nil)

;; lsp-ui
(package-install 'lsp-ui)
(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

;; company
(package-install 'company)
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; company-lsp
(package-install 'company-lsp)
(require 'company-lsp)

;; flycheck
(package-install 'flycheck)
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; cquery
(package-install 'cquery)
(require 'cquery)
(setq cquery-executable "/home/anakrish/work/cquery/install/bin/cquery")

;; ace-jump-mode
;; Use C-c SPC 
(package-install 'ace-jump-mode)
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
 
;; windmove
;; Shift + arrow-key
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <left>") 'windmove-left)

;; cmake-mode
(package-install 'cmake-mode)
(setq load-path (cons (expand-file-name "/dir/with/cmake-mode") load-path))
(require 'cmake-mode)


;; column-number-mode
(setq column-number-mode t)

;; magit
;; (package-install 'magit)

