;; Anand Krishnamoorthi's emacs setup

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8ffdc8c66ceeaf7921f4510a70d808f01b303e6b4d177c947b442e80d4228678" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" "adf5275cc3264f0a938d97ded007c82913906fc6cd64458eaae6853f6be287ce" "2642a1b7f53b9bb34c7f1e032d2098c852811ec2881eec2dc8cc07be004e45a0" "ec5f697561eaf87b1d3b087dd28e61a2fc9860e4c862ea8e6b0b77bd4967d0ba" default)))
 '(package-selected-packages
   (quote
    (tuareg caml merlin counsel smart-mode-line-atom-one-dark-theme smart-mode-line ace-window beacon powerline markdown-preview-eww monokai-theme cmake-mode magit ace-jump-mode planet-theme solarized-theme atom-one-dark-theme atom-dark-theme lsp-ui flycheck company-lsp company cquery lsp-mode zenburn-theme))))
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


;; backup folder
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; fix color names
(load-file "~/.emacs.d/color-names.el")

;; apply putty fixes
(load-file "~/.emacs.d/putty.el")

;; desktop
;;(desktop-save-mode)
;;(desktop-auto-save)

;; Fix frame loading in -nw mode
(setq desktop-restore-forces-onscreen nil)
(add-hook 'desktop-after-read-hook
 (lambda ()
   (frameset-restore
    desktop-saved-frameset
    :reuse-frames (eq desktop-restore-reuses-frames t)
    :cleanup-frames (not (eq desktop-restore-reuses-frames 'keep))
    :force-display desktop-restore-in-current-display
    :force-onscreen desktop-restore-forces-onscreen)))

;; Themes
(package-install 'zenburn-theme)
(package-install 'atom-dark-theme)
(package-install 'atom-one-dark-theme)
(load-theme 'atom-one-dark)

;; lsp
(package-install 'lsp-mode)
(require 'lsp-mode)
(add-hook 'c-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'lsp)
(setq lsp-prefer-flymake nil)

(defun my-lsp-mode-keys ()
  "Bind keys to lsp-functions."
  (define-key (current-local-map) (kbd "M-?") 'lsp-find-references))
(add-hook 'lsp-mode-hook 'my-lsp-mode-keys)

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
(setq cquery-cache-dir-function 'cquery-cache-dir-consolidated)

;; ace-jump-mode
;; Use C-c SPC 
(package-install 'ace-jump-mode)
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(define-key global-map (kbd "C-c l") 'ace-jump-line-mode)

;; windmove
;; Shift + arrow-key
(windmove-default-keybindings)


;; ace-window
;; M-o
(package-install 'ace-window)
(require 'ace-window)
(global-set-key (kbd "M-o") 'ace-window)


;; power-line
;;(require 'powerline)
;;(powerline-default-theme)

;; beacon-mode
(package-install 'beacon)
(require 'beacon)
(beacon-mode 1)

;; cmake-mode
(package-install 'cmake-mode)
(require 'cmake-mode)


;; column-number-mode
(setq column-number-mode t)

;; tool-bar-mode and menu-bar-mode off
(tool-bar-mode -1)
(menu-bar-mode -1)

;; window split thresholds
(setq split-height-threshold 2160)
(setq split-width-threshold 3840)

;; winner-mode
(winner-mode)


;; ivy and counsel
(package-install 'counsel)
(require 'ivy)
(require 'counsel)
(ivy-mode)
(counsel-mode)
(define-key global-map (kbd "C-c f") 'counsel-git)


;; named frames
(defun make-named-frame (name)
  "Make a frame with a given name"
  (interactive "sEnter frame name: ")
  (select-frame
   (make-frame (cons (cons 'name name) ()))))

(define-key global-map (kbd "C-x 5 2") 'make-named-frame)
(define-key global-map (kbd "C-z") 'other-frame)


;; eshell visual commands
(setq eshell-visual-commands
      '("gdb" "bash"))

;; Setup gdb layout
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


;; merlin
(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
 (when (and opam-share (file-directory-p opam-share))
  (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
  (autoload 'merlin-mode "merlin" nil t nil)
  (add-hook 'tuareg-mode-hook 'merlin-mode t)
  (add-hook 'caml-mode-hook 'merlin-mode t)))

(defun my-merlin-keys ()
  "Bind xref keys to merlin commands in tuareg mode."
  (interactive)
  (define-key (current-local-map) (kbd "M-.") 'merlin-locate)
  (define-key (current-local-map) (kbd "M-,") 'merlin-pop-stack)
  (define-key (current-local-map) (kbd "M-?") 'merlin-occurrences))
    
(add-hook 'tuareg-mode-hook 'my-merlin-keys)

;; magit
(package-install 'magit)

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
