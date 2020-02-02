;;; -*- lexical-binding: t -*-
(require 'seq)

;; Package
(package-initialize)

(setq package-user-dir "~/.emacs.d/packages")

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")))

(setq package-selected-packages
      '(company
        company-go
        company-quickhelp
        csv-mode
        disable-mouse
        dockerfile-mode
        erlang
	exec-path-from-shell
        forge
        go-mode
        helm
        helm-ag
        json-mode
        magit
        markdown-mode
        mustache-mode
        protobuf-mode
        terraform-mode
        yaml-mode))

(defalias 'yes-or-no-p 'y-or-n-p)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Backup / auto-save file
(setq make-backup-files nil)
(setq auto-save-default nil)

;; File local variables
(setq enable-local-variables nil)

;; Frame
(add-to-list 'default-frame-alist '(height . 101))
(add-to-list 'default-frame-alist '(width . 170))

;; Cursor {
(blink-cursor-mode 0)
(setq x-stretch-cursor t)


;; UI {
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(setq column-number-mode t)

(global-display-line-numbers-mode)

;; desktop-mode
(setq desktop-path (list user-emacs-directory))
(setq desktop-auto-save-timeout 30)
(desktop-save-mode 1)

;; disable-mode
(global-disable-mouse-mode)
(setq-default global-disable-mouse-mode-lighter "")

;; company-mode
(require 'company)

(company-quickhelp-mode 1)

(setf company-idle-delay nil)

(add-hook 'after-init-hook 'global-company-mode)

(global-set-key (kbd "M-TAB") 'company-complete)
(define-key emacs-lisp-mode-map (kbd "M-TAB") 'company-complete)

(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "TAB") 'company-complete-selection)
(define-key company-active-map [tab] 'company-complete-selection)

;; magit-mode / forge-mode
(with-eval-after-load 'magit
  (require 'forge))

;; HELM {
(require 'helm-config)

(helm-mode 1)

;; Layout
(setq helm-always-two-window nil)
(setq helm-display-buffer-default-height 16)
(setq helm-default-display-buffer-functions '(display-buffer-in-side-window))

;; Command
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x g") 'magit-status)

;; Buffers
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)

(setq helm-buffer-max-length 30)

(setq helm-borring-buffer-regexp-list
      (list
       "\\*"
       "\\*Echo Area"
       "\\*Minibuf"
       "\\*Shell Command Output\\*"
       "\\*helm "
       "\\*godoc "))

(defun g-remove-trailing-whitespaces ()
  (delete-trailing-whitespace))

(add-hook 'before-save-hook 'g-remove-trailing-whitespaces)

;; Files
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; protobuf-mode
(require 'protobuf-mode)

(defun g-protobuf-mode-init ()
  (c-add-style "protobuf"'((c-basic-offset . 2)) t))

(add-hook 'protobuf-mode-hook 'g-protobuf-mode-init)

;; markdown-mode
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mkd\\'" . markdown-mode))

;; json-mode
(defun g-json-mode-init ()
  (setq js-indent-level 2))

(add-hook 'json-mode-hook 'g-json-mode-init)

;; go-mode
(add-hook 'before-save-hook 'gofmt-before-save)

;; makefile-mode
(defun g-makefile-mode-init ()
  (setq tab-width 8))

(add-hook 'makefile-mode-hook 'g-makefile-mode-init)

;; Font
(face-spec-set 'default '((t :font "Roboto Mono for Powerline")))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (tango-dark)))
 '(package-selected-packages
   (quote
    (ztree company-quickhelp exec-path-from-shell terraform-mode helm-ag json-mode forge elixir-mode erlang disable-mouse company-go company protobuf-mode yaml-mode markdown-mode go-mode org-plus-contrib org magit helm))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
