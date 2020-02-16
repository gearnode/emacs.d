;;; -*- lexical-binding: t -*-
(require 'seq)

(setq package-user-dir
      (expand-file-name "packages"
                        user-emacs-directory))

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))
(package-initialize)

(require 'org)
(org-babel-load-file
  (expand-file-name "configuration.org"
                   user-emacs-directory))

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
    (dockerfile-mode yasnippet ztree company-quickhelp exec-path-from-shell terraform-mode helm-ag json-mode forge elixir-mode erlang disable-mouse company-go company protobuf-mode yaml-mode markdown-mode go-mode org-plus-contrib org magit helm))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
