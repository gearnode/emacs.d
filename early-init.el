;; Copyright (c) 2020 Bryan Frimin <bryan@frimin.fr>.
;;
;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.
;;
;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
;; REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
;; AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
;; INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
;; LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
;; OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
;; PERFORMANCE OF THIS SOFTWARE.

;; no need of package-quickstart since I use `straight.el' as package
;; manager.
(setq package-quickstart nil)

;; disable package enable at startup, since I am using `straight.el'
(setq package-enable-at-startup nil)
(advice-add #'package--ensure-init-file :override #'ignore)

;; comment out this line if you are not using emacs native compilation
;; branch native compile elisp files as they are loaded
;; (setq comp-deferred-compilation t)

;; defer garbage collection further back in the startup process.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; prevent the glimpse of un-styled Emacs by disabling these UI
;; elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; resizing the Emacs frame can be a terribly expensive part of changing
;; the font. By inhibiting this, I easily halve startup times with fonts
;; that are larger than the system default.
(setq frame-inhibit-implied-resize t)

;; ignore X resources; its settings would be redundant with the other
;; settings in this file and can conflict with later config
;; (particularly where the cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)
