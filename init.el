(let ((cwvh:path "/Users/cwvh/.cabal/bin:/usr/local/bin:/usr/bin/"))
  (setenv "PATH" cwvh:path)
  (setq exec-path (split-string cwvh:path path-separator)))

(setq initial-scratch-message nil)
(setq inhibit-startup-message t)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(prefer-coding-system 'utf-8)
(setq make-backup-files nil)
(setq auto-save-default nil)
(global-font-lock-mode t)

(setq ring-bell-function 'ignore)
(global-unset-key "\C-z")

(fset 'yes-or-no-p 'y-or-n-p)
(iswitchb-mode 1)
(icomplete-mode 1)

(if (window-system)
    (progn
      (scroll-bar-mode -1)
      (tool-bar-mode -1)
      (tooltip-mode -1)
      (set-frame-font "Menlo-12")
      (load-theme 'tango-dark))
  ;; -nw
    (menu-bar-mode -1))

(delete-selection-mode t)
(blink-cursor-mode t)
(show-paren-mode t)
(column-number-mode t)
;(set-fringe-style -1)

(if (or (eq window-system 'ns)
        (eq window-system 'mac))
    (progn
      (setq mac-command-modifier 'meta)
      ;(setq mac-option-modifier nil)
      ;(setq ns-function-modifier 'hyper)
      ;; keybinding to toggle fullscreen
      (global-set-key (quote [M-f10]) (quote ns-toggle-fullscreen))))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(eval-after-load 'shell
  '(progn
     (define-key shell-mode-map [up] 'comint-previous-input)
     (define-key shell-mode-map [down] 'comint-next-input)
     (define-key shell-mode-map "\C-p" 'comint-previous-input)
     (define-key shell-mode-map "\C-n" 'comint-next-input)))

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(require 'hungry-delete)
(global-hungry-delete-mode)

(define-key global-map (kbd "RET") 'newline-and-indent)

(defun cwvh:c-initialization-hook ()
  (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
  (setq c-basic-offset 4
        tab-with 4
        indent-tabs-mode nil))
(add-hook 'c-initialization-hook 'cwvh:c-initialization-hook)

(defun cwvh:c-mode-hook ()
  (c-set-style "k&r"))
(add-hook 'c-mode-hook 'cwvh:c-mode-hook)

(defun cwvh:c++-mode-hook ()
  (c-set-style "stroustrup"))
(add-hook 'c++-mode-hook 'cwvh:c++-mode-hook)

;(defun cwvh:slime-common-lisp ()
;  (interactive)
;  (setq inferior-lisp-program "sbcl")
;  (add-to-list 'load-path "~/.emacs.d/slime-2012-04-24")
;  (require 'slime)
;  (setq slime-protocol-version 'ignore)
;  (slime-setup '(slime-fancy))
;  (define-key slime-mode-map (kbd "<tab>") 'slime-indent-and-complete-symbol))

;; (defun turn-on-paredit () (paredit-mode 1))
;; (add-hook 'clojure-mode-hook 'turn-on-paredit)
;; (add-hook 'slime-repl-mode-hook
;;           (defun clojure-mode-slime-font-lock ()
;;             (require 'clojure-mode)
;;             (let (font-lock-mode)
;;               (clojure-mode-font-lock-setup))))

;; (defun cwvh:slime-common-lisp ()
;;   (add-to-list 'load-path "~/.emacs.d/slime")
;;   (setq inferior-lisp-program "sbcl")
;;   (require 'slime)
;;   (slime-setup '(slime-fancy))
;;   (define-key slime-mode-map (kbd "<tab>") 'slime-indent-and-complete-symbol))

(add-to-list 'load-path "~/.cabal/share/ghc-mod-1.11.3")
(autoload 'ghc-init "ghc" nil t)
(defun cwvh:haskell-mode-hook ()
  (set (make-local-variable 'require-final-newline) t)
  (turn-on-haskell-decl-scan)
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indentation)
  (setq tab-width 4
        haskell-indent-offset 4
        haskell-indentation-layout-offset 4
        haskell-indentation-left-offset 4
        haskell-indentation-ifte-offset 4)
  (auto-fill-mode)
  (ghc-init))
(add-hook 'haskell-mode-hook 'cwvh:haskell-mode-hook)
