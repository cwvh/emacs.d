(setq initial-scratch-message nil)
(setq inhibit-startup-message t)
(setq-default tab-width 2)
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

(auto-fill-mode 1)
(setq fill-column 79)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(if window-system
    (progn
      (scroll-bar-mode -1)
      (tool-bar-mode -1)
      (tooltip-mode -1)
      (set-frame-font "Anonymous Pro-13"))
  ;; -nw
  (menu-bar-mode -1))
(load-theme 'moe-dark t)

(delete-selection-mode t)
(blink-cursor-mode t)
(show-paren-mode t)
(column-number-mode t)

(when (memq window-system '(mac ns))
  (progn
    (setq mac-command-modifier 'meta)
    (global-set-key (quote [M-f10]) (quote ns-toggle-fullscreen))))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(eval-after-load 'shell
  '(progn
     (define-key shell-mode-map [up] 'comint-previous-input)
     (define-key shell-mode-map [down] 'comint-next-input)
     (define-key shell-mode-map "\C-p" 'comint-previous-input)
     (define-key shell-mode-map "\C-n" 'comint-next-input)))

(require 'tramp)
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
(setq tramp-default-method "ssh")

(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(defun cwvh:c-initialization-hook ()
  (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
  (setq c-basic-offset 4
        tab-with 4
        indent-tabs-mode nil))
(add-hook 'c-initialization-hook 'cwvh:c-initialization-hook)

(defun cwvh:c-mode-hook ()
  (c-set-style "linux"))
(add-hook 'c-mode-hook 'cwvh:c-mode-hook)

(c-add-style "tc++pl"
             '("stroustrup"
               (c-offsets-alist
                (inline-open . 0))))

(defun cwvh:c++-mode-hook ()
  (c-set-style "tc++pl"))
(add-hook 'c++-mode-hook 'cwvh:c++-mode-hook)

(require 'slime-autoloads)
(setq inferior-lisp-program "sbcl")
(setq slime-contribs '(slime-fancy))

(defun cwvh:haskell-mode-hook ()
  (turn-on-haskell-decl-scan)
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indentation)
  (setq tab-width 2
        haskell-indent-offset 2
        haskell-indentation-layout-offset 2
        haskell-indentation-left-offset 2
        haskell-indentation-ifte-offset 2))
(add-hook 'haskell-mode-hook 'cwvh:haskell-mode-hook)

(ido-mode)
