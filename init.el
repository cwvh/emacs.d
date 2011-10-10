(let ((cwvh:path "/usr/local/bin:/bin:/usr/bin:~/go/bin"))
  (setenv "PATH" cwvh:path)
  (setq exec-path (split-string cwvh:path path-separator)))

(setq inhibit-startup-message t)
(setq-default tab-width 8)
(setq-default indent-tabs-mode nil)
(prefer-coding-system 'utf-8)
(setq make-backup-files nil)
(setq auto-save-default nil)

(setq ring-bell-function 'ignore)
(global-unset-key "\C-z")

(fset 'yes-or-no-p 'y-or-n-p)

(delete-selection-mode t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode t)
(show-paren-mode t)
(column-number-mode t)
(set-fringe-style -1)
(tooltip-mode -1)

(set-frame-font "Menlo-12")
(load-theme 'wombat)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(eval-after-load 'shell
  '(progn
     (define-key shell-mode-map [up] 'comint-previous-input)
     (define-key shell-mode-map [down] 'comint-next-input)
     (define-key shell-mode-map "\C-p" 'comint-previous-input)
     (define-key shell-mode-map "\C-n" 'comint-next-input)))

;; el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (end-of-buffer)
    (eval-print-last-sexp)))
(el-get 'sync)

;; Go
(add-to-list 'load-path "~/go/misc/emacs" t)
(require 'go-mode-load)
(add-hook 'before-save-hook #'gofmt-before-save)

(defun cwvh:c-initialization-hook ()
  (define-key c-mode-base-map (kbd "RET") 'newline-and-indent))
(add-hook 'c-initialization-hook 'cwvh:c-initialization-hook)

(defun cwvh:c-mode-hook ()
  (c-set-style "linux")
  (setq c-basic-offset 8
        tab-width 8
        indent-tabs-mode t))
(add-hook 'c-mode-hook 'cwvh:c-mode-hook)

(defun cwvh:c++-mode-hook ()
  (c-set-style "stroustrup")
  (setq c-basic-offset 4
        tab-width 4
        indent-tabs-mode nil))
(add-hook 'c++-mode-hook 'cwvh:c++-mode-hook)
