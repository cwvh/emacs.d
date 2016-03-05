(package-initialize)

(add-to-list 'exec-path "/usr/local/bin")

;; Keep start-up clean of banners.
(setq initial-scratch-message nil
      inhibit-startup-message t)

;; Set sensible defaults for all modes.
(setq-default tab-width 8
              indent-tabs-mode nil)
(prefer-coding-system 'utf-8)
(global-font-lock-mode t)
(blink-cursor-mode t)
(show-paren-mode t)
(column-number-mode t)
(fset 'yes-or-no-p 'y-or-n-p)

;; Selecting a region and then typing will delete
(delete-selection-mode t)

;; Do not drop backup files in $PWD
(defvar backup-dir "~/.emacs.d/backups/")
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq make-backup-files nil)

;; Get rid of audible bell and drop backgrounding since it crashes on Mac.
(setq ring-bell-function 'ignore visible-bell t)
(global-unset-key "\C-z")

;; Make the usual copy-paste keys behave as expected.
(cua-mode 1)
(transient-mark-mode 1)

;; Disable menu bars and stuff
(if window-system
    (progn
      (scroll-bar-mode -1)
      (tool-bar-mode -1)
      (tooltip-mode -1))
  ;; Disable menu bars when run from terminal with -nw.
  (menu-bar-mode -1))

;; Add in some highly trafficked repos.
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE and return non-nil if successful.
In the event of failure return nil and print a warning message.
Optionally require MIN-VERSION. If NO-REFRESH is non-nil then the available
package lists will not be re-downloaded in order to locate PACKAGE."
  (condition-case err
      (require-package package min-version no-refresh)
    (error
     (message "Could not install package `%s': %S" package err)
     nil)))

(defun require-package (package &optional min-version no-refresh)
  "Install PACKAGE and optionally require MIN-VERSION of it.
If NO-REFRESH is non-nil then the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(setq package-enable-at-startup nil)
(package-initialize)

(maybe-require-package 'use-package)
(eval-when-compile (require 'use-package))

;; What follows are my insane ViM bindings that have come from
;; years of not properly learning to use ViM. If you intend to
;; use Evil mode, be sure to read through the key bindings first.
(defun ascii:config-evil-leader ()
  "Configure evil leader mode."
  (evil-leader/set-leader ",")
  (setq evil-leader/in-all-states 1)
  (evil-leader/set-key
    "B" 'magit-blame-toggle)

  (defun magit-blame-toggle ()
    "Toggle magit-blame-mode on and off interactively."
    (interactive)
    (if (and (boundp 'magit-blame-mode) magit-blame-mode)
        (magit-blame-quit)
      (call-interactively 'magit-blame))))

(defun ascii:config-evil ()
  "Configure evil mode."
  ;; Use emacs state in these additional modes.
  (dolist (mode '(flycheck-error-list-mode
                  git-rebase-mode))
    (add-to-list 'evil-emacs-state-modes mode))

  ;; Use INSERT state in these additional modes.
  (dolist (mode '(magit-log-edit-mode))
    (add-to-list 'evil-insert-state-modes mode))

  (evil-add-hjkl-bindings occur-mode-map 'emacs
    (kbd "/") 'evil-search-forward
    (kbd "n") 'evil-search-next
    (kbd "N") 'evil-search-previous
    (kbd "C-d") 'evil-scroll-down
    (kbd "C-u") 'evil-scroll-up
    (kbd "C-w C-w") 'other-window)

  (define-key evil-normal-state-map (kbd "C-p") 'helm-projectile)
  (define-key evil-normal-state-map (kbd "C-]") 'gtags-find-tag-from-here)
  ;; Accept some of my quasi-vim brain damage.
  (define-key evil-normal-state-map (kbd "C-k") (lambda () (interactive) (previous-line 6)))
  (define-key evil-normal-state-map (kbd "C-j") (lambda () (interactive) (next-line 6)))
  ;; Accept some of my "set -o emacs" brain damage.
  (define-key evil-normal-state-map (kbd "C-a") 'beginning-of-line-text)
  (define-key evil-normal-state-map (kbd "C-e") 'end-of-line)

  (defun minibuffer-keyboard-quit ()
    "Abort recursive edit.
In DELETE-SELECTION-MODE, if the mark is active just deactivate it; then
it takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setq deactivate-mark t)
      (when (get-buffer "*Completions*")
        (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))

  ;; Make ESC powerful like god intended.
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit))

(use-package helm
  :ensure t
  :defer t
  :diminish helm-mode
  :init
  (setq helm-buffers-fuzzy-matching t)
  :config
  (setq helm-autoresize-mode t)
  (setq helm-buffer-max-length 40))

(use-package evil
  :ensure t
  :config
  (add-hook 'evil-mode-hook 'ascii:config-evil)
  ;; Uncomment to enable ViM on Emacs start-up.
  ;; Evil mode can be turned on for any buffer with
  ;; M-x evil-mode and turned off with M-x turn-off-evil-mode.
;  (evil-mode 1)

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    (ascii:config-evil-leader))

  (use-package powerline-evil
    :ensure t))

(when (memq window-system '(mac ns))
  (setq ns-use-srgb-colorspace nil))

(use-package powerline
  :ensure t
  :config
  (setq powerline-default-separator (if (display-graphic-p) 'slant
                                      nil)))

(use-package exec-path-from-shell
  :ensure t
  :defer t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package company
  :ensure t
  :defer t
  :init
  (global-company-mode)
  :config
  (setq company-idle-delay 0.5)
  (setq company-selection-wrap-around t)
  (define-key company-active-map [tab] 'company-complete)
  (define-key company-active-map [kbd "C-n"] 'company-select-next)
  (define-key company-active-map [kbd "C-p"] 'company-select-previous))

(use-package flycheck :ensure t)

(use-package helm-projectile
  :commands (helm-projectile helm-projectile-switch-project)
  :ensure t)

(use-package projectile
  :ensure t
  :defer t
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t))

(use-package highlight-symbol
  :ensure t
  :defer t
  :diminish ""
  :config
  (setq-default highlight-symbol-idle-delay 0.5))

(use-package magit
  :ensure t
  :defer t
  :config
  (setq magit-branch-arguments nil)
  (setq magit-push-always-verify nil))

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :init
  (load-theme 'sanityinc-tomorrow-night t))

;; Shell mode
(add-hook 'sh-mode-hook (lambda ()
                          (setq sh-basic-offset 2
                                sh-indentation 2)))

(setq c-default-style "stroustrup")
(add-hook 'c-mode-common-hook '(lambda () (c-toggle-hungry-state 1)))

;; Put site customization here. Good examples would
;; be setting system-specific fonts and sizes.
(load-file "~/.emacs.d/custom.el")
