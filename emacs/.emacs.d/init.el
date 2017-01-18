;;; init.el --- Duncan Mac-Vicar P. emacs init
(defconst emacs-start-time (current-time))
(require 'cl)
;; do this early
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;;(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance & Behavior
;;
(use-package ido
  :ensure t
  :defer t
  :init
  (progn
    (setq gc-cons-threshold (* 20 (expt 2 20)) ; megabytes
          ido-use-faces nil)
    (setq ido-create-new-buffer 'always))
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (use-package flx-ido
    :ensure t
    :defer t
    :config
    (flx-ido-mode 1)))

(use-package popwin :ensure t)

(defun set-frame-size-according-to-resolution ()
  (interactive)
  (when (display-graphic-p)
  (progn
    ;; use 120 char wide window for largeish displays
    ;; and smaller 80 column windows for smaller displays
    ;; pick whatever numbers make sense for you
    (if (> (x-display-pixel-width) 1280)
           (add-to-list 'default-frame-alist (cons 'width 120))
           (add-to-list 'default-frame-alist (cons 'width 80)))
    ;; for the height, subtract a couple hundred pixels
    ;; from the screen height (for panels, menubars and
    ;; whatnot), then divide by the height of a char to
    ;; get the height we want
    (add-to-list 'default-frame-alist 
         (cons 'height (/ (- (x-display-pixel-height) 200)
                             (frame-char-height)))))))

(set-frame-size-according-to-resolution)
;(set-face-attribute 'default nil :font "Consolas-11")
(set-face-attribute 'default nil :font "Monospace")

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)
;; disable startup screen
(setq inhibit-startup-screen t)
;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
  '("" invocation-name " - " (:eval (if (buffer-file-name)
                                       (abbreviate-file-name (buffer-file-name))
                                     "%b"))))

(use-package highlight-indentation
  :ensure t
  :init
  (setq highlight-indentation-offset 2))

(use-package powerline
  :ensure t
  :config
  (progn
    (powerline-default-theme))
)

(use-package leuven-theme
  :ensure t
  :config
  (load-theme 'leuven t))

(setq show-trailing-whitespace t)
(setq-default show-trailing-whitespace t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Behavior
;;
(use-package popwin
  :ensure t
  :config
  (popwin-mode 1)
)

;; custom shortcuts
(global-set-key (kbd "C-c -") 'split-window-vertically)
(global-set-key (kbd "C-c |") 'split-window-horizontally)
(global-set-key (kbd "C-c x") 'delete-window)
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

(windmove-default-keybindings)
(setq windmove-wrap-around t)

;; X11 keyboard in console
(defun xclip-tramp-fix (orig-fun &rest args)
  ;; temporary set current dir to local machine
  (let ((default-directory "/"))
    (apply orig-fun args)))

(advice-add 'xclip-selection-value :around #'xclip-tramp-fix)
(advice-add 'xclip-select-text :around #'xclip-tramp-fix)
(use-package xclip
  :ensure t
  :config
  (progn (xclip-mode 1) (turn-on-xclip))
)

;;;; scrolling
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common programming and tools
;;
(use-package multi-term :ensure t)

(use-package projectile
  :ensure t
  :config (projectile-global-mode t)
  :diminish projectile-mode
)

(use-package company
  :ensure t
  :config (add-hook 'prog-mode-hook 'company-mode))

(use-package magit :ensure t)

(use-package flycheck
  :ensure t
  :defer t
  :init
  (defun my-flycheck-prog-mode-hook ()
    (flycheck-mode t))
  (add-hook 'prog-mode-hook 'my-flycheck-prog-mode-hook))

(use-package flycheck-pos-tip
  :ensure t
  :after (flycheck)
  :config
  (with-eval-after-load 'flycheck (flycheck-pos-tip-mode)))

;; Auto refresh buffers
(global-auto-revert-mode 1)
;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Lines should be 80 characters wide, not 72
(setq fill-column 80)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Delete selected text when typing
(delete-selection-mode 1)

;;fix flyspell shortcut
(global-set-key (kbd "<C-tab>") 'flyspell-auto-correct-word)
;;fix auto-complete
;(ac-flyspell-workaround)

;; key board / input method settings
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

; never use tabs
(setq-default indent-tabs-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Backup files
(setq auto-save-default nil)
(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.saves"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups

(global-set-key (kbd "M-RET") 'electric-buffer-list)
(global-set-key (kbd "C-M-j") 'electric-buffer-list)

(add-hook 'Buffer-menu-mode-hook 'buffer-disable-undo)

;(cua-mode 1)
(setq shift-select-mode nil)
(setq x-alt-keysym 'meta)

(use-package nav
  :ensure t
  :config
  (nav-disable-overeager-window-splitting))

; highlight the current line
(global-hl-line-mode t)

; tabs
(setq tabbar-ruler-global-tabbar t) ; If you want tabbar
(use-package tabbar-ruler
  :ensure t
  :config
  (tabbar-ruler-group-by-projectile-project))

(mapc
  (lambda (face)
    (set-face-attribute face nil :weight 'normal :underline nil))
  (face-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Session save/restore support
;; Minimal setup from http://www.emacswiki.org/emacs/DeskTop
;; use only one desktop
(setq desktop-path (list (expand-file-name user-emacs-directory)))
(setq desktop-dirname (expand-file-name user-emacs-directory))
(setq desktop-base-file-name "emacs-desktop")

;; remove desktop after it's been read
(add-hook 'desktop-after-read-hook
	  '(lambda ()
	     ;; desktop-remove clears desktop-dirname
	     (setq desktop-dirname-tmp desktop-dirname)
	     (desktop-remove)
	     (setq desktop-dirname desktop-dirname-tmp)))

(defun saved-session ()
  (file-exists-p (concat desktop-dirname "/" desktop-base-file-name)))

;; use session-restore to restore the desktop manually
(defun session-restore ()
  "Restore a saved emacs session."
  (interactive)
  (if (saved-session)
      (desktop-read)
    (message "No desktop found.")))

;; use session-save to save the desktop manually
(defun session-save ()
  "Save an emacs session."
  (interactive)
  (if (saved-session)
      (if (y-or-n-p "Overwrite existing desktop? ")
	  (desktop-save-in-desktop-dir)
	(message "Session not saved."))
  (desktop-save-in-desktop-dir)))

;; ask user whether to restore desktop at start-up
(add-hook 'after-init-hook
	  '(lambda ()
	     (if (saved-session)
		 (if (y-or-n-p "Restore desktop? ")
		     (session-restore)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; language specific tweaks
;;
;; C, C++ and other compiled languages
(use-package rust-mode
  :ensure t
  :config
  (progn
    (use-package flycheck-rust :ensure t :after (flycheck))))

(use-package go-mode
  :ensure t
  :config
  (progn
    (use-package company-go
      :ensure t)
    (add-hook 'go-mode-hook
              (lambda ()
                (setq tab-width 4)
                (set (make-local-variable 'company-backends) '(company-go))
                (company-mode)))))

(defun my-c-mode-hook ()
  (setq c-basic-offset 4)
  (c-set-offset 'substatement-open 0))
(add-hook 'c-mode-common-hook 'my-c-mode-hook)

;; Scripting languages
(use-package rinari :ensure t)
(use-package bundler :ensure t)
(use-package robe
  :ensure t
  :init
  (progn (add-hook 'ruby-mode-hook 'robe-mode)
         (push 'company-robe company-backends)))
;;         (add-hook 'robe-mode-hook 'ac-robe-setup)
;;         (add-hook 'ruby-mode-hook 'auto-complete-mode)))

(use-package python
  :ensure t
  :config
  (progn
    (setq-default python-indent 4)
    (use-package flycheck-pyflakes
      :ensure t
      :defer t
      :after (flycheck))))

(use-package feature-mode
  :ensure t
  :mode ("\\.feature$" . feature-mode))

;; web development
(use-package web-mode
  :ensure t
  :mode (("\\.erb\\'" . web-mode) ("\\.html?\\'" . web-mode))
  :config
    (setq web-mode-enable-css-colorization t)
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-style-padding 2)
    (setq web-mode-script-padding 2))
(use-package less-css-mode :ensure t)
;;; Colourise CSS colour literals
(use-package rainbow-mode
  :if (eval-when-compile (>= emacs-major-version 24))
  :ensure t
  :config
  (dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook))
    (add-hook hook 'rainbow-mode)))

;; markup formats
(use-package yaml-mode
  :ensure t
  :mode (("\\.yml$" . yaml-mode) ("\\.sls\\'" . yaml-mode)))
(use-package salt-mode :ensure t)
(use-package markdown-mode :ensure t)
(use-package toml-mode :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other functionality (org, calendar)
(use-package calfw :ensure t)

(use-package org
  :ensure t
  :config
  (progn           
    (setq org-src-fontify-natively t)
    (setq org-fontify-whole-heading-line t)
    (setq org-pretty-entities t)
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
    (define-key org-mode-map (kbd "M-RET") nil)))

;; work setup
(if (file-exists-p "~/.emacs.suse.d/init.el")
    (load-file "~/.emacs.suse.d/init.el"))

;; Hacks
;;
;; Hack to fix a bug with tabulated-list.el
;; see: http://redd.it/2dgy52
(defun tabulated-list-revert (&rest ignored)
  "The `revert-buffer-function' for `tabulated-list-mode'.
It runs `tabulated-list-revert-hook', then calls `tabulated-list-print'."
  (interactive)
  (unless (derived-mode-p 'tabulated-list-mode)
    (error "The current buffer is not in Tabulated List mode"))
  (run-hooks 'tabulated-list-revert-hook)
  ;; hack is here
  ;; (tabulated-list-print t)
  (tabulated-list-print))

;; tramp mode
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

(when window-system
  (let
    ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))
  (add-hook 'after-init-hook
    `(lambda ()
       (let ((elapsed (float-time (time-subtract (current-time)
                                                 emacs-start-time))))
         (message "Loading %s...done (%.3fs) [after-init]"
                  ,load-file-name elapsed)))
    t))
(provide 'init)
