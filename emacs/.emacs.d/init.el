;;; init.el --- Duncan Mac-Vicar P. emacs init
;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))
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
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package quelpa
  :ensure t
  :init
  (progn
    ;; we use quelpa mainly for non-melpa/unpublished packages
    (setq quelpa-update-melpa-p nil)
    (setq quelpa-checkout-melpa-p nil)))
(use-package quelpa-use-package
  :ensure t
  :config
  (setq quelpa-self-upgrade-p nil))

(eval-when-compile
  (require 'use-package))

; Diminished modes are minor modes with no modeline display
(use-package diminish :ensure t)
(require 'bind-key)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance & Behavior
;;
; I don't want emacs to overwrite my init.el with customization
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
   (when (file-exists-p custom-file)
       (load custom-file :noerror))

; scratch mode settings
(setq initial-major-mode 'text-mode)

; http://pragmaticemacs.com/emacs/dont-kill-buffer-kill-this-buffer-instead/
(defun dmacvicar/kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'dmacvicar/kill-this-buffer)

; allow y or n answers
(fset 'yes-or-no-p 'y-or-n-p)

; disable function keys
(dotimes (i 12)
  (global-unset-key (kbd (format "<f%d>" (+ i 1)))))

; disable overwrite mode
(define-key global-map [(insert)] nil)

; so that vc mode do not ask again
(setq vc-follow-symlinks t)

; recent files
(use-package recentf
  :ensure t
  ;; Loads after 1 second of idle time.
  :bind ("C-x C-r" . recentf-open-files)
  :init
  :config
  (progn
    ; Save the list every 5 minutes
    (recentf-mode 1) (run-at-time nil (* 5 60) 'recentf-save-list)))

(use-package f
  :defer t
  :ensure t)

(use-package perspeen
  :defer t
  :ensure t
  :init
  (setq perspeen-use-tab t)
  :config
  (perspeen-mode))

(use-package popwin :ensure t)

(use-package xclip
  :ensure t
  ;; Loads after 1 second of idle time.
  :defer 1
  :config (xclip-mode 1))

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
(if window-system
  (set-face-attribute 'default nil :font "Monospace"))

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)
;; disable startup screen
(setq inhibit-startup-screen t)
(setq-default inhibit-startup-message t)
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

;;;; scrolling
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common programming and tools
;;
(use-package multi-term
  :defer t
  :ensure t)

(use-package ag
  :defer t
  :ensure t
  :config
  (add-hook 'ag-mode-hook 'toggle-truncate-lines)
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers 't))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (progn
    (ivy-mode 1)
    (bind-key "C-c C-r" 'ivy-resume)
    (use-package ivy-rich
      :ensure t
      :init
      (progn
        (setq ivy-virtual-abbreviate 'full)
        (setq ivy-rich-switch-buffer-align-virtual-buffer t)
        (setq ivy-rich-path-style 'abbrev))
      :config
      (progn
        (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer)))))

(use-package counsel
  :after ivy
  :diminish
  :ensure t
  :bind
  ("M-x" . counsel-M-x)
;  ("C-z f" . counsel-describe-function)
;  ("C-z v" . counsel-describe-variable)
  ("C-c k" . counsel-ag))

(use-package projectile
  :ensure t
  ;; Loads after 1 second of idle time.
  :defer 1
  :init
  (progn
    (setq projectile-mode-line
          '(:eval (format " [%s]" (projectile-project-name))))
    (setq projectile-remember-window-configs t)
    (setq projectile-completion-system 'ivy))
  :config
  (progn
    (projectile-global-mode)
    (use-package counsel-projectile
      :ensure t
      :config
      (progn
        (counsel-projectile-mode)))))

(use-package company
  :ensure t
  :config (add-hook 'prog-mode-hook 'company-mode))

(use-package magit
  :defer t
  :ensure t)

(use-package flycheck
  :ensure t
  :defer t
  :init
  (progn
    (setq flycheck-command-wrapper-function
          (lambda (command)
            (append '("bundle" "exec") command))))
  :config
  (global-flycheck-mode))

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

;(global-set-key (kbd "M-RET") 'electric-buffer-list)
;(global-set-key (kbd "C-M-j") 'electric-buffer-list)
(global-set-key (kbd "M-RET") 'ivy-switch-buffer)
(global-set-key (kbd "C-M-j") 'ivy-switch-buffer)

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
  :defer t
  :ensure t
  :config
  (progn
    (use-package flycheck-rust
      :hook rust-mode
      :after (flycheck)
      :ensure t)))

(use-package go-mode
  :defer t
  :ensure t
  :init
  (progn
    (use-package company-go
      :hook go-mode
      :ensure t))
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              (setq tab-width 4)
              (set (make-local-variable 'company-backends) '(company-go))
              (company-mode))))

(defun my-c-mode-hook ()
  (setq c-basic-offset 4)
  (c-set-offset 'substatement-open 0))
(add-hook 'c-mode-common-hook 'my-c-mode-hook)

;; Scripting languages
(use-package rinari
  :hook ruby-mode
  :ensure t)
(use-package robe
  :hook (ruby-mode . robe-mode)
  :ensure t
  :init
  (add-hook 'ruby-mode-hook
            (lambda ()
              (setq-local company-backends '((company-robe))))))

(use-package python
  :defer t
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

(use-package less-css-mode
  :defer t
  :ensure t)

;;; Colourise CSS colour literals
(use-package rainbow-mode
  :if (eval-when-compile (>= emacs-major-version 24))
  :ensure t
  :hook ((css-mode html-mode sass-mode) . rainbow-mode))

;; markup formats
(use-package yaml-mode
  :ensure t
  :mode (("\\.yml$" . yaml-mode) ("\\.sls\\'" . yaml-mode)))
(use-package salt-mode
  :defer t
  :ensure t)
(use-package markdown-mode
  :defer t
  :ensure t
  :bind (:map markdown-mode-map ("M-RET" . nil)))
(use-package toml-mode
  :defer t
  :ensure t)
(use-package vue-mode
  :defer t
  :ensure t
  :config
  (setq js-indent-level 2))

(use-package hcl-mode
  :defer t
  :ensure t)
(use-package terraform-mode
  :defer t
  :ensure t)

(use-package ponylang-mode
  :defer t
  :ensure t
  :config
  (progn
    (add-hook
     'ponylang-mode-hook
     (lambda ()
       (set-variable 'indent-tabs-mode nil)
       (set-variable 'tab-width 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other functionality (org, calendar)
(use-package calfw
  :defer t
  :ensure t)

(use-package org
  :defer t
  :ensure t
  :config
  (progn
    (setq org-src-fontify-natively t
          org-fontify-whole-heading-line t
          org-pretty-entities t
          org-return-follows-link t
          org-src-tab-acts-natively t)
    (define-key org-mode-map (kbd "M-RET") nil)
    (require 'org-crypt)
    (org-crypt-use-before-save-magic)
    (setq org-tags-exclude-from-inheritance (quote ("crypt")))
    ;; GPG key to use for encryption
    ;; Either the Key ID or set to nil to use symmetric encryption.
    (setq org-crypt-key nil)
    (use-package org-bullets
      :ensure t
      :hook (org-mode . org-bullets-mode))
    ;; Avoid `org-babel-do-load-languages' since it does an eager require.
    (use-package ob-ruby
      :defer t
      :ensure org-plus-contrib
      :commands (org-babel-execute:ruby))
    (use-package ob-python
      :defer t
      :ensure org-plus-contrib
      :commands (org-babel-execute:python))
    (use-package ob-markdown
      :defer t
      :ensure org-plus-contrib
      :quelpa (ob-markdown :fetcher github :repo "tnoda/ob-markdown")
      :commands
      (org-babel-execute:markdown
       org-babel-expand-body:markdown))
    (use-package ob-go
      :defer t
      :ensure org-plus-contrib
      :commands
      (org-babel-execute:go
       org-babel-expand-body:go))
    (use-package ob-http
      :defer t
      :ensure org-plus-contrib
      :commands
      (org-babel-execute:http
       org-babel-expand-body:http))
    (use-package ob-shell
      :defer t
      :ensure org-plus-contrib
      :commands
      (org-babel-execute:sh
       org-babel-expand-body:sh
       org-babel-execute:bash
       org-babel-expand-body:bash))
    (use-package ob-diagrams
      :defer t
      :ensure org-plus-contrib
      :commands (org-babel-execute:diagrams))
    (use-package ox-gfm
      :defer t
      :ensure t)
    (use-package ox-reveal
      :defer t
      :ensure t)
    (use-package htmlize
      :defer t
      :ensure t)))

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

;; Use a hook so the message doesn't get clobbered by other messages.
;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (message "Emacs ready in %s with %d garbage collections."
;;                      (format "%.2f seconds"
;;                              (float-time
;;                               (time-subtract after-init-time before-init-time)))
;;                      gcs-done)))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
(provide 'init)
