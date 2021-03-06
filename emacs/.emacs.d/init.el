;;; init.el --- Duncan Mac-Vicar P. emacs init

;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))
(defconst emacs-start-time (current-time))

(setq straight-check-for-modifications 'live)
(setq straight-vc-git-default-clone-depth 1)
;; straight bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; do this early
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Diminished modes are minor modes with no modeline display
(use-package diminish)
(require 'bind-key)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance & Behavior
;;
;; I don't want emacs to overwrite my init.el with customization
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file :noerror))

;; don't annoy colleagues with beeps
(setq visible-bell t)
;; don't annoy me with dialog boxes
(setq use-dialog-box nil)

;; scratch mode settings
;; I want an empty text file, and not a lisp buffer with a comment
(setq initial-major-mode 'text-mode)
(setq initial-scratch-message "")

;; http://pragmaticemacs.com/emacs/dont-kill-buffer-kill-this-buffer-instead/
(defun dmacvicar/kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'dmacvicar/kill-this-buffer)

;; allow y or n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; disable function keys
(dotimes (i 12)
  (global-unset-key (kbd (format "<f%d>" (+ i 1)))))

;; disable overwrite mode
(define-key global-map [(insert)] nil)

;; so that vc mode do not ask again
(setq vc-follow-symlinks t)

;; recent files
(use-package recentf
  :straight (:type built-in)
  ;; Loads after 1 second of idle time.
  :bind ("C-x C-r" . recentf-open-files)
  :init
  :config
  (progn
    ;; Save the list every 5 minutes
    (recentf-mode 1) (run-at-time nil (* 5 60) 'recentf-save-list)))

(use-package f :defer t)

;; We only load perspeen when in graphical mode, as we lack tmux
(use-package perspeen
  :if window-system
  :init
  (perspeen-mode)
  (setq perspeen-use-tab nil)
  :config
  (perspeen-rename-ws "main")
  :bind
  ("C-x c" . perspeen-create-ws)
  ;; like tmux close window
  ("C-x &" . perspeen-delete-ws)
  ;; X230 has these keys next to arrows
  ("C-x <next>" . perspeen-next-ws)
  ("C-x <prior>" . perspeen-previous-ws)
  ;; X220 has these keys next to arrows, so for convenience
  ("C-x <XF86Back>" . perspeen-previous-ws)
  ("C-x <XF86Forward>" . perspeen-next-ws))
(bind-key* "M-S-<right>" #'perspeen-next-ws)
(bind-key* "M-S-<left>" #'perspeen-previous-ws)

(use-package popwin
  :config
  (push '("\\*Org QL View*" :regexp t) popwin:special-display-config)
  (push "*Org-QL-Agenda*" popwin:special-display-config)
  (push 'cfw:details-mode  popwin:special-display-config)
  (push 'calendar-mode  popwin:special-display-config)
  (push 'ag-mode  popwin:special-display-config)
  (popwin-mode 1))

(use-package all-the-icons
  :defer t)

(unless (display-graphic-p)
  ; xclip is used only on terminals
  (when (executable-find "xclip")
  (use-package xclip
    ;; Loads after 1 second of idle time.
    :defer 1
    :config (xclip-mode 1))))

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

(use-package highlight-indent-guides
  :diminish highlight-indent-guides-mode
  :config (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  :init
  (progn
    (setq highlight-indent-guides-method 'column)))

(use-package powerline
  :config
  (progn
    (powerline-default-theme)))

(use-package leuven-theme
  :custom
  (leuven-scale-outline-headlines nil)
  (leuven-scale-org-agenda-structure nil)
  :config
  (load-theme 'leuven t))

(setq show-trailing-whitespace t)
(setq-default show-trailing-whitespace t)

;; Window splitting functions
(use-package windmove
  ;; :defer 4
  :straight (:type built-in)
  :config
  ;; use command key on Mac
  ;;(::windmove-default-keybindings 'super)
  ;; wrap around at edges
  (setq windmove-wrap-around t)
  :bind
  (("C-x -" . (lambda ()
               (interactive) (split-window-vertically) (other-window 1) (switch-to-buffer "*scratch*")))
  ("C-x |" . (lambda ()
               (interactive) (split-window-horizontally) (other-window 1) (switch-to-buffer "*scratch*")))
  ("C-x x" . (lambda ()
               (interactive) (kill-buffer (current-buffer)) (if (one-window-p) () (delete-window))))))
(bind-key* "M-<left>" #'windmove-left)
(bind-key* "M-<right>" #'windmove-right)
(bind-key* "M-<up>" #'windmove-up)
(bind-key* "M-<down>" #'windmove-down)
(bind-key* "<XF86Back>" (lambda () (interactive) (other-window -1)))
(bind-key* "<XF86Forward>" (lambda () (interactive) (other-window 1)))


(bind-key "<XF86Back>" (lambda () (interactive) (other-window -1)))
(bind-key "<XF86Forward>" (lambda () (interactive) (other-window 1)))

;;;; scrolling
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common programming and tools
;;
(use-package multi-term
  :defer t)

(use-package ag
  :defer t
  :config
  (add-hook 'ag-mode-hook 'toggle-truncate-lines)
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers 't))

(use-package ivy
  :diminish ivy-mode
  :config
  (progn
    (ivy-mode 1)
    (bind-key "C-c C-r" 'ivy-resume)))

(use-package ivy-rich
  :after ivy
  :init
  (progn
    (setq ivy-virtual-abbreviate 'full)
    (setq ivy-rich-switch-buffer-align-virtual-buffer t)
    (setq ivy-rich-path-style 'abbrev)))

;; This is needed to order M-x by most recent commands
(use-package smex
  :defer t)

;; better C-s
(use-package swiper
  :defer t
  :bind ("C-s" . swiper))

(use-package counsel
  :after ivy
  :diminish
  :bind
  ("M-x" . counsel-M-x)
  ;;("C-z f" . counsel-describe-function)
  ;;("C-z v" . counsel-describe-variable)
  ("C-c k" . counsel-ag))

(use-package treemacs
  :defer t
  :config
  (treemacs-git-mode 'deferred))

(use-package projectile
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
      :preface (setq projectile-keymap-prefix (kbd "C-c p"))
      :hook (after-init . counsel-projectile-mode))))

(use-package treemacs-projectile
  :defer t
  :after projectile
  :after treemacs)

(use-package company
  :config (add-hook 'prog-mode-hook 'company-mode))

(defun duncan/ruby-bundler-project-root ()
  (let ((project-root (projectile-project-root)))
    (if
      (and
       (or (equal major-mode 'enh-ruby-mode) (equal major-mode 'ruby-mode))
       (file-exists-p (concat project-root "Gemfile"))
       (file-directory-p (concat project-root ".bundle")))
      project-root)))

(defun duncan/ruby-solargraph-project-p ()
  (let ((project-root (duncan/ruby-bundler-project-root)) (case-fold-search t))
    (with-temp-buffer
      (insert-file-contents (concat project-root "Gemfile.lock"))
      (goto-char (point-min))
      (ignore-errors (search-forward-regexp "solargraph")))))

(defun duncan/ruby-wrap-when-bundler-project (command)
  (if
    (duncan/ruby-bundler-project-root)
      (append '("bundle" "exec") command)
      command))

(use-package lsp-mode
  :commands lsp
  :defer t
  :init
  ; We only activate LSP mode for some languages
  (add-hook 'go-mode-hook #'lsp-deferred)
  (add-hook 'enh-ruby-mode-hook #'lsp-deferred)
  (add-hook 'c-mode-hook #'lsp-deferred)
  (add-hook 'c++-mode-hook #'lsp-deferred)
  (add-hook 'python-mode-hook #'lsp-deferred)
  (add-hook 'java-mode-hook #'lsp-deferred)
  (add-hook 'enh-ruby-mode-hook #'(lambda ()
                                    (if (duncan/ruby-solargraph-project-p)
                                        (let ((lsp-solargraph-use-bundler t)) (lsp-deferred)))))
  :custom
  (lsp-auto-guess-root t)
  (lsp-solargraph-diagnostics t)
  (lsp-enable-snippet nil)
  (lsp-prefer-flymake nil)
  (lsp-enable-xref nil)
  (lsp-eldoc-enable-hover nil)
  (lsp-eldoc-enable-signature-help nil)
  (lsp-eldoc-render-all nil)
  (lsp-eldoc-enable-signature-help nil)
  (lsp-eldoc-prefer-signature-help nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-completion-at-point nil)
  (lsp-solargraph-use-bundler t)
  :hook 'lsp-ui-mode)

(use-package lsp-ui
  :defer t
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-peek-enable t)
  (lsp-ui-sideline-enable t)
  (lsp-ui-imenu-enable t)
  (lsp-ui-flycheck-enable t)
  :after flycheck)

(use-package lsp-treemacs
  :defer t
  :after treemacs
  :after lsp-mode
  :config
  (lsp-treemacs-sync-mode 1))

(use-package company-lsp
  :commands company-lsp
  :defer t
  :config
  (push 'company-lsp company-backends))

(use-package magit
  :defer t)

(use-package treemacs-magit
  :defer t
  :after treemacs
  :after magit)

(use-package flycheck
  :defer t
  :after projectile
  :custom
  (flycheck-command-wrapper-function #'duncan/ruby-wrap-when-bundler-project)
  (flycheck-check-syntax-automatically '(idle-change save mode-enabled new-line))
  :config
  (global-flycheck-mode))

(use-package flycheck-pos-tip
  :after (flycheck)
  :config
  (with-eval-after-load 'flycheck (flycheck-pos-tip-mode)))

(use-package lsp-java
  :after lsp
  :config (add-hook 'java-mode-hook 'lsp)
  :custom
  lsp-file-watch-ignored '(".idea" ".ensime_cache" ".eunit" "node_modules"
                           ".git" ".hg" ".fslckout" "_FOSSIL_"
                           ".bzr" "_darcs" ".tox" ".svn" ".stack-work"
                           "build" "data"))

(use-package dap-mode
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t)
  (require 'dap-go)
  (dap-go-setup))

(use-package dap-java
  :defer t
  :after (lsp-java)
  :straight (:type built-in))

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

;; never use tabs
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

;;(global-set-key (kbd "M-RET") 'electric-buffer-list)
;;(global-set-key (kbd "C-M-j") 'electric-buffer-list)
(global-set-key (kbd "M-RET") 'ivy-switch-buffer)
(global-set-key (kbd "C-M-j") 'ivy-switch-buffer)

(add-hook 'Buffer-menu-mode-hook 'buffer-disable-undo)

;;(cua-mode 1)
(setq shift-select-mode nil)
(setq x-alt-keysym 'meta)

(use-package nav
  :config
  (nav-disable-overeager-window-splitting))

;; highlight the current line
(global-hl-line-mode t)

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

;;; writing

;;; FIXME why this one does not work?
;;;(use-package flycheck-vale
;;;  :defer t
;;;  :ensure t
;;;  :after (flycheck)
;;;  :hook markdown-mode
;;;  :hook org-mode
;;;  :config
;;;  (require 'flycheck-vale)
;;;  (flycheck-vale-setup))

(flycheck-define-checker vale
  "A checker for prose"
  :command ("vale" "--output" "line"
            source)
  :standard-input nil
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ":" (id (one-or-more (not (any ":")))) ":" (message) line-end))
  :modes (markdown-mode org-mode text-mode)
  )
; FIXME disable vale until finishing straight migration
;(add-to-list 'flycheck-checkers 'vale 'append)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; language specific tweaks
;;
;; C, C++ and other compiled languages

(use-package cmake-mode
  :defer t)

(use-package meson-mode
  :defer t)

(use-package rust-mode
  :defer t
  :config
  (progn
    (use-package flycheck-rust
      :hook rust-mode
      :after (flycheck))))

(use-package go-mode
  :defer t
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              (setq tab-width 4)
              (company-mode))))

(defun my-c-mode-hook ()
  (setq c-basic-offset 4)
  (c-set-offset 'substatement-open 0))
(add-hook 'c-mode-common-hook 'my-c-mode-hook)

;; Java family of languages
(use-package kotlin-mode
  :defer t
  :config
  (setq kotlin-tab-width 4))

(use-package clojure-mode
  :defer t)

(use-package cider
  :defer t)

(use-package elisp-mode
  :defer t
  :straight (:type built-in)
  :config
  (use-package flycheck-elsa
    :ensure t
    :after (flycheck elsa)
    :config
    (add-hook 'emacs-lisp-mode-hook #'flycheck-elsa-setup)))

;; Scripting languages
(use-package enh-ruby-mode
  :interpreter "ruby"
  :after projectile
  :defer t
  :mode ("\\.rake\\'" . enh-ruby-mode)
  :mode ("\\Rakefile\\'" . enh-ruby-mode)
  :mode ("\\.gemspec\\'" . enh-ruby-mode)
  :mode ("\\.ru\\'" . enh-ruby-mode)
  :mode ("Gemfile\\'" . enh-ruby-mode)
  :mode ("Guardfile\\'" . enh-ruby-mode)
  :mode ("Capfile\\'" . enh-ruby-mode)
  :mode ("\\.thor\\'" . enh-ruby-mode)
  :mode ("\\.rabl\\'" . enh-ruby-mode)
  :mode ("Thorfile\\'" . enh-ruby-mode)
  :mode ("Vagrantfile\\'" . enh-ruby-mode)
  :mode ("\\.jbuilder\\'" . enh-ruby-mode)
  :mode ("Podfile\\'" . enh-ruby-mode)
  :mode ("\\.podspec\\'" . enh-ruby-mode)
  :mode ("Puppetfile\\'" . enh-ruby-mode)
  :mode ("Berksfile\\'" . enh-ruby-mode)
  :mode ("Appraisals\\'" . enh-ruby-mode)
  :mode ("\\.rb\\'" . enh-ruby-mode)
  :config
  (setq ruby-deep-indent-paren nil)
  (add-hook 'ruby-mode-hook 'rbenv-use-corresponding))

(use-package rbenv
  :init
  (progn
    (setq
     rbenv-modeline-function 'rbenv--modeline-plain
     rbenv-show-active-ruby-in-modeline nil)
    (global-rbenv-mode)))

(use-package python
  :defer t
  :straight (:type built-in)
  :config
  (progn
    (setq-default python-indent 4)
    (use-package flycheck-pyflakes
      :defer t
      :after (flycheck))))

(use-package nim-mode
  :defer t)

(use-package zig-mode
  :defer t)

(use-package feature-mode
  :mode ("\\.feature$" . feature-mode))

;; web development
(use-package web-mode
  :mode (("\\.erb\\'" . web-mode) ("\\.html?\\'" . web-mode))
  :config
    (setq web-mode-enable-css-colorization t)
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-style-padding 2)
    (setq web-mode-script-padding 2))

(use-package less-css-mode
  :defer t)

;;; Colourise CSS colour literals
(use-package rainbow-mode
  :if (eval-when-compile (>= emacs-major-version 24))
  :hook ((css-mode html-mode sass-mode) . rainbow-mode))

;; markup formats
(use-package polymode
  :defer t)
(use-package yaml-mode
  :mode (("\\.yml$" . yaml-mode) ("\\.sls\\'" . yaml-mode)))
(use-package salt-mode
  :defer t)
(use-package markdown-mode
  :defer t
  :bind (:map markdown-mode-map ("M-RET" . nil)))
(use-package poly-markdown
  :defer t
  :hook (markdown-mode . poly-markdown-mode))
(use-package toml-mode
  :defer t)
(use-package vue-mode
  :defer t
  :config
  (setq js-indent-level 2))
(use-package poly-vue
  :defer t
  :straight (poly-vue :type git :host github :repo "akirak/poly-vue"))
(use-package js2-mode
  :defer t)
(use-package typescript-mode
  :defer t
  :config
  (setq js-indent-level 2))
(use-package tide
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package hcl-mode
  :defer t)
(use-package terraform-mode
  :defer t)

(use-package ponylang-mode
  :defer t
  :config
  (progn
    (add-hook
     'ponylang-mode-hook
     (lambda ()
       (set-variable 'indent-tabs-mode nil)
       (set-variable 'tab-width 2)))))

(use-package lua-mode
  :defer t)

(use-package ess
  :defer t)
(use-package ess-R-data-view
  :defer t)
(use-package ess-view
  :defer t)

(use-package smartparens
  :defer 3
  :init
  (progn
    (require 'smartparens-config)
    (require 'smartparens-html)
    (smartparens-global-mode 1)
    (show-smartparens-global-mode 1))
  :config
  (progn
    (setq smartparens-strict-mode 1)
    (setq sp-highlight-pair-overlay nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other functionality (org, calendar, browser)
(use-package eww
  :defer t
  :bind
  ("C-x m" . browse-url-at-point)
  :config
  ;; do not hijack the shortcut I use to switch buffers
  (unbind-key "M-RET" eww-mode-map)

(add-hook 'eww-mode-hook #'toggle-word-wrap)
  (add-hook 'eww-mode-hook #'(lambda () (setq show-trailing-whitespace nil))))

(use-package eww-lnum
    :config
    (bind-key "f" #'eww-lnum-follow eww-mode-map)
    (bind-key "U" #'eww-lnum-universal eww-mode-map))

(use-package link-hint
  :bind ("C-c f" . link-hint-open-link))

(use-package calfw
  :defer t)

(use-package org
  :defer t
;  :straight (:type built-in)
  :hook
  (org-babel-after-execute . org-redisplay-inline-images)
  (org-mode . visual-line-mode)
  :custom
  (org-latex-listings 'minted)
  (org-latex-pdf-process
   '("xelatex -shell-escape -interaction nonstopmode %f"
     "xelatex -shell-escape -interaction nonstopmode %f"))
  (org-todo-keyword-faces
   '(("RED" . (:foreground "red" :weight bold))
     ("YELLOW" . (:foreground "#EA6300" :background "#F6FECD" (:line-width 1 :color "#EA6300") :weight bold))
     ("GREEN" . (:foreground "#556b2f" :background "#20b2aa" (:line-width 1 :color "#556b2f") :weight bold))
     ("SHARE" . (:foreground "#0059b3" :background "#99ccff" (:line-width 1 :color "#0059b3") :weight bold))))
  (org-startup-indented t)
  (org-src-fontify-natively t)
  (org-fontify-whole-heading-line t)
  (org-pretty-entities t)
  (org-return-follows-link t)
  (org-src-tab-acts-natively t)
  (org-bullets-bullet-list '("⁖"))
  (org-crypt-key nil "symmetric encryption")
  (org-tags-exclude-from-inheritance (quote ("crypt")))
  ;; Does not work in org-ql yet :-(
  (org-agenda-category-icon-alist
   `(("emacs" ,(list (all-the-icons-fileicon "emacs")) nil nil :ascent center)))
  ;;(org-agenda-prefix-format "○ ")  
  :config
  ;; we have Alt-Enter map to ivy-switch-buffer
  (unbind-key "M-<return>" org-mode-map)
  (unbind-key "M-RET" org-mode-map)
  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
    ;;; (all-the-icons-insert-icons-for 'faicon) inserts all faicon icons to check
  (use-package org-super-agenda
    :defer t)
  (use-package org-ql
    :defer t)
  (use-package org-bullets
    :hook (org-mode . org-bullets-mode))
  ;; Avoid `org-babel-do-load-languages' since it does an eager require.
  (use-package ob-C
    :defer t
    :straight (:type built-in)
    :requires (org-plus-contrib)
    :commands (org-babel-execute:C org-babel-execute:C++))
  (use-package ob-ruby
    :defer t
    :straight (:type built-in)
    :requires (org-plus-contrib)
    :commands (org-babel-execute:ruby))
  (use-package ob-python
    :defer t
    :straight (:type built-in)
    :requires (org-plus-contrib)
    :commands (org-babel-execute:python))
  (use-package ob-octave
    :defer t
    :requires (org-plus-contrib)
    :straight (:type built-in)
    :commands (org-babel-execute:octave))
  (use-package ob-gnuplot
    :defer t
    :requires (org-plus-contrib)
    :straight (:type built-in)
    :commands (org-babel-execute:gnuplot))
  (use-package ob-markdown
    :defer t
    :requires (org-plus-contrib)
    :straight (ob-markdown :type git :host github :repo "tnoda/ob-markdown")
    :commands
    (org-babel-execute:markdown
     org-babel-expand-body:markdown))
  (use-package ob-go
    :defer t
    :straight (:type built-in)
    :requires (org-plus-contrib)
    :commands
    (org-babel-execute:go
     org-babel-expand-body:go))
  (use-package ob-http
    :defer t
    :straight (:type built-in)
    :requires (org-plus-contrib)
    :commands
    (org-babel-execute:http
     org-babel-expand-body:http))
  (use-package ob-shell
    :defer t
    :straight (:type built-in)
    :requires (org-plus-contrib)
    :commands
    (org-babel-execute:sh
     org-babel-expand-body:sh
     org-babel-execute:bash
     org-babel-expand-body:bash))
  (use-package ob-diagrams
    :defer t
    :straight (:type built-in)
    :requires (org-plus-contrib)
    :commands (org-babel-execute:diagrams))
  (use-package ob-ditaa
    :defer t
    :straight (:type built-in)
    :requires (org-plus-contrib)
    :custom
    (org-ditaa-jar-path "/usr/share/java/ditaa.jar")
    :commands (org-babel-execute:ditaa))
  (use-package ob-plantuml
    :defer t
    :straight (:type built-in)
    :requires (org-plus-contrib)
    :custom
    (org-plantuml-jar-path "/usr/share/java/plantuml.jar")
    :commands (org-babel-execute:plantuml))
  (use-package ox-gfm
    :defer t)
  (use-package ox-reveal
    :defer t)
;  (use-package ox-beamer
;    :defer t
;    ; bug with built-in packages
;    :ensure nil)
  (use-package htmlize
    :defer t))

(use-package poly-org
  :defer t
  :hook
  (org-mode . poly-org-mode))

;; kubernetes
(use-package kubernetes
  :commands (kubernetes-overview))
(use-package kubel)

;; Other tools and browsers
(use-package hackernews
  :defer t)

;; work setup. It conflicts with the home setup because of mu4e
;; so we load one or the other
(if (file-exists-p "~/.emacs.suse.d/init.el")
    (load-file "~/.emacs.suse.d/init.el")
  (if (file-exists-p "~/.emacs.home.d/init.el")
      (load-file "~/.emacs.home.d/init.el")))

;; Hacks

;; tramp mode
(use-package tramp
  :defer t
  :config
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))
  ; turn off the backup feature for remote files and stop TRAMP from saving to the backup directory
  (add-to-list 'backup-directory-alist
               (cons tramp-file-name-regexp nil)))



;; https://stackoverflow.com/a/16779511/203718
;; Used to create a completely new shell in the current buffer
;; like I do in tmux
(use-package xterm-color
  :defer t
  :init
  (setq comint-output-filter-functions
      (remove 'ansi-color-process-output comint-output-filter-functions))
  :hook (shell-mode . (lambda ()
                        ;; Disable font-locking in this buffer to improve performance
                        (font-lock-mode -1)
                        ;; Prevent font-locking from being re-enabled in this buffer
                        (make-local-variable 'font-lock-function)
                        (setq font-lock-function (lambda (_) nil))
                        (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t))))
(use-package shx
  :defer t
  :hook (shell-mode . shx-mode))

(defun new-shell (name)
  (interactive)
  (let ((currentbuf (get-buffer-window (current-buffer)))
        (newbuf     (generate-new-buffer-name name)))
    (generate-new-buffer newbuf)
    (set-window-dedicated-p currentbuf nil)
    (set-window-buffer currentbuf newbuf)
    (shell newbuf)))

(defun shell-with-name ()
  (interactive)
  (let* ((shell-buffers (seq-filter (lambda (buf)
                                      (string-match-p "*shell*" (buffer-name buf)))
                                    (buffer-list)))
         (shell-buffer-names (mapcar 'buffer-name shell-buffers)))
    (ivy-read "shell buffers: "
              shell-buffer-names
              :action (lambda (buffer-name)
                        (let* ((shell-buffer-exists (member buffer-name
                                                            (mapcar 'buffer-name 
                                                                    (buffer-list)))))
                          (if shell-buffer-exists
                              (switch-to-buffer buffer-name)
                            (new-shell (concat "*shell*<" buffer-name ">"))))))))
(global-set-key (kbd "C-c s") 'shell-with-name)

(when (getenv "EMACS_PROFILE_START")
  (add-hook 'emacs-startup-hook
            (lambda ()
              (message "Emacs ready in %s with %d garbage collections."
                       (format "%.2f seconds"
                               (float-time
                                (time-subtract after-init-time before-init-time)))
                       gcs-done))))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
(provide 'init)
;;; init.el ends here
