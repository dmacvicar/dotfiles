;; -*- coding: utf-8; lexical-binding: t -*-

;; make startup faster by avoiding gc pauses
(setq gc-cons-threshold (* 50 1000 1000))
(defconst emacs-start-time (current-time))

;; disable UI elements, do this early
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(prefer-coding-system 'utf-8)

(require 'bind-key)

(setq custom-file
      (convert-standard-filename
       (expand-file-name  "emacs/custom.el" (xdg-cache-home))))
(when (file-exists-p custom-file)
    (load custom-file :noerror))

;; nicer default frame size depending of screen
(defun duncan/set-frame-size-according-to-resolution ()
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

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))
(add-hook 'text-mode-hook (lambda () (setq show-trailing-whitespace t)))

(column-number-mode)
;; if you type with text selected, delete it
(delete-selection-mode 1)
(pixel-scroll-mode)


;; misc defaults
(setq inhibit-startup-screen t
      initial-scratch-message nil
      initial-buffer-choice (if (< (length command-line-args) 2) #'enlight nil)
      initial-major-mode 'text-mode
      sentence-end-double-space nil
      ring-bell-function 'ignore
      visible-bell nil
      save-interprogram-paste-before-kill t
      use-dialog-box nil
      mark-even-if-inactive nil
      kill-whole-line t
      compilation-read-command nil
      compilation-scroll-output 'first-error
      use-short-answers t
      fast-but-imprecise-scrolling t
      load-prefer-newer t
      confirm-kill-processes nil
      native-comp-async-report-warnings-errors 'silent
      truncate-string-ellipsis "…"
      confirm-kill-emacs nil
      global-auto-revert-mode t
      auto-revert-use-notify t
      global-auto-revert-non-file-buffers t
      auto-revert-verbose nil
      delete-by-moving-to-trash t
      fill-column 80
      make-backup-files nil
      auto-save-default nil
      create-lockfiles nil
      dired-listing-switches "-lt"
      frame-title-format '(""
			   invocation-name
			   " - "
			   (:eval
			    (if (buffer-file-name)
				(abbreviate-file-name (buffer-file-name))
			    "%b"))))
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8-unix)

(defvar elpaca-installer-version 0.9)
(defvar elpaca-directory (expand-file-name "emacs/elpaca/" (xdg-cache-home)))
;;(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  (elpaca-use-package-mode)                 ; enable :elpaca use-package keyword
  (setq elpaca-use-package-by-default t))   ; assume :elpaca t unless otherwise specified

; block until current queue is processed - this will allow use of use-package right away
(elpaca-wait)

;; use gnome secret service to store passwords
;; TODO add keepass
(use-package auth-source
  :ensure nil
  :defer t
  :custom
  (auth-sources '("secrets:login"))
  (auth-source-cache-expiry nil)
  (auth-source-debug 'trivia))

(use-package expand-region
  :defer t
  :bind ("C-=" . er/expand-region))

;; copy from clipboard in terminal
(use-package xclip
  :custom
  (xclip-method (if (getenv "WAYLAND_DISPLAY") 'wl-copy 'xclip))
  :init
  (xclip-mode))

;; recent files
(use-package recentf
  :ensure nil
  :defer t
  :config
  (recentf-mode)
  :custom
  (recentf-save-file
   (convert-standard-filename
       (expand-file-name  "emacs/recentf" (xdg-state-home))))
  (recentf-max-menu-items 25)
  (recentf-max-saved-items 25)
  (recentf-exclude '("/autosave$"
		     "/treemacs-persist$"
                     ".cache")))

;; minibuffer history
(use-package savehist
  :ensure nil
  :custom
  (savehist-file (convert-standard-filename
       (expand-file-name  "emacs/history" (xdg-state-home))))
  :init
  (savehist-mode))

;; modeline
(use-package doom-modeline
  :config (doom-modeline-mode 1))

;; dashboard screen

(use-package grid
  :ensure (:host github :repo "ichernyshovvv/grid.el"))

(use-package enlight
  :ensure (:host github :repo "ichernyshovvv/enlight")
  :after grid
  :config
  (require 'grid)
  :custom
  (enlight-content
    (concat
      (propertize "Emacs Dashboard" 'face 'highlight )
      "\n\n\n"
      (enlight-menu
        '(("Files/projects"
           ("Projects" project-switch-project "p")
           ("Recent" recentf-open "r"))
          ("Calendar/TODO"
            ("Agenda" (org-agenda-list "1") "a")
            ("todo.org"
              (find-file (seq-find (lambda (x) (string-match "todo.org" x)) org-agenda-files)) "t"))
          ("Misc"
           ("Emacs Configuration" (find-file user-init-file) "c")
           ("Home folder" (dired "~/") "h")))))))

;; popups
(use-package popper
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*Warnings\\*"
          "\\*grep\\*"
          "\\*rg*\\*"
          "\\*Org Agenda\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode
	  go-test-mode
	  xref-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

;; discoverability
(use-package which-key
  :config
  (which-key-mode)
  :custom
  (which-key-popup-type 'minibuffer))

;; completion system (alternative to ivy)
(use-package vertico
  :init
  (vertico-mode)
  :ensure (vertico
             :files (:defaults "extensions/vertico-directory.el")
             :includes (vertico-directory)))

;; configure directory extension
(use-package vertico-directory
  :ensure nil
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; reimpl of common emacs command using completion-system/vertico
;; alternative to consul
(use-package consult
  :demand t
  :init
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-preview-key '("S-<down>" "S-<up>"))
  :bind
  ("C-s" . consult-line)
  ("C-x b" . consult-buffer)
  :config)

(bind-keys*
 ("M-RET" . consult-buffer)
 ("C-M-j" . consult-buffer))

(use-package dashboard-elfeed
  :defer t
  :disabled
  :ensure (:host nil :type git :repo "git@github.com:dmacvicar/emacs-dashboard-elfeed"))

(use-package dashboard
  :defer t
  :custom
  (dashboard-set-footer nil)
  (dashboard-banner-logo-title "My Dashboard")
  (dashboard-startup-banner 'logo)
  (dashboard-projects-backend 'project-el)
  (dashboard-set-init-info t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-items '((agenda . 5)
                     (recents . 5)
                     (bookmarks . 5)
                     (projects . 5)))
;                     (elfeed . 6)))
  (dashboard-center-content t)
  (dashboard-week-agenda t))
;(add-hook 'elpaca-after-init-hook #'dashboard-open)

(use-package perspective
  :after consult
  :config
  (consult-customize consult--source-buffer :hidden t :default nil)
  (add-to-list 'consult-buffer-sources persp-consult-source)
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))  ; pick your own prefix key here
  (persp-suppress-no-prefix-key-warning t)
  :init
  (persp-mode))

;; complete in any order
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; add context to completions. eg. help to M-x functions
(use-package marginalia
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package nerd-icons
  :if (display-graphic-p))

(use-package nerd-icons-dired
  :after nerd-icons
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package transient
  :custom
  (transient-history-file (convert-standard-filename
                           (expand-file-name  "emacs/transient/history.el" (xdg-cache-home)))))

;; emojis
;; todo, set custom (emojify-display-style) when
;; we get a good font
(use-package emojify
  :custom
  (emojify-emojis-dir (convert-standard-filename
                       (expand-file-name  "emacs/emojis" (xdg-cache-home))))
  :hook (after-init . global-emojify-mode))

(use-package shell-pop
  :defer t
  :custom
  (shell-pop-universal-key "C-t"))

;; terminal emulator (replaces vterm, in pure-lisp)
;; provides eat-project to start a shell in current project
;; if TERM is not functional, install tic (terminfo compiler) from
;; ncurses devel and run eat-compile-termifo
(use-package eat
  :defer t
  :ensure (:host nil :type git
           :repo "https://codeberg.org/akib/emacs-eat"
           :files ("*.el" "dir"
                   ("integration" "integration/*")
                   "*.info" "*.texi"
                   "*.ti" ("e" "e/*"))))
;; theme
(use-package modus-themes
  :custom
  (modus-themes-mixed-fonts t)
  (modus-themes-org-blocks 'gray-background)
  (modus-themes-fringes 'intense))

(elpaca-wait)

(load-theme 'modus-operandi-tinted :no-confirm)
(define-key global-map (kbd "<f5>") #'modus-themes-toggle)

(set-face-attribute 'default nil :family "Fira Code" :height 130)
(set-face-attribute 'variable-pitch nil :family "Noto Sans")
(set-face-attribute 'fixed-pitch nil :family (face-attribute 'default :family) :height 110)

(use-package visual-fill-column
  :defer t
  :custom
  (visual-fill-column-adjust-for-text-scale t)
  (visual-fill-column-width 100)
  (visual-fill-column-center-text t)
  (visual-fill-column-fringes-outside-margins nil))
(advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)

(use-package mixed-pitch
  :defer t
  :custom
  (mixed-pitch-variable-pitch-cursor '(bar . 3)))

(duncan/set-frame-size-according-to-resolution)
;; not decided yet if maximizing is better?
;;(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; window splitting functions
(use-package windmove
  :ensure nil
  :config
  ;; do not set ever windmove-wrap-around as it prevents jumping to tmux panes
  :bind
  (("C-x -" . (lambda ()
               (interactive) (split-window-vertically) (other-window 1) (switch-to-buffer "*scratch*")))
  ("C-x |" . (lambda ()
               (interactive) (split-window-horizontally) (other-window 1) (switch-to-buffer "*scratch*")))
  ("C-x x" . (lambda ()
               (interactive) (kill-buffer (current-buffer)) (if (one-window-p) () (delete-window))))))

(use-package tmux-pane
  :if (not (display-graphic-p))
  :config
  (setq -override-map-enable nil))
;; define M-arrows using escape codes so that they
;; work in terminal
(define-key input-decode-map "\e\eOA" [(meta up)])
(define-key input-decode-map "\e\eOB" [(meta down)])
(define-key input-decode-map "\e\eOC" [(meta right)])
(define-key input-decode-map "\e\eOD" [(meta left)])

(elpaca-wait)

(if (display-graphic-p)
    (progn
      ;; if graphic mode, use windmove
      (windmove-mode)
      (bind-key* "M-<left>" #'windmove-left)
      (bind-key* "M-<right>" #'windmove-right)
      (bind-key* "M-<up>" #'windmove-up)
      (bind-key* "M-<down>" #'windmove-down)
      (bind-key* "<XF86Back>" (lambda () (interactive) (other-window -1)))
      (bind-key* "<XF86Forward>" (lambda () (interactive) (other-window 1)))
      (bind-key "<XF86Back>" (lambda () (interactive) (other-window -1)))
      (bind-key "<XF86Forward>" (lambda () (interactive) (other-window 1))))
      ;; else, move across tmux panes too
      (tmux-pane-mode)
      (bind-key* "M-<left>" #'tmux-pane-omni-window-left)
      (bind-key* "M-<right>" #'tmux-pane-omni-window-right)
      (bind-key* "M-<up>" #'tmux-pane-omni-window-up)
      (bind-key* "M-<down>" #'tmux-pane-omni-window-down))

;; respect style of projects
(use-package editorconfig
  :defer t
  :config
  (editorconfig-mode 1))

;; parenthesis
(use-package paren
  :defer t
  :ensure nil
  :hook ((prog-mode . show-paren-mode))
  :custom (show-paren-style 'expression))

(use-package rainbow-delimiters
  :defer t
  :hook ((prog-mode . rainbow-delimiters-mode)))

;; highlight undoed text
(use-package undo-hl
  :defer t
  :ensure (:host github :repo "casouri/undo-hl"))
(add-hook 'prog-mode-hook #'undo-hl-mode)
(add-hook 'text-mode #'undo-hl-mode)

;; text completion
(use-package company
  :defer t
  :config (add-hook 'prog-mode-hook 'company-mode))

;; github copilot
(use-package copilot
  :defer t
  :ensure (:host github :repo "copilot-emacs/copilot.el"
                 :files ("dist" "*.el"))
  :bind (:map copilot-completion-map
              ("<tab>" . copilot-accept-completion)
              ("TAB" . copilot-accept-completion)))

;; different gpt models
(use-package gptel
  :defer t)

(with-eval-after-load 'company
  ;; disable inline previews
  (delq 'company-preview-if-just-one-frontend company-frontends))

;; remote file access
(use-package tramp
  :defer t
  :ensure nil
  :config
  (setq vc-ignore-dir-regexp
	(format "\\(%s\\)\\|\\(%s\\)"
		vc-ignore-dir-regexp
		tramp-file-name-regexp))
  ;; turn off the backup feature for remote files and stop TRAMP from saving to the backup directory
  (add-to-list 'backup-directory-alist
	       (cons tramp-file-name-regexp nil)))

;; helper to find bundler project root
(defun duncan/ruby-bundler-project-root ()
  (let* ((current (project-current))
	 (root (if current (project-root current))))
    (if root
	(if (and
	     (or (equal major-mode 'enh-ruby-mode) (equal major-mode 'ruby-mode))
	     (file-exists-p (expand-file-name "Gemfile" root))
	     (file-directory-p (expand-file-name ".bundle" root)))
	    root))))

(defun duncan/ruby-solargraph-project-p ()
  (let ((root (duncan/ruby-bundler-project-root))
	(case-fold-search t))
    (if root
	(with-temp-buffer
	  (insert-file-contents (expand-file-name "Gemfile.lock" root))
	  (goto-char (point-min))
	  (ignore-errors (search-forward-regexp "solargraph"))))))

(defun duncan/ruby-wrap-when-bundler-project (command)
  (if (duncan/ruby-bundler-project-root)
      (append '("bundle" "exec") command)
    command))

;; LSP
(use-package lsp-mode
  :defer t
  :commands lsp
  :init
  (add-hook 'go-ts-mode-hook #'lsp-deferred)
  (add-hook 'enh-ruby-mode-hook #'lsp-deferred)
  (add-hook 'c-ts-mode-hook #'lsp-deferred)
  (add-hook 'c++-ts-mode-hook #'lsp-deferred)
  (add-hook 'python-ts-mode-hook #'lsp-deferred)
  (add-hook 'java-ts-mode-hook #'lsp-deferred)
  (add-hook 'ruby-ts-mode-hook #'(lambda ()
				(if (duncan/ruby-solargraph-project-p)
				    (let ((lsp-solargraph-use-bundler t)) (lsp-deferred)))))
  :custom
  ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
  (lsp-server-install-dir (convert-standard-filename
                           (expand-file-name  "emacs/lsp" (xdg-cache-home))))
  (lsp-auto-guess-root t)
  (lsp-solargraph-use-bundler t)
  (lsp-file-watch-threshold 2000)
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  :hook 'lsp-ui-mode
  :hook (lsp-mode . (lambda ()
                      (let ((lsp-keymap-prefix "C-c l"))
                        (lsp-enable-which-key-integration)))))

(use-package lsp-ui
  :defer t
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-peek-enable t)
  (lsp-ui-sideline-enable t)
  (lsp-ui-imenu-enable t)
  (lsp-ui-flycheck-enable t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-show-diagnostics t)
  :after flycheck)

(use-package company-lsp
  :defer t
  :after company
  :ensure nil
  :commands company-lsp
  :config
  (add-to-list 'company-lsp company-backends))

(use-package flycheck
  :defer t
  :after projectile
  :custom
  (flycheck-command-wrapper-function #'duncan/ruby-wrap-when-bundler-project)
  (flycheck-check-syntax-automatically '(idle-change save mode-enabled new-line))
  :config
  (global-flycheck-mode))

(use-package flycheck-pos-tip
  :defer t
  :after (flycheck)
  :config
  (with-eval-after-load 'flycheck (flycheck-pos-tip-mode)))

(use-package lsp-java
  :defer t
  :after lsp
  :config (add-hook 'java-mode-hook 'lsp)
  :custom
  lsp-file-watch-ignored '(".idea" ".ensime_cache" ".eunit" "node_modules"
			   ".git" ".hg" ".fslckout" "_FOSSIL_"
			   ".bzr" "_darcs" ".tox" ".svn" ".stack-work"
			   "build" "data"))

;; meson build system
(use-package meson-mode
  :defer t)

;; git
(use-package magit
  :defer t)

;; never lose your cursor
(use-package beacon
  :defer 5
  :config
  (setq beacon-push-mark 5)
  (setq beacon-size 25))

 ; `M-x combobulate' (or `C-c o o') to start using Combobulate
(use-package treesit
  :init
  (setq =treesit-grammar-cache-directory (convert-standard-filename
                                          (expand-file-name  "emacs/tree-sitter" (xdg-cache-home))))
  (when (boundp 'treesit-extra-load-path)
    (add-to-list 'treesit-extra-load-path =treesit-grammar-cache-directory))
  :config
  (advice-add #'treesit--install-language-grammar-1 :around
	          (lambda (fn out-dir &rest args)
	            (apply fn (or out-dir =treesit-grammar-cache-directory) args)))

  (dolist (mode
           '((bash-mode       . bash-ts-mode)
             (c-mode          . c-ts-mode)
             (c++-mode        . c++-ts-mode)
             (cmake-mode        . cmake-ts-mode)
             (python-mode     . python-ts-mode)
             (css-mode        . css-ts-mode)
             (dockerfile-mode . dockerfile-ts-mode)
             (go-mode         . go-ts-mode)
             (javascript-mode . js-ts-mode)
             (js-json-mode    . json-ts-mode)
             (typescript-mode . typescript-ts-mode)))
    (add-to-list 'major-mode-remap-alist mode))
  :ensure nil)

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  ; those are broken
  (treesit-auto-opt-out-list
        '(markdown protobuf ruby r yaml))
  :config
  (global-treesit-auto-mode))

(use-package combobulate
  :defer t
  :after treesit
  :ensure (:host github :repo "mickeynp/combobulate")
  :hook ((python-ts-mode . combobulate-mode)
         (js-ts-mode . combobulate-mode)
         (css-ts-mode . combobulate-mode)
         (yaml-ts-mode . combobulate-mode)
         (typescript-ts-mode . combobulate-mode)
         (tsx-ts-mode . combobulate-mode)))

;; replace built-in pdf viewer with something we
;; can copy paste, based on poppler
(use-package pdf-tools
  :defer t)

;; markdown
(use-package markdown-mode
  :defer t
  :hook
  (markdown-mode . visual-line-mode)
  (markdown-mode . visual-fill-column-mode)
  (markdown-mode . mixed-pitch-mode)
  :mode ("\\.md\\'" . gfm-mode))

;; use lang modes inside org src blocks
(use-package poly-org
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.org\\'" . poly-org-mode)))
;; use lang modes inside markdow code fences
(use-package poly-markdown
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.md\\'" . poly-gfm-mode)))
(use-package web-mode :mode "\\.qtpl\\'"
  :defer t)
(use-package vue-html-mode
  :defer t)
;; vue single file component mode (uses polymode)
(use-package sfc-mode
  :defer t
  :ensure (:host github :repo "gexplorer/sfc-mode")
  :custom
  (sfc-template-default-mode 'vue-html-mode)
  :mode "\\.vue\\'")

(use-package json-mode
  :defer t)
(use-package yaml-mode
  :defer t)

(use-package dockerfile-ts-mode
  :ensure nil
  :mode
  "Dockerfile\\'"
  "\\.dockerfile\\'"
  :defer t)
(use-package docker-compose-mode
  :ensure t
  :defer t)

;; guess indentation params
(use-package dtrt-indent
  :custom
  (dtrt-indent-max-lines 2000)
  (dtrt-indent-verbosity 2)
  :diminish
  :hook (prog-mode . dtrt-indent-mode))
(setq-default tab-width 8)

;; C
(setq-default c-default-style "linux")

(use-package cmake-ts-mode
  :ensure nil
  :mode
  "CMakeLists\\.txt\\'"
  :defer t)

;; integrate compile command for project with cmake
(use-package cmake-project
  :ensure t
  :defer t
  :custom
  (cmake-project-default-build-dir-name "build"))
(defun maybe-cmake-project-hook ()
  (if (file-exists-p "CMakeLists.txt") (cmake-project-mode)))
(add-hook 'c-ts-mode-hook 'maybe-cmake-project-hook)
(add-hook 'c++-ts-mode-hook 'maybe-cmake-project-hook)

;; go
(use-package go-ts-mode
  :ensure nil
  :defer t
  :custom
  (go-test-verbose t)
  (gofmt-args '("-s")))

;; like play.golang.org
(use-package go-playground
  :defer t
  :bind (:map go-playground-mode-map
              ([M-return] . nil)
              ("C-c C-c" . go-playground-exec)))
;; generates tests from funcs
(use-package go-gen-test
  :defer t)

(use-package rust-mode
  :defer t)
(use-package elixir-mode
  :defer t)
(use-package zig-mode
  :defer t)
(use-package nix-mode
  :defer t)
(use-package lua-mode
  :defer t)

;; browse HN
(use-package hackernews
  :defer t
  :custom
  (hackernews-visited-links-file (convert-standard-filename
                                  (expand-file-name  "emacs/hackernews/visited-links.el" (xdg-cache-home)))))

;; https://d2lang.com/tour/intro/
(use-package d2-mode
  :defer t)

(custom-set-variables '(ad-redefinition-action (quote accept)))

(use-package calendar
  :defer t
  :ensure nil
  :init
  (setq calendar-view-diary-initially-flag t)
  (setq calendar-date-style 'european))

(use-package diary
  :defer t
  :ensure nil
  :after calendar
  :init
  (advice-add 'diary :before 'duncan/generate-diary-from-calendars)
  (setq diary-file (expand-file-name "emacs/diary/calendars-diary" (xdg-cache-home)))
  (setq diary-number-of-entries 7)
  (setq diary-display-function #'diary-fancy-display)
  (add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
  (add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files)
  (add-hook 'diary-list-entries-hook 'diary-sort-entries))

;; org mode
(use-package org
  :ensure nil
  :defer t
  :hook
  ;; handwriten style
  (org-mode . (lambda() (buffer-face-set '(:family "Purisa"))))
  (org-babel-after-execute . org-redisplay-inline-images)
  (org-mode . visual-line-mode)
  (org-mode . visual-fill-column-mode)
  (org-mode . mixed-pitch-mode)
  (org-mode . buffer-face-mode)
  :custom
  (org-hide-emphasis-markers t)
  (org-log-repeat nil)
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
  (org-startup-with-inline-images t)
  (org-display-remote-inline-images 'cache)
  (org-image-actual-width 300)
  (org-src-fontify-natively t)
  (org-edit-src-content-indentation 0)
  (org-src-tab-acts-natively t)
  (org-fontify-whole-heading-line t)
  (org-pretty-entities t)
  (org-return-follows-link t)
  (org-crypt-key nil "symmetric encryption")
  (org-tags-exclude-from-inheritance (quote ("crypt")))
  ;; Does not work in org-ql yet :-(
  (org-agenda-category-icon-alist
   `(("emacs" ,(list (nerd-icons-sucicon "nf-custom-emacs")) nil nil :ascent center)))
  ;;(org-agenda-prefix-format "○ ")
  :config
  (require 'org-crypt)
  (require 'org-yt)
  (org-crypt-use-before-save-magic)
  ;; this allows to retrieve http images and show them inline (with
  ;; helo from org-yt)
  (defun org-http-image-data-fn (protocol link _description)
    "Interpret LINK as an URL to an image file."
    (when (and (image-type-from-file-name link)
               (not (eq org-display-remote-inline-images 'skip)))
      (if-let (buf (url-retrieve-synchronously (concat protocol ":" link)))
          (with-current-buffer buf
            (goto-char (point-min))
            (re-search-forward "\r?\n\r?\n" nil t)
            (buffer-substring-no-properties (point) (point-max)))
        (message "Download of image \"%s\" failed" link)
        nil)))
  (org-link-set-parameters "http"  :image-data-fun #'org-http-image-data-fn)
  (org-link-set-parameters "https" :image-data-fun #'org-http-image-data-fn)
  (set-face-attribute 'org-headline-done nil :strike-through t))

(use-package org-agenda
  :ensure nil
  :after org
  :init
  ;; do not ask to reload the calendar when regenerated
  (add-to-list 'revert-without-query "\\.cache/.+\\.ics")
  :config
  (org-agenda-dim-blocked-tasks t)
  :init
  (advice-add 'org-agenda :before 'duncan/generate-diary-from-calendars)
  (advice-add 'org-agenda-exit :after
            (lambda (&rest _)
              (message "closing all diaries opened by agenda")
              (dolist (buffer (buffer-list))
                (with-current-buffer buffer
                  ; stupid org-agenda opens a generated diary as DCL mode
                  (when (or (eq major-mode 'diary-mode) (eq major-mode 'dcl-mode))
                    (kill-buffer buffer))))))
  :custom
  (org-agenda-include-diary t))

(use-package org-timeblock
  :defer t
  :ensure (:host github :repo "ichernyshovvv/org-timeblock"))

;; this takes care of being able to display inline images in the buffer
(use-package org-yt
  :after org
  :defer t
  ;;:ensure (org-yt :host github :repo "TobiasZawada/org-yt"))
  ;; https://github.com/TobiasZawada/org-yt/pull/1
  ;; don't require imagemagick for resizing
  :ensure (org-yt :host github :repo "league/org-yt"))

(use-package org-super-agenda
  :hook (org-agenda-mode . org-super-agenda-mode))
;(use-package org-ql
;  :after org
;  :defer t)
(use-package org-superstar              ; supersedes `org-bullets'
  :after org
  :defer t
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '(" ")) ;; '("🞛" "◉" "○" "▷")
  (org-superstar-leading-bullet ?\s)
  (org-superstar-leading-fallback ?\s)
  (org-hide-leading-stars nil)
  (org-superstar-prettify-item-bullets t)
  :hook (org-mode . org-superstar-mode))

(use-package org-fancy-priorities ; priority icons
  :defer t
  :after org
  :hook (org-mode . org-fancy-priorities-mode)
  :hook (org-agenda-mode . org-fancy-priorities-mode)
  :custom
  (org-fancy-priorities-list '("⚑" "⬆" "■")))

(use-package org-modern
  :defer t
  :hook (org-mode . org-modern-mode))

(use-package org-appear
  :after org
  :defer t
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autoemphasis  t)
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t))

;; Avoid `org-babel-do-load-languages' since it does an eager require.
(use-package ob-C
  :ensure nil
  :requires (org-plus-contrib)
  :commands (org-babel-execute:C org-babel-execute:C++))
(use-package ob-ruby
  :ensure nil
  :requires (org-plus-contrib)
  :commands (org-babel-execute:ruby))
(use-package ob-python
  :ensure nil
  :requires (org-plus-contrib)
  :commands (org-babel-execute:python))
(use-package ob-octave
  :ensure nil
  :requires (org-plus-contrib)
  :commands (org-babel-execute:octave))
(use-package ob-gnuplot
  :ensure nil
  :requires (org-plus-contrib)
  :commands (org-babel-execute:gnuplot))
(use-package ob-markdown
  :ensure nil
  :requires (org-plus-contrib)
  :commands
    (org-babel-execute:markdown
     org-babel-expand-body:markdown))
(use-package ob-http
  :commands
  (org-babel-execute:http
   org-babel-expand-body:http))
(use-package ob-grpc
  :ensure (ob-grpc :type git :host github :repo "shsms/ob-grpc")
  :commands
  (org-babel-execute:grpc
   org-babel-expand-body:grpc)
  :bind (:map org-mode-map
              ("C-c g i" . ob-grpc-init)
              ("C-c g b" . ob-grpc-insert-block)))
(use-package ob-shell
  :ensure nil
  :requires (org-plus-contrib)
  :commands
  (org-babel-execute:sh
   org-babel-expand-body:sh
   org-babel-execute:bash
   org-babel-expand-body:bash))
(use-package ob-sql
  :ensure nil
  :commands (org-babel-execute:sql))
(use-package ob-diagrams
  :requires (org-plus-contrib)
  :commands (org-babel-execute:diagrams))
(use-package ob-ditaa
  :ensure nil
  :requires (org-plus-contrib)
  :custom
  (org-ditaa-jar-path "/usr/share/java/ditaa.jar")
  :commands (org-babel-execute:ditaa))
(use-package ob-plantuml
  :ensure nil
  :requires (org-plus-contrib)
  :custom
  (org-plantuml-jar-path "/usr/share/java/plantuml.jar")
  :commands (org-babel-execute:plantuml))
(use-package ob-d2
  :ensure (:host github :repo "dmacvicar/ob-d2")
  :requires (org-plus-contrib)
  :commands (org-babel-execute:d2))
(use-package ox-gfm
  :defer t)
(use-package ox-reveal
  :defer t)
(use-package org-tree-slide
  :defer t)
(use-package hide-mode-line
  :defer t)
(use-package org-present
  :defer t
  :init
  (add-hook 'org-present-mode-hook #'(lambda ()
                                       (setq-local face-remap-cookies
                                                   (mapcar (lambda (remapping)
                                                             (face-remap-add-relative (car remapping) (cdr remapping)))
                                                           '((default :height 2.5)
                                                             (header-line :height 4.0)
                                                             (org-document-title :height 2.75)
                                                             (org-code :height 2.55)
                                                             (org-verbatim :height 2.55)
                                                             (org-block :height 3.25)
                                                             (org-block-begin-line :height 0.7))))

                                       (let ((font-family
                                              (car (pcase (org-collect-keywords '("PRESENT_FONT"))
                                                     (`(("PRESENT_FONT" . ,val)) val)))))
                                         (when font-family
                                           (message "font: %s" font-family)
                                           (setq-local face-remap-cookies
                                                       (append face-remap-cookies
                                                               (mapcar (lambda (face)
                                                                         (face-remap-add-relative face :family font-family))
                                                                         '(default org-document-title org-code org-verbatim org-block org-block-begin-line))))))
                                       (setq-local org-image-actual-width (display-pixel-width))
                                       (org-present-big)
                                       (hide-mode-line-mode t)
                                       (display-line-numbers-mode -1)
                                       (org-display-inline-images)
                                       (toggle-frame-fullscreen)))
  (add-hook 'org-present-mode-quit-hook #'(lambda ()
                                       (dolist (cookie face-remap-cookies)
                                         (face-remap-remove-relative cookie))
                                       (org-present-small)
                                       (hide-mode-line-mode -1)
                                       (display-line-numbers-mode t)
                                       (org-remove-inline-images)
                                       (toggle-frame-fullscreen))))
;; I also tried jupyter packagebut did not work
(use-package ein
  :defer t
  :custom
  (ein:output-area-inlined-images t))

(use-package denote
  :defer t
  :bind ("C-c d" . denote)
  :config
  (with-eval-after-load 'org
    (require 'denote)))

(use-package consult-notes
  :defer t
  :commands (consult-notes
             consult-notes-search-in-all-notes))
(use-package htmlize :defer t)
(use-package protobuf-mode :defer t)

;; start services
(use-package prodigy :defer t)
(use-package rg :defer t)

;; load specific configuration for different user accounts (work, home)
;; from .config/emacs.$profile/*.el
(dolist (profile-name '("work" "home"))
  (let ((profile-dir (expand-file-name (concat "emacs." profile-name) (xdg-config-home))))
    (if (file-directory-p profile-dir)
        (progn
          (message "Including files in: %s" profile-dir)
          (mapc 'load (file-expand-wildcards (concat profile-dir "/*.el")))))))

;; email
(defconst mu4e-system-path "/usr/share/emacs/site-lisp/mu4e")
(use-package mu4e
  :defer t
  :ensure nil
  :load-path mu4e-system-path
  :commands 'mu4e
  :config
  (add-hook 'mu4e-compose-mode-hook (lambda () (setq show-trailing-whitespace t)))
  (add-hook 'mu4e-search-bookmark-hook (lambda (_) (setq mu4e-headers-include-related nil)))
  :init
  ;; nerdfonts for marks
  (setq mu4e-headers-unread-mark    '("u" . ""))
  (setq mu4e-headers-draft-mark     '("D" . ""))
  (setq mu4e-headers-flagged-mark   '("F" . "󰈻"))
  (setq mu4e-headers-new-mark       '("N" . ""))
  (setq mu4e-headers-passed-mark    '("P" . "↪"))
  (setq mu4e-headers-replied-mark   '("R" . "↩ "))
  (setq mu4e-headers-seen-mark      '("S" . ""))
  (setq mu4e-headers-trashed-mark   '("T" . ""))
  (setq mu4e-headers-attach-mark    '("a" . "󰁦"))
  (setq mu4e-headers-encrypted-mark '("x" . "󰌆"))
  (setq mu4e-headers-calendar-mark  '("c" . "󰃭"))
  (setq mu4e-headers-signed-mark    '("s" . "󰷼 "))
  (setq mu4e-headers-personal-mark  '("P" . ""))
  :custom
  (mu4e-date-format "%F")
  (mu4e-headers-date-format "%F")
  (mu4e-use-fancy-chars t)
  (smtpmail-queue-dir (expand-file-name "~/Mail/queue/cur"))
  (message-signature-file (expand-file-name "~/.signature"))
  (mu4e-compose-dont-reply-to-self t)
  (mu4e-view-prefer-html t)
  (mu4e-view-show-images t)
  (mu4e-attachment-dir (expand-file-name "~/Downloads"))
  (mu4e-update-interval 1800)
  (mu4e-view-fields '(:subject :to :from :cc :bcc :from-or-to :date :attachments :maildir :mailing-list))
  (mu4e-maildir (expand-file-name "~/Mail"))
  (smtpmail-queue-mail nil)
  (mu4e-get-mail-command "/usr/bin/mbsync -aV")
  (message-send-mail-function 'message-send-mail-with-sendmail)
  (sendmail-program "/usr/bin/msmtp"))

(use-package outlook
  :after mu4e
  :defer t
  :init
  (require 'outlook-mu4e))

(use-package mu4e-jump-to-list
  :after mu4e
    :defer t)
(use-package mu4e-contrib
  :ensure nil
  :after mu4e
  :defer t
  :load-path mu4e-system-path)

(use-package mu4e-conversation
  :after mu4e
  :defer t)

(use-package mu4e-query-fragments
  :after mu4e
  :defer t)

(use-package org-mu4e
  :defer t
  :ensure nil
  :after mu4e
  :load-path mu4e-system-path)

(use-package mu4e-icalendar
  :defer t
  :ensure nil
  :after mu4e
  :load-path mu4e-system-path
  :config
  (require 'mu4e-icalendar)
  (mu4e-icalendar-setup))

(use-package mu4e-views
  :defer t
  :after mu4e
  :bind (:map mu4e-headers-mode-map
	      ("v" . mu4e-views-mu4e-select-view-msg-method) ;; select viewing method
	      ("M-n" . mu4e-views-cursor-msg-view-window-down) ;; from headers window scroll the email view
	      ("M-p" . mu4e-views-cursor-msg-view-window-up) ;; from headers window scroll the email view
        ("f" . mu4e-views-toggle-auto-view-selected-message)) ;; toggle opening messages automatically when moving in the headers view)
  :config
  (setq mu4e-views-next-previous-message-behaviour 'always-switch-to-view)
  (setq mu4e-views-completion-method 'ivy)
  (setq mu4e-views-default-view-method "text")
  (mu4e-views-mu4e-use-view-msg-method "text")
  (setq mu4e-views-auto-view-selected-message nil))

(use-package mu4e-column-faces
  :after mu4e
  :defer t
  :config (mu4e-column-faces-mode))

;; takes all calendars generated by vdirsyncer in .cache/vdirsyncer/calendars
;; and generates diary files, and a top-level .cache/emacs/diary/calendars-diary
;; including all single calendar diaries
(defun duncan/generate-diary-from-calendars ()
  "Generate diary files and calendars-diary from .ics files."
  (interactive)
  (let* ((ics-directory (expand-file-name "vdirsyncer/calendars/" (xdg-cache-home)))
         (calendars-diary-directory (expand-file-name "emacs/diary/calendars/" (xdg-cache-home)))
         (calendar-list (directory-files ics-directory nil "\\.ics$"))
         (calendars-diary-file diary-file)
         (calendar-date-style 'european))
    (unless (file-exists-p calendars-diary-directory)
      (make-directory calendars-diary-directory t))
    (with-temp-file calendars-diary-file
      (dolist (ics-file-name calendar-list)
        (let* ((ics-file-path (concat ics-directory ics-file-name))
               (diary-file-name (file-name-sans-extension ics-file-name))
               (diary-file-path (concat calendars-diary-directory diary-file-name ".ics")))
          (message "importing %s" ics-file-path)
          (with-temp-buffer
            (insert-file-contents ics-file-path)
            (when (file-exists-p diary-file-path)
                (message "deleting old diary file")
                (delete-file diary-file-path t)
                (with-temp-buffer (write-file diary-file-path)))
            (icalendar-import-buffer diary-file-path t t))
          (insert (format "#include \"%s\"\n" diary-file-path)))))))

(use-package org-web-tools
  :defer t)

(defun --elfeed-mark-all-as-read ()
  (interactive)
  (mark-whole-buffer)
  (elfeed-search-untag-all-unread))

;; From nooker blog.
(defun --elfeed-eww-open (&optional use-generic-p)
  "open with eww"
  (interactive "P")
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
             do (elfeed-untag entry 'unread)
             when (elfeed-entry-link entry)
             do (eww-browse-url it))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

(defun --elfeed-firefox-open (&optional use-generic-p)
  "open with firefox"
  (interactive "P")
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
             do (browse-url-firefox (elfeed-entry-link entry)))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

;; feeds
(use-package elfeed
  :defer t
  :bind (("C-c 3" . elfeed)
         :map elfeed-search-mode-map
         ("R" . --elfeed-mark-all-as-read)
         ("I" . elfeed-protocol-owncloud-reinit)
         ("O" . elfeed-protocol-owncloud-update-older)
         ("S" . elfeed-protocol-owncloud-update-star)
         ("U" . elfeed-protocol-owncloud-update)
         ("f" . --elfeed-firefox-open)
         ("e" . --elfeed-eww-open))
  :custom
  (shr-max-image-proportion 0.3)
  (elfeed-search-filter "+unread @1-week-ago")
  (elfeed-use-curl t)
  (elfeed-log-level 'debug)
  (elfeed-feeds '(("owncloud+https://dmacvicar@cloud.mac-vicar.eu"
                   :use-authinfo t))))

(use-package elfeed-protocol
  :defer t
  :after elfeed
  :custom
  ;; allow to use nextcloud news
  (elfeed-protocol-enabled-protocols '(owncloud)))
;(advice-add 'elfeed :after #'elfeed-protocol-enable)

(use-package elfeed-score
  :defer t
  :after elfeed
  :config
  (progn
    (elfeed-score-enable)
    (define-key elfeed-search-mode-map "=" elfeed-score-map)))

;; pinboard feed has no content. This inserts a cleaned up html into the elfeed db
;; adapted from https://punchagan.muse-amuse.in/blog/elfeed-hook-to-fetch-full-content/
(defun --elfeed-get-entry-content (entry)
  "Fetches content for pinboard entries that are not tweets."
  (interactive
   (let ((entry elfeed-show-entry))
     (list entry)))
  (let ((url (elfeed-entry-link entry))
        (feed-id (elfeed-deref (elfeed-entry-feed-id entry)))
        (content (elfeed-deref (elfeed-entry-content entry))))
    (require 'org-web-tools)
    (require 'eww)
    (require 'url)
    (when (and (s-matches? "feeds.pinboard.in/" feed-id)
               (not (s-matches? "twitter.com/\\|pdf$\\|png$\\|jpg$" url))
               (string-equal "" content))
      (elfeed-curl-retrieve url
                            (lambda (status)
                              (if status
                                  (let* ((data (buffer-string))
                                         (doc (org-web-tools--eww-readable data))
                                         (title (car doc))
                                         (html (cdr doc)))
                                    (setf (elfeed-entry-content entry) (elfeed-ref html)))
                                (setf (elfeed-entry-content entry) (elfeed-ref "<h1>Not found</h1>"))))))))
(add-hook 'elfeed-new-entry-hook  #'--elfeed-get-entry-content)

(defun --elfeed-log-entry (entry) (message (elfeed-entry-feed-id entry)))
(add-hook 'elfeed-new-entry-hook  #'--elfeed-log-entry)
;(add-hook 'elfeed-new-entry-hook
;          (elfeed-make-tagger :feed-url "pinboard\\.in"
;                              :add 'saved))
;; eww
;; disable because the org-inhibit version check thing
(use-package shr-tag-code-highlight
  :defer t
  :disabled
  :ensure (:host github :repo "dmacvicar/shr-tag-code-highlight.el")
  :after shr
  :config
  (add-to-list 'shr-external-rendering-functions
               '(code . shr-tag-code-highlight)))

;; maps
(use-package osm
  :defer t
  :bind (("C-c m h" . osm-home)
         ("C-c m s" . osm-search)
         ("C-c m v" . osm-server)
         ("C-c m t" . osm-goto)
         ("C-c m x" . osm-gpx-show)
         ("C-c m j" . osm-bookmark-jump))
  :custom
  (osm-server 'default)
  (osm-copyright t)
  (osm-tile-directory (convert-standard-filename
                       (expand-file-name  "emacs/osm/" (xdg-cache-home)))))

;; contacts completion
;; http://pragmaticemacs.com/emacs/tweaking-email-contact-completion-in-mu4e/
;;need this for hash access
(require 'subr-x)

;;my favourite contacts - these will be put at front of list
(setq dmacvicar/contact-file "~/.favorite-contacts.txt")

(use-package emacs
  :ensure nil
  :config
  :commands (duncan/cwd-fn)
  :config
  (defun duncan/cwd-fn ()
    (expand-file-name
     ;; custom-src-directory is supposed to come from .dir-locals.el
     (if (boundp 'custom-src-directory)
         custom-src-directory
       (or (when-let ((project (project-current)))
             (project-root project))
           default-directory)))))

(use-package envrc
 :config
 (envrc-global-mode))

(defun dmacvicar/read-contact-list ()
  "Return a list of email addresses"
  (with-temp-buffer
    (when (file-exists-p dmacvicar/contact-file)
      (insert-file-contents dmacvicar/contact-file))
    (split-string (buffer-string) "\n" t)))

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
