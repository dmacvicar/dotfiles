;; -*- coding: utf-8; lexical-binding: t -*-

;; make startup faster by avoiding gc pauses
(setq gc-cons-threshold most-positive-fixnum
      native-comp-jit-compilation t)

(defvar elpaca-installer-version 0.11)
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
    (when (<= emacs-major-version 28) (require 'subr-x))
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
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  (elpaca-use-package-mode)                 ; enable :elpaca use-package keyword
  (setq elpaca-use-package-by-default t     ; assume :elpaca t unless otherwise specified
        use-package-always-defer t          ; defer by default, use :demand t when needed
        use-package-expand-minimally t))    ; reduce macro expansion overhead

; block until current queue is processed - this will allow use of use-package right away
(elpaca-wait)

(use-package emacs
  :ensure nil
  :bind
  (("M-g r" . recentf)
   ("M-s g" . grep)
   ("M-s f" . find-name-dired))
  :custom
  (inhibit-startup-screen t)
  (initial-buffer-choice nil)
  (initial-scratch-message nil)
  (initial-major-mode 'text-mode)
  (sentence-end-double-space nil)
  (ring-bell-function 'ignore)
  (visible-bell nil)
  (save-interprogram-paste-before-kill t)
  (use-dialog-box nil)
  (mark-even-if-inactive nil)
  (kill-whole-line t)
  (compilation-read-command nil)
  (compilation-scroll-output 'first-error)
  (use-short-answers t)
  (fast-but-imprecise-scrolling t)
  (load-prefer-newer t)
  (confirm-kill-processes nil)
  (native-comp-async-report-warnings-errors 'silent)
  (truncate-string-ellipsis "…")
  (confirm-kill-emacs nil)
  (global-auto-revert-mode t)
  (auto-revert-use-notify t)
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  (delete-by-moving-to-trash t)
  (fill-column 80)
  (make-backup-files nil)
  (auto-save-default nil)
  (create-lockfiles nil)
  (dired-listing-switches "-lt")
  (frame-title-format '(""
			invocation-name
			" - "
			(:eval
			 (if (buffer-file-name)
			     (abbreviate-file-name (buffer-file-name))
			   "%b"))))
  (custom-file
   (convert-standard-filename
    (expand-file-name  "emacs/custom.el" (xdg-cache-home))))
  :config
  (set-charset-priority 'unicode)
  (prefer-coding-system 'utf-8-unix)
  (when (file-exists-p custom-file)
    (load custom-file :noerror))
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))
  (add-hook 'text-mode-hook (lambda () (setq show-trailing-whitespace t)))
  (column-number-mode)
  ;; if you type with text selected, delete it
  (delete-selection-mode 1)
  (pixel-scroll-mode))

;; use gnome secret service to store passwords
;; TODO add keepass
(use-package auth-source
  :ensure nil
  :custom
  (auth-sources '("secrets:login"))
  (auth-source-cache-expiry nil)
  (auth-source-debug 'trivia))

;; kitty protocol
(use-package kkp
  :ensure t
  :config
  ;; (setq kkp-alt-modifier 'alt) ;; use this if you want to map the Alt keyboard modifier to Alt in Emacs (and not to Meta)
  (global-kkp-mode +1))

;; copy from clipboard in terminal
(use-package xclip
  :demand t
  :custom
  (xclip-method (if (getenv "WAYLAND_DISPLAY") 'wl-copy 'xclip))
  :config
  (xclip-mode))

;; recent files
(use-package recentf
  :ensure nil
  :demand t
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
  :demand t
  :custom
  (savehist-file (convert-standard-filename
       (expand-file-name  "emacs/history" (xdg-state-home))))
  :config
  (savehist-mode))

;; modeline
(use-package doom-modeline
  :hook (elpaca-after-init-hook . doom-modeline-mode))

;; dashboard screen

(use-package grid
  :ensure (:host github :repo "ichernyshovvv/grid.el")
  :demand t)

(use-package enlight
  :ensure (:host github :repo "ichernyshovvv/enlight")
  :demand t
  :after grid
  :init
  (add-hook 'elpaca-after-init-hook (lambda()
                               (if (null (cdr command-line-args))
                                   (enlight-open))))
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
             (progn
              (require 'org)
              (find-file (seq-find (lambda (x) (string-match "todo.org" x)) org-agenda-files))) "t"))
          ("Misc"
           ("Emacs Configuration" (find-file user-init-file) "c")
           ("Home folder" (dired "~/") "h")))))))

;; discoverability
(use-package which-key
  :demand t
  :config
  (which-key-mode)
  :custom
  (which-key-popup-type 'minibuffer))

;; hide commands in M-x which do not apply to the current mode.
(setq read-extended-command-predicate #'command-completion-default-include-p)

;; completion system (alternative to ivy)
(use-package vertico
  :demand t
  :ensure (:files (:defaults "extensions/vertico-directory.el" "extensions/vertico-sort.el"))
  :config
  (vertico-mode))

(use-package vertico-sort
  :ensure nil
  :demand t
  :after vertico
  :config
  (setq vertico-sort-function #'vertico-sort-history-alpha))

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
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-preview-key '("S-<down>" "S-<up>"))
  :bind
  ("C-s" . consult-line)
  ("C-x b" . consult-buffer)
  ("C-x p b" . consult-project-buffer)

  :config)

(use-package tab-bar
  :ensure nil
  :demand t
  :config
  (tab-bar-mode)
  (bind-key* "M-S-<left>" #'tab-previous)
  (bind-key* "M-S-<right>" #'tab-next)
  :custom
  (tab-bar-show 1))

(use-package project
  :ensure nil
  :demand t
  :custom
  (project-vc-extra-root-markers
   '(".project" "Cargo.toml" "go.mod" "package.json" "pyproject.toml"
     "build.zig" "CMakeLists.txt" "meson.build" "Makefile"))
  :config
  (defun duncan/project-create-tab ()
    (interactive)
    (if-let ((project (project-current)))
        (progn
          (tab-bar-new-tab)
          (tab-bar-rename-tab (project-name project))
          (project-dired project))
      (user-error "No project in current directory")))
  (setq project-switch-commands #'duncan/project-create-tab))

;; complete in any order
(use-package orderless
  :demand t
  :config
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; add context to completions. eg. help to M-x functions
(use-package marginalia
  :demand t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :custom
  (marginalia-align 'right)
  :init
  (marginalia-mode))

(use-package nerd-icons
  :if (display-graphic-p))

(use-package nerd-icons-completion
  :demand t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

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
  :disabled t
  :custom
  (emojify-emojis-dir (convert-standard-filename
                       (expand-file-name  "emacs/emojis" (xdg-cache-home))))
  :hook (after-init . global-emojify-mode))

(use-package shell-pop
  :custom
  (shell-pop-universal-key "C-t"))

;; terminal emulator (replaces vterm, in pure-lisp)
;; provides eat-project to start a shell in current project
;; if TERM is not functional, install tic (terminfo compiler) from
;; ncurses devel and run eat-compile-termifo
(use-package eat
  :ensure (:host nil :type git
           :repo "https://codeberg.org/akib/emacs-eat"
           :files ("*.el" "dir"
                   ("integration" "integration/*")
                   "*.info" "*.texi"
                   "*.ti" ("e" "e/*"))))

;; theme
(use-package modus-themes
  :demand t
  :custom
  (modus-themes-mixed-fonts t)
  (modus-themes-org-blocks 'gray-background)
  (modus-themes-fringes 'intense)
  :config
  (defun duncan/theme-from-dbus (value)
    "Change the theme based on a D-Bus property.

VALUE should be an integer or an arbitrarily nested list that
contains an integer.  When VALUE is equal to 2 then a dark theme
will be selected, otherwise a light theme will be selected (0 is default)"
    (load-theme (if (= 1 (car (flatten-list value)))
                    'modus-vivendi-tinted
                  'modus-operandi-tinted)
                t))
  (if (featurep 'dbusbind)
      (condition-case err
          (progn
            (require 'dbus)
            ;; Set the current theme based on what the system theme is right now
            (dbus-call-method-asynchronously
             :session "org.freedesktop.portal.Desktop"
             "/org/freedesktop/portal/desktop"
             "org.freedesktop.portal.Settings"
             "Read"
             #'duncan/theme-from-dbus
             "org.freedesktop.appearance"
             "color-scheme")
            ;; Register to be notified when the system theme changes:
            (dbus-register-signal
             :session "org.freedesktop.portal.Desktop"
             "/org/freedesktop/portal/desktop"
             "org.freedesktop.portal.Settings"
             "SettingChanged"
             (lambda (path var value)
               (when (and (string-equal path "org.freedesktop.appearance")
                          (string-equal var "color-scheme"))
                 (duncan/theme-from-dbus value)))))
        (error
         (message "D-Bus theme sync unavailable: %s" (error-message-string err))))
    (message "D-Bus not available; skipping theme sync")))

(elpaca-wait)

(define-key global-map (kbd "<f5>") #'modus-themes-toggle)

(set-face-attribute 'default nil :family "FiraCode Nerd Font Mono" :height 130)
(set-face-attribute 'variable-pitch nil :family "Noto Sans")
(set-face-attribute 'fixed-pitch nil :family (face-attribute 'default :family) :height 110)

(use-package visual-fill-column
  :custom
  (visual-fill-column-adjust-for-text-scale t)
  (visual-fill-column-width 100)
  (visual-fill-column-center-text t)
  (visual-fill-column-fringes-outside-margins nil))
(advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)

(use-package mixed-pitch
  :custom
  (mixed-pitch-variable-pitch-cursor '(bar . 3)))

;; nicer default frame size depending of screen
(defun duncan/set-frame-size-according-to-resolution ()
  (interactive)
  (when (display-graphic-p)
    (let* ((width (if (> (display-pixel-width) 1280) 120 80))
          (height-px (floor (* (display-pixel-height) 0.85)))
          (height (floor (/ height-px (frame-char-height))))
          (top-px (floor (/ (- (display-pixel-height) height-px) 2))))
      (add-to-list 'default-frame-alist
                   (cons 'height height))
      (add-to-list 'default-frame-alist
                   (cons 'width width)))))

(duncan/set-frame-size-according-to-resolution)
;; not decided yet if maximizing is better?
;;(add-to-list 'default-frame-alist '(fullscreen . maximized))

(use-package window
  :ensure nil
  :custom
  (display-buffer-alist
   '(("\\*\\(Messages\\|Warnings\\|grep\\|rg\\|Org Agenda\\|Output\\|Async Shell Command\\|compilation\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 0))
     ((lambda (bufname action)
        (with-current-buffer bufname
          (bound-and-true-p gptel-mode)))
      (display-buffer-in-side-window)
      (window-width . 0.25)
      (side . right)
      (slot . 0)))))

;; popups
(use-package popper
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :custom
  ; respect display-buffer-alist
  (popper-display-control nil)
  :init
  (setq popper-reference-buffers
        '("\\*\\(Messages\\|Warnings\\|grep\\|rg\\|Org Agenda\\|Output\\|Async Shell Command\\|compilation\\)\\*"
          help-mode
          compilation-mode
	  go-test-mode
	  xref-mode
          (lambda (bufname)
            (with-current-buffer bufname
              (bound-and-true-p gptel-mode)))))
  (popper-mode +1)
  (popper-echo-mode +1))

;; window splitting functions
(use-package windmove
  :ensure nil)

(use-package tmux-pane
  :if (not (display-graphic-p))
  :demand t
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
  (progn
    (tmux-pane-mode)
    (bind-key* "M-<left>" #'tmux-pane-omni-window-left)
    (bind-key* "M-<right>" #'tmux-pane-omni-window-right)
    (bind-key* "M-<up>" #'tmux-pane-omni-window-up)
    (bind-key* "M-<down>" #'tmux-pane-omni-window-down)))

;; respect style of projects
(use-package editorconfig
  :demand t
  :config
  (editorconfig-mode 1))

;; put env variables and PATH in emacs process environment
(use-package mise
  :hook (elpaca-after-init-hook . global-mise-mode))

;; parenthesis
(use-package paren
  :ensure nil
  :hook ((prog-mode . show-paren-mode))
  :custom (show-paren-style 'expression))

(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))

;; highlight undoed text
(use-package undo-hl
  :ensure (:host github :repo "casouri/undo-hl")
  :hook ((prog-mode text-mode) . undo-hl-mode))

;; text completion
(use-package corfu
  :demand t
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode 1)
  :custom
  (corfu-auto t))

;; remove with emacs 31
(use-package corfu-terminal
  :demand t
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

(use-package kind-icon
  :ensure t
  :demand t
  :after corfu
  :custom
  (svg-lib-icons-dir (expand-file-name "emacs/svg-lib/" (xdg-cache-home)))
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  :bind ("C-c p" . cape-prefix-map)
  :init
  ;;(add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

;; dictation
(use-package whisper
  :ensure (:type git :host github :repo "natrys/whisper.el")
  :bind ("C-x !" . whisper-run)
  :config
  (setq whisper-install-directory (expand-file-name "emacs/whisper.el/" (xdg-cache-home))
        whisper-model "base"
        whisper-language "en"
        whisper-translate nil
        whisper-use-threads 4))

;; github copilot
(use-package copilot
  :custom
  (copilot-install-dir (expand-file-name "emacs/copilot/" (xdg-cache-home)))
  :ensure (:host github :repo "copilot-emacs/copilot.el"
                 :files ("dist" "*.el"))
  :bind (:map copilot-completion-map
              ("<tab>" . copilot-accept-completion)
              ("TAB" . copilot-accept-completion)))

(use-package claude-code-ide
  :ensure (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu)
  :custom
  (claude-code-ide-terminal-backend 'eat)
  :config
  (claude-code-ide-emacs-tools-setup))

;; different gpt models
(use-package gptel
  :bind
  (("C-c g m" . gptel-menu)
   ("C-c g c" . gptel)
   ("C-c g s" . gptel-send)
   ("C-c g a" . gptel-add)
   ("C-c g A" . gptel-abort))
  :custom
  (gptel-model 'codestral-latest)
  (gptel-backend (gptel-make-openai "openai"
                   :key (auth-source-pick-first-password
                         :host "api.openai.com" :user "apiKey")
                   :stream t
                   :models '("gpt-5-mini" "gpt-5")))
  :config
  (gptel-make-openai "github"
    :host "models.inference.ai.azure.com"
    :key (auth-source-pick-first-password
          :host "models.inference.ai.azure.com" :user "apiKey")
    :endpoint "/chat/completions"
    :stream t
    :models '(gpt-4o)))

(with-eval-after-load 'company
  ;; disable inline previews
  (delq 'company-preview-if-just-one-frontend company-frontends))

;; remote file access
(use-package tramp
  :ensure nil
  :config
  (setq vc-ignore-dir-regexp
	(format "\\(%s\\)\\|\\(%s\\)"
		vc-ignore-dir-regexp
		tramp-file-name-regexp))
  ;; turn off the backup feature for remote files and stop TRAMP from saving to the backup directory
  (add-to-list 'backup-directory-alist
	       (cons tramp-file-name-regexp nil)))

(use-package flymake
  :hook (prog-mode-hook . flymake-mode))

;; LSP
(use-package eglot
  :hook
  ((go-ts-mode
    zig-mode
    rust-ts-mode
    python-ts-mode
    c-ts-mode
    c++-ts-mode) . eglot-ensure)
  :bind (:map
         eglot-mode-map
         ("C-c l a" . eglot-code-actions)
         ("C-c l o" . eglot-code-actions-organize-imports)
         ("C-c l r" . eglot-rename)
         ("C-c l f" . eglot-format))
  :custom
  ;; bulb emoji changes line height on TUI
  ;; https://lists.nongnu.org/archive/html/bug-gnu-emacs/2025-10/msg01604.html
  ;; prevent margin eglot-code-action-indicator
  (eglot-code-action-indications '(eldoc-hint mode-line))
  (eglot-extend-to-xref t)
  :config
  (add-to-list 'eglot-server-programs
               `(python-ts-mode
                 . ,(eglot-alternatives '(("pyright-langserver" "--stdio")
                                          "jedi-language-server"
                                          "pylsp")))))

(use-package eldoc
  :ensure nil)

(use-package pulsar)

(use-package repeat
  :ensure nil
  :demand t
  :config
  (repeat-mode))

(use-package dape
  :config
  (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)
  ;; (remove-hook 'dape-start-hook 'dape-info)
  (remove-hook 'dape-start-hook 'dape-repl))

;; meson build system
(use-package meson-mode)

;; git
(use-package magit)

(use-package treesit
  :ensure nil
  :init
  (defvar duncan/treesit-grammar-dir
    (expand-file-name "emacs/tree-sitter" (xdg-cache-home)))
  (add-to-list 'treesit-extra-load-path duncan/treesit-grammar-dir)
  (setq treesit--install-language-grammar-out-dir-history
        (list duncan/treesit-grammar-dir))
  :config
  (dolist (mode '((bash-mode       . bash-ts-mode)
                  (c-mode          . c-ts-mode)
                  (c++-mode        . c++-ts-mode)
                  (cmake-mode      . cmake-ts-mode)
                  (python-mode     . python-ts-mode)
                  (css-mode        . css-ts-mode)
                  (dockerfile-mode . dockerfile-ts-mode)
                  (go-mode         . go-ts-mode)
                  (javascript-mode . js-ts-mode)
                  (js-json-mode    . json-ts-mode)
                  (typescript-mode . typescript-ts-mode)))
    (add-to-list 'major-mode-remap-alist mode)))

(use-package treesit-auto
  :demand t
  :custom
  (treesit-auto-install 'prompt)
  ; those are broken
  (treesit-auto-opt-out-list
        '(markdown protobuf ruby r yaml))
  :config
  (global-treesit-auto-mode))

;; structured navigation including expand region
(use-package combobulate
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
(use-package pdf-tools)

;; markdown
(use-package markdown-mode
  :hook
  (markdown-mode . visual-line-mode)
  (markdown-mode . visual-fill-column-mode)
  (markdown-mode . mixed-pitch-mode)
  :mode ("\\.md\\'" . gfm-mode))

;; use lang modes inside org src blocks
(use-package poly-org
  :demand t
  :config
  (add-to-list 'auto-mode-alist '("\\.org\\'" . poly-org-mode)))
;; use lang modes inside markdow code fences
(use-package poly-markdown
  :demand t
  :config
  (add-to-list 'auto-mode-alist '("\\.md\\'" . poly-gfm-mode)))
(use-package web-mode
  :mode "\\.qtpl\\'")
(use-package vue-html-mode)
;; vue single file component mode (uses polymode)
(use-package sfc-mode
  :ensure (:host github :repo "gexplorer/sfc-mode")
  :custom
  (sfc-template-default-mode 'vue-html-mode)
  :mode "\\.vue\\'")

(use-package json-mode)
(use-package yaml-mode)

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package dockerfile-ts-mode
  :ensure nil
  :mode
  "Dockerfile\\'"
  "\\.dockerfile\\'")
(use-package docker-compose-mode
  :ensure t)

(use-package terraform-ts-mode
  :mode "\\.tf\\'"
  :ensure (:host github :repo "kgrotel/terraform-ts-mode"))

;; guess indentation params
(use-package dtrt-indent
  :custom
  (dtrt-indent-max-lines 2000)
  (dtrt-indent-verbosity 2)
  :diminish
  :hook (prog-mode . dtrt-indent-mode))
(setq-default tab-width 8)

(use-package paredit
  :hook
  (ielm-mode-hook . paredit-mode)
  :config
  ;; Infortunately there is a key conflict between Paredit and IELM.
  ;; Paredit overrides return to execute paredit-RET, meaning that the
  ;; original ielm-return is not called.
  ;; https://www.reddit.com/r/emacs/comments/101uwgd/enable_paredit_mode_for_evalexpression_mini/
  (defun duncan/paredit-RET ()
    "Wraps `paredit-RET' to provide a sensible minibuffer experience"
    (interactive)
    (cond
     ((minibufferp)
      (read--expression-try-read))
     ((and (eq major-mode 'inferior-emacs-lisp-mode)
           (string-prefix-p "*ielm*" (buffer-name)))
      (ielm-return))
     (t
      (paredit-RET))))
  :bind
  (:map paredit-mode-map
        ("RET" . duncan/paredit-RET)
        ("C-j" . paredit-newline)))

;; elisp
(use-package ielm
  :ensure nil
  :config
  (add-hook 'ielm-mode-hook (lambda () (paredit-mode 1)))
  (add-hook 'ielm-mode-hook 'eldoc-mode)
  :custom
  (ielm-history-file-name (expand-file-name "emacs/ielm-history.eld" (xdg-cache-home))))

;; C
(setq-default c-default-style "linux")

(use-package cmake-ts-mode
  :ensure nil
  :mode "CMakeLists\\.txt\\'")

;; integrate compile command for project with cmake
(use-package cmake-project
  :ensure t
  :custom
  (cmake-project-default-build-dir-name "build"))
(defun duncan/maybe-cmake-project-hook ()
  (if (file-exists-p "CMakeLists.txt") (cmake-project-mode)))
(add-hook 'c-ts-mode-hook 'duncan/maybe-cmake-project-hook)
(add-hook 'c++-ts-mode-hook 'duncan/maybe-cmake-project-hook)

;; go
(use-package go-ts-mode
  :ensure nil
  :custom
  (go-test-verbose t)
  (gofmt-args '("-s")))

;; like play.golang.org
(use-package go-playground
  :bind (:map go-playground-mode-map
              ([M-return] . nil)
              ("C-c C-c" . go-playground-exec)))
;; generates tests from funcs
(use-package go-gen-test)

(use-package pet
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10))

(use-package rust-mode)
(use-package elixir-mode)
(use-package zig-mode)
(use-package nix-mode)
(use-package lua-mode)
(use-package typescript-mode
  :demand t
  :config
  (add-to-list 'auto-mode-alist '("\\.ts[x]?\\'" . typescript-mode)))

;; browse HN
(use-package hackernews
  :custom
  (hackernews-visited-links-file (convert-standard-filename
                                  (expand-file-name  "emacs/hackernews/visited-links.el" (xdg-cache-home)))))

;; https://d2lang.com/tour/intro/
(use-package d2-mode)

(custom-set-variables '(ad-redefinition-action (quote accept)))

(use-package calendar
  :ensure nil
  :init
  (setq calendar-view-diary-initially-flag t)
  (setq calendar-date-style 'european))

(use-package diary
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
  (defun duncan/org-http-image-data-fn (protocol link _description)
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
  (org-link-set-parameters "http"  :image-data-fun #'duncan/org-http-image-data-fn)
  (org-link-set-parameters "https" :image-data-fun #'duncan/org-http-image-data-fn)
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
  :ensure (:host github :repo "ichernyshovvv/org-timeblock"))

;; this takes care of being able to display inline images in the buffer
(use-package org-yt
  :after org
  ;;:ensure (org-yt :host github :repo "TobiasZawada/org-yt"))
  ;; https://github.com/TobiasZawada/org-yt/pull/1
  ;; don't require imagemagick for resizing
  :ensure (org-yt :host github :repo "league/org-yt"))

(use-package org-super-agenda
  :hook (org-agenda-mode . org-super-agenda-mode))
;(use-package org-ql
;  :after org)
(use-package org-superstar              ; supersedes `org-bullets'
  :after org
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '(" ")) ;; '("🞛" "◉" "○" "▷")
  (org-superstar-leading-bullet ?\s)
  (org-superstar-leading-fallback ?\s)
  (org-hide-leading-stars nil)
  (org-superstar-prettify-item-bullets t)
  :hook (org-mode . org-superstar-mode))

(use-package org-fancy-priorities ; priority icons
  :after org
  :hook (org-mode . org-fancy-priorities-mode)
  :hook (org-agenda-mode . org-fancy-priorities-mode)
  :custom
  (org-fancy-priorities-list '("⚑" "⬆" "■")))

(use-package org-modern
  :hook (org-mode . org-modern-mode))

(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autoemphasis  t)
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t))

;; Avoid `org-babel-do-load-languages' since it does an eager require.
(use-package ob-mermaid
  :custom
  (ob-mermaid-cli-path "mmdr")
  :commands (org-babel-execute:mermaid))
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
(use-package ox-gfm)
(use-package ox-reveal)
(use-package org-tree-slide)
(use-package hide-mode-line)
(use-package org-present
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
  :custom
  (ein:output-area-inlined-images t))

(use-package denote
  :bind ("C-c d" . denote)
  :config
  (with-eval-after-load 'org
    (require 'denote)))

(use-package consult-notes
  :commands (consult-notes
             consult-notes-search-in-all-notes))
(use-package htmlize)
(use-package protobuf-mode)

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
  :ensure nil
  :load-path mu4e-system-path
  :commands 'mu4e
  :config
  (add-hook 'mu4e-compose-mode-hook (lambda () (setq show-trailing-whitespace t)))
  (add-hook 'mu4e-search-bookmark-hook (lambda (_) (setq mu4e-headers-include-related nil)))
  ;; signature
  ;(flycheck-add-mode 'vale 'mu4e-compose-mode)
  ;; minor modes for compose mode
  (add-hook 'mu4e-compose-mode-hook (lambda ()
                                        (flyspell-mode)
                                        (auto-fill-mode -1)
                                        (visual-line-mode 1)
                                        (turn-off-smartparens-mode)))
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
  (mu4e-completing-read-function 'completing-read)
  (mu4e-compose-format-flowed t)
  (mm-text-html-renderer 'shr)
  ;(mm-text-html-renderer 'gnus-w3m)
  ;(shr-color-visible-luminance-min 80)
  ;(shr-color-visible-distance-min 5)
  ;(gnus-inhibit-images t)
  (mu4e-context-policy 'pick-first)
  (mu4e-compose-context-policy 'ask)
  (mail-user-agent 'mu4e-user-agent)
  (mu4e-date-format "%F")
  (mu4e-headers-date-format "%F")
  (mu4e-use-fancy-chars t)
  (smtpmail-queue-dir (expand-file-name "~/Mail/queue/cur"))
  (message-signature-file (expand-file-name "~/.signature"))
  (message-dont-reply-to-names 'mu4e-personal-or-alternative-address-p)
  (mu4e-attachment-dir (expand-file-name "~/Downloads"))
  (mu4e-update-interval 1800)
  (mu4e-view-fields '(:subject :to :from :cc :bcc :from-or-to :date :attachments :maildir :mailing-list))
  (smtpmail-queue-mail nil)
  (mu4e-get-mail-command "/usr/bin/mbsync -aV")
  (mu4e-change-filenames-when-moving t)
  (message-kill-buffer-on-exit t)
  (message-send-mail-function 'message-send-mail-with-sendmail)
  (user-full-name "Duncan Mac-Vicar P.")
  (sendmail-program "/usr/bin/msmtp"))

(use-package outlook
  :after mu4e
  :init
  (require 'outlook-mu4e))

(use-package mu4e-jump-to-list
  :after mu4e)
(use-package mu4e-contrib
  :ensure nil
  :after mu4e
  :load-path mu4e-system-path)

(use-package mu4e-conversation
  :after mu4e)

(use-package mu4e-query-fragments
  :after mu4e)

(use-package org-mu4e
  :ensure nil
  :after mu4e
  :load-path mu4e-system-path)

(use-package mu4e-icalendar
  :ensure nil
  :after mu4e
  :load-path mu4e-system-path
  :config
  (require 'mu4e-icalendar)
  (mu4e-icalendar-setup))

(use-package mu4e-views
  :after mu4e
  :bind (:map mu4e-headers-mode-map
	      ("v" . mu4e-views-mu4e-select-view-msg-method) ;; select viewing method
	      ("M-n" . mu4e-views-cursor-msg-view-window-down) ;; from headers window scroll the email view
	      ("M-p" . mu4e-views-cursor-msg-view-window-up) ;; from headers window scroll the email view
        ("f" . mu4e-views-toggle-auto-view-selected-message)) ;; toggle opening messages automatically when moving in the headers view)
  :config
  (setq mu4e-views-next-previous-message-behaviour 'always-switch-to-view)
  (setq mu4e-views-completion-method 'default)
  (setq mu4e-views-default-view-method "text")
  (mu4e-views-mu4e-use-view-msg-method "text")
  (setq mu4e-views-auto-view-selected-message nil))

(use-package mu4e-column-faces
  :after mu4e
  :config (mu4e-column-faces-mode))

;; takes all calendars generated by vdirsyncer in .cache/vdirsyncer/calendars
;; and generates diary files, and a top-level .cache/emacs/diary/calendars-diary
;; including all single calendar diaries
(defun duncan/generate-diary-from-calendars ()
  "Generate diary files and calendars-diary from .ics files."
  (interactive)
  (let* ((ics-directory (expand-file-name "vdirsyncer/calendars/" (xdg-cache-home)))
         (calendars-diary-directory (expand-file-name "emacs/diary/calendars/" (xdg-cache-home)))
         (calendars-diary-file diary-file)
         (calendar-date-style 'european))
    (if (file-directory-p ics-directory)
        (progn
          (unless (file-exists-p calendars-diary-directory)
            (make-directory calendars-diary-directory t))
          (with-temp-file calendars-diary-file
            (dolist (ics-file-name (directory-files ics-directory nil "\\.ics$"))
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
                (insert (format "#include \"%s\"\n" diary-file-path))))))
      (message "No vdirsyncer calendars directory at %s; skipping diary generation" ics-directory))))

(use-package org-web-tools)

(defun duncan/elfeed-mark-all-as-read ()
  (interactive)
  (mark-whole-buffer)
  (elfeed-search-untag-all-unread))

;; From nooker blog.
(defun duncan/elfeed-eww-open (&optional use-generic-p)
  "open with eww"
  (interactive "P")
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
             do (elfeed-untag entry 'unread)
             when (elfeed-entry-link entry)
             do (eww-browse-url it))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

(defun duncan/elfeed-firefox-open (&optional use-generic-p)
  "open with firefox"
  (interactive "P")
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
             do (browse-url-firefox (elfeed-entry-link entry)))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

;; feeds
(use-package elfeed
  :bind (("C-c 3" . elfeed)
         :map elfeed-search-mode-map
         ("R" . duncan/elfeed-mark-all-as-read)
         ("I" . elfeed-protocol-owncloud-reinit)
         ("O" . elfeed-protocol-owncloud-update-older)
         ("S" . elfeed-protocol-owncloud-update-star)
         ("U" . elfeed-protocol-owncloud-update)
         ("f" . duncan/elfeed-firefox-open)
         ("e" . duncan/elfeed-eww-open))
  :custom
  (shr-max-image-proportion 0.3)
  (elfeed-search-filter "+unread @1-week-ago")
  (elfeed-use-curl t)
  (elfeed-log-level 'debug)
  (elfeed-feeds '(("owncloud+https://dmacvicar@cloud.mac-vicar.eu"
                   :use-authinfo t))))

(use-package elfeed-protocol
  :after elfeed
  :custom
  ;; allow to use nextcloud news
  (elfeed-protocol-enabled-protocols '(owncloud)))
;(advice-add 'elfeed :after #'elfeed-protocol-enable)

(use-package elfeed-score
  :after elfeed
  :config
  (progn
    (elfeed-score-enable)
    (define-key elfeed-search-mode-map "=" elfeed-score-map)))

;; pinboard feed has no content. This inserts a cleaned up html into the elfeed db
;; adapted from https://punchagan.muse-amuse.in/blog/elfeed-hook-to-fetch-full-content/
(defun duncan/elfeed-get-entry-content (entry)
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
(add-hook 'elfeed-new-entry-hook  #'duncan/elfeed-get-entry-content)

(defun duncan/elfeed-log-entry (entry) (message (elfeed-entry-feed-id entry)))
(add-hook 'elfeed-new-entry-hook  #'duncan/elfeed-log-entry)
;(add-hook 'elfeed-new-entry-hook
;          (elfeed-make-tagger :feed-url "pinboard\\.in"
;                              :add 'saved))
;; eww
;; disable because the org-inhibit version check thing
(use-package shr-tag-code-highlight
  :disabled
  :ensure (:host github :repo "dmacvicar/shr-tag-code-highlight.el")
  :after shr
  :config
  (add-to-list 'shr-external-rendering-functions
               '(code . shr-tag-code-highlight)))

;; maps
(use-package osm
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
(use-package obsolete
  :ensure (obsolete
           :repo "dmacvicar/obsolete.el"
           :host github
           :protocol ssh))

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

(when (getenv "EMACS_PROFILE_START")
  (add-hook 'emacs-startup-hook
	    (lambda ()
	      (message "Emacs ready in %s with %d garbage collections."
		       (emacs-init-time) gcs-done))))

;; intelligent garbage collection during idle time
(use-package gcmh
  :hook (elpaca-after-init-hook . gcmh-mode)
  :custom
  (gcmh-idle-delay 'auto)
  (gcmh-high-cons-threshold (* 64 1024 1024)))

(provide 'init)
;;; init.el ends here
