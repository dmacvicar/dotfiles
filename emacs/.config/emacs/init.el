;; -*- coding: utf-8; lexical-binding: t -*-

;; make startup faster by avoiding gc pauses
(setq gc-cons-threshold (* 50 1000 1000))
(defconst emacs-start-time (current-time))

;; speedup
(setq straight-check-for-modifications nil)

;; bootstrap straight package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" straight-base-dir))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq use-package-compute-statistics t)
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(eval-when-compile
  (require 'use-package))

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

(global-display-line-numbers-mode t)
(column-number-mode)
;; if you type with text selected, delete it
(delete-selection-mode 1)

;; misc defaults
(setq inhibit-startup-screen t
      initial-scratch-message nil
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

(duncan/set-frame-size-according-to-resolution)
;; not decided yet if maximizing is better?
;;(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; use gnome secret service to store passwords
;; TODO add keepass
(use-package auth-source
  :straight (:type built-in)
  :custom
  (auth-sources '("secrets:login"))
  (auth-source-cache-expiry nil)
  (auth-source-debug 'trivia))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; copy from clipboard in terminal
(use-package xclip
  :init
  (xclip-mode)
  :ensure t)

;; recent files
(use-package recentf
  :straight (:type built-in)
  :config
  (recentf-mode)
  :custom
  (recentf-save-file
   (convert-standard-filename
       (expand-file-name  "emacs/recentf" (xdg-state-home))))
  (recentf-max-menu-items 25)
  (recentf-max-saved-items 25)
  (recentf-exclude '("/autosave$"
		     "/treemacs-persist$")))

;; minibuffer history
(use-package savehist
  :straight (:type built-in)
  :custom
  (savehist-file (convert-standard-filename
       (expand-file-name  "emacs/history" (xdg-state-home))))
  :init
  (savehist-mode))

;; modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; popups
(use-package popper
  :ensure t ; or :straight t
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

;; completion system (alternative to ivy)
(use-package vertico
  :init
  (vertico-mode)
  :straight (vertico
             :files (:defaults "extensions/vertico-directory.el")
             :includes (vertico-directory)))

;; configure directory extension
(use-package vertico-directory
  :straight nil
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
   (setq xref-show-xrefs-function #'consult-xref
         xref-show-definitions-function #'consult-xref)
  :bind
  ("C-s" . consult-line)
  ("C-x b" . consult-buffer)
  :config)

(bind-keys*
 ("M-RET" . consult-buffer)
 ("C-M-j" . consult-buffer))

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

(ignore-errors (set-frame-font "JuliaMono-12"))

(use-package all-the-icons)

(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

;; emojis
;; todo, set custom (emojify-display-style) when
;; we get a good font
(use-package emojify
  :hook (after-init . global-emojify-mode))

(use-package shell-pop
  :custom
  (shell-pop-universal-key "C-t")
  :defer t)

(use-package vterm
  :custom
  (vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no")
  :hook
  ((vterm-mode . (lambda ()
                   (duncan/vterm-configure-faces)
                  (setq-local global-hl-line-mode nil
                               solaire-mode nil))))
  :init
  (defun duncan/vterm-configure-faces ()
    (face-remap-add-relative 'default :background "#ffffdd")
    ;; I am not sure if the background color has to be set here, or use #0000
    (set-face-attribute 'vterm-color-black nil :foreground "#171421" :background "#5E5C64")
    (set-face-attribute 'vterm-color-red nil :foreground "#C01C28" :background "#F66151")
    (set-face-attribute 'vterm-color-green nil :foreground "#26A269" :background "#33DA7A")
    (set-face-attribute 'vterm-color-yellow nil :foreground "#A2734C" :background "#E9AD0C")
    (set-face-attribute 'vterm-color-blue nil :foreground "#12488B" :background "#2A7BDE")
    (set-face-attribute 'vterm-color-magenta nil :foreground "#A347BA" :background "#C061CB")
    (set-face-attribute 'vterm-color-cyan nil :foreground "#2AA1B3" :background "#33C7DE")
    (set-face-attribute 'vterm-color-white nil :foreground "#D0CFCC" :background "#FFFFFF")
    (set (make-local-variable 'buffer-face-mode-face) '(:family "Source Code Pro Regular" :foreground "#000000"))
    ;; not working...
    (display-line-numbers-mode nil)
    ;;
    (buffer-face-mode t))
  :ensure t
  :defer t)

;; teme
(use-package leuven-theme
  :custom
  (leuven-scale-outline-headlines nil)
  (leuven-scale-org-agenda-structure nil)
  :config
  (load-theme 'leuven t))

;; window splitting functions
(use-package windmove
  :defer t
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
  :defer t)
;; define M-arrows using escape codes so that they
;; work in terminal
(define-key input-decode-map "\e\eOA" [(meta up)])
(define-key input-decode-map "\e\eOB" [(meta down)])
(define-key input-decode-map "\e\eOC" [(meta right)])
(define-key input-decode-map "\e\eOD" [(meta left)])

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

;; tabs 4 spaces width (we only use spaces in Go)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil)

;; whitespace highlight when coding
(setq-default show-trailing-whitespace t)

;; parenthesis
(use-package paren
  :defer t
  :hook ((prog-mode . show-paren-mode))
  :custom (show-paren-style 'expression))

(use-package rainbow-delimiters
    :hook ((prog-mode . rainbow-delimiters-mode)))

;; highlight undoed text
(use-package undo-hl
  :defer t
  :straight (
    :host github
    :repo "casouri/undo-hl")
  :config
  (add-hook 'prog-mode-hook #'undo-hl-mode)
  (add-hook 'text-mode #'undo-hl-mode))

;; text completion
(use-package company
  :defer t
  :config (add-hook 'prog-mode-hook 'company-mode))

;; github copilot
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el"
                   :files ("dist" "copilot.el")))
(with-eval-after-load 'company
  ;; disable inline previews
  (delq 'company-preview-if-just-one-frontend company-frontends))
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

;; remote file access
(use-package tramp
  :straight (:type built-in)
  :defer t
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
  :commands lsp
  :defer t
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

(use-package company-lsp
  :commands company-lsp
  :defer t
  :config
  (push 'company-lsp company-backends))

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

;; debugger
(use-package dap-mode
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t)
  (require 'dap-go)
  (dap-go-setup))

;; meson build system
(use-package meson-mode
  :ensure t
  :defer t)

(use-package dap-java
  :defer t
  :after (lsp-java)
  :straight (:type built-in))

;; git
(use-package magit
  :ensure t
  :defer t)

;; never lose your cursor
(use-package beacon
  :ensure t
  :defer 5
  :config
  (setq beacon-push-mark 5)
  (setq beacon-size 25))

;; `M-x combobulate' (or `C-c o o') to start using Combobulate
(use-package treesit
  :straight (:type built-in)
  :init
  (dolist (mapping '((python-mode . python-ts-mode)
                     (c-mode . c-ts-mode)
                     (typescript-mode . tsx-ts-mode)
                     (c++-mode . c++-ts-mode)
                     (rust-mode . rust-ts-mode)
                     (go-mode . go-ts-mode)
                     (yaml-mode . yaml-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  ;; we could install gramars here first
  (use-package combobulate
    :straight (:host github :repo "mickeynp/combobulate")
    :hook ((python-ts-mode . combobulate-mode)
           (js-ts-mode . combobulate-mode)
           (css-ts-mode . combobulate-mode)
           (yaml-ts-mode . combobulate-mode)
           (typescript-ts-mode . combobulate-mode)
           (tsx-ts-mode . combobulate-mode))))

;; markdown
(use-package markdown-mode
  :defer t
  :ensure t
  :mode ("\\.md\\'" . gfm-mode))

;; use lang modes inside org src blocks
(use-package poly-org
  :init
  (add-to-list 'auto-mode-alist '("\\.org" . poly-org-mode))
  :defer t)

;; use lang modes inside markdow code fences
(use-package poly-markdown
  :init
  (add-to-list 'auto-mode-alist '("\\.md" . poly-gfm-mode))
  :defer t)

(use-package web-mode
  :defer t
  :mode "\\.qtpl\\'")

(use-package vue-html-mode
  :defer t)

;; vue single file component mode (uses polymode)
(use-package sfc-mode
  :straight (:host github :repo "gexplorer/sfc-mode")
  :defer t
  :custom
  (sfc-template-default-mode 'vue-html-mode)
  :mode "\\.vue\\'")

(use-package json-mode
  :defer t)
(use-package yaml-mode
  :defer t)

;; go
(use-package go-mode
  :defer t
  :custom
  (go-test-verbose t)
  (gofmt-args '("-s")))

;; like play.golang.org
(use-package go-playground
  :bind (:map go-playground-mode-map
              ([M-return] . nil)
              ("C-c C-c" . go-playground-exec))
  :defer t)

;; generates tests from funcs
(use-package go-gen-test
  :defer t)

;; rust
(use-package rust-mode
  :ensure t
  :defer t)

;; elixir, mostly for slides highlighting
(use-package elixir-mode
  :ensure t
  :defer t)

;; zig
(use-package zig-mode
  :ensure t
  :defer t)

;; nix
(use-package nix-mode
  :ensure t
  :defer t)

;; browse HN
(use-package hackernews
  :custom
  (hackernews-visited-links-file (convert-standard-filename
                                  (expand-file-name  "emacs/hackernews/visited-links.el" (xdg-cache-home))))
  :defer t)

;; org mode
(use-package org
  :defer t
;  :straight (:type built-in)
  :hook
  (org-babel-after-execute . org-redisplay-inline-images)
  (org-mode . visual-line-mode)
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
  (org-src-fontify-natively t)
  (org-fontify-whole-heading-line t)
  (org-pretty-entities t)
  (org-return-follows-link t)
  (org-src-tab-acts-natively t)
  (org-crypt-key nil "symmetric encryption")
  (org-tags-exclude-from-inheritance (quote ("crypt")))
  ;; Does not work in org-ql yet :-(
  (org-agenda-category-icon-alist
   `(("emacs" ,(list (all-the-icons-fileicon "emacs")) nil nil :ascent center)))
  ;;(org-agenda-prefix-format "○ ")
  :config
  (require 'org-crypt)
  (org-crypt-use-before-save-magic))
  ;;; (all-the-icons-insert-icons-for 'faicon) inserts all faicon icons to check

(use-package org-super-agenda
  :defer t
  :hook (org-agenda-mode . org-super-agenda-mode))

(use-package org-ql
  :defer t)

(use-package org-superstar              ; supersedes `org-bullets'
  :ensure
  :after org
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '(" ")) ;; '("🞛" "◉" "○" "▷")
  (org-superstar-leading-bullet ?\s)
  (org-superstar-leading-fallback ?\s)
  (org-hide-leading-stars nil)
  (org-superstar-prettify-item-bullets t)
  (org-superstar-special-todo-items t)
  (org-superstar-todo-bullet-alist
   '(("TODO" . 9744)
     ("[ ]"  . 9744)
     ("NEXT" . ?✒)
     ("WAITING" . ?💤)
     ("CANCELLED" . ?✘)
     ("DONE" . 9745)
     ("[X]"  . 9745)
     ("RED" . ?🔥)
     ("YELLOW" . ?🤢)
     ("UNKNOWN" . ?❓)
     ("GREEN" . ?👍)
     ("LOVE" ?🫶)))
  :hook (org-mode . org-superstar-mode))

(use-package org-fancy-priorities ; priority icons
  :after org
  :hook (org-mode . org-fancy-priorities-mode)
  :hook (org-agenda-mode . org-fancy-priorities-mode)
  :custom
  (org-fancy-priorities-list '("⚑" "⬆" "■")))

(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autoemphasis  t)
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t))

;; Avoid `org-babel-do-load-languages' since it does an eager require.
(use-package ob-C
  :straight (:type built-in)
  :defer t
  :requires (org-plus-contrib)
  :commands (org-babel-execute:C org-babel-execute:C++))
(use-package ob-ruby
  :straight (:type built-in)
  :defer t
  :requires (org-plus-contrib)
  :commands (org-babel-execute:ruby))
(use-package ob-python
  :straight (:type built-in)
  :defer t
  :requires (org-plus-contrib)
  :commands (org-babel-execute:python))
(use-package ob-octave
  :straight (:type built-in)
  :defer t
  :requires (org-plus-contrib)
  :commands (org-babel-execute:octave))
(use-package ob-gnuplot
  :straight (:type built-in)
  :defer t
  :requires (org-plus-contrib)
  :commands (org-babel-execute:gnuplot))
(use-package ob-markdown
  :straight (:type built-in)
  :defer t
  :requires (org-plus-contrib)
  :commands
    (org-babel-execute:markdown
     org-babel-expand-body:markdown))
(use-package ob-http
  :commands
  (org-babel-execute:http
   org-babel-expand-body:http))
(use-package ob-grpc
  :straight (ob-grpc :type git :host github :repo "shsms/ob-grpc")
  :defer t
  :commands
  (org-babel-execute:grpc
   org-babel-expand-body:grpc)
  :bind (:map org-mode-map
              ("C-c g i" . ob-grpc-init)
              ("C-c g b" . ob-grpc-insert-block)))
(use-package ob-shell
  :straight (:type built-in)
  :defer t
  :requires (org-plus-contrib)
  :commands
  (org-babel-execute:sh
   org-babel-expand-body:sh
   org-babel-execute:bash
   org-babel-expand-body:bash))
(use-package ob-sql
  :straight (:type built-in)
  :defer t
  :commands (org-babel-execute:sql))
(use-package ob-diagrams
  :defer t
  :requires (org-plus-contrib)
  :commands (org-babel-execute:diagrams))
(use-package ob-ditaa
  :straight (:type built-in)
  :defer t
  :requires (org-plus-contrib)
  :custom
  (org-ditaa-jar-path "/usr/share/java/ditaa.jar")
  :commands (org-babel-execute:ditaa))
(use-package ob-plantuml
  :straight (:type built-in)
  :defer t
  :requires (org-plus-contrib)
  :custom
  (org-plantuml-jar-path "/usr/share/java/plantuml.jar")
  :commands (org-babel-execute:plantuml))
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
                                       (setq-local org-image-actual-width (display-pixel-width))
                                       (org-present-big)
                                       (hide-mode-line-mode t)
                                       (display-line-numbers-mode -1)
                                       (org-display-inline-images)))
  (add-hook 'org-present-mode-quit-hook #'(lambda ()
                                       (org-present-small)
                                       (hide-mode-line-mode -1)
                                       (display-line-numbers-mode t)
                                       (org-remove-inline-images))))

;; I also tried jupyter packagebut did not work
(use-package ein
  :custom
  (ein:output-area-inlined-images t)
  :defer t)

(use-package denote
  :defer t)

(use-package consult-notes
  :commands (consult-notes
             consult-notes-search-in-all-notes))

(use-package htmlize
  :defer t)

(use-package protobuf-mode
  :defer t)

;; start services
(use-package prodigy
  :defer t)

;; TODO replace with rg
(use-package ag
  :defer t)

;; ripgrep search
(use-package rg
  :defer t)

(use-package markdown-mode
  :defer t)

;; work setup. It conflicts with the home setup because of mu4e
;; so we load one or the other
(if (file-directory-p "~/.emacs.work.d")
    (mapc 'load (file-expand-wildcards "~/.emacs.work.d/*.el")))
(if (file-directory-p "~/.emacs.home.d")
    (mapc 'load (file-expand-wildcards "~/.emacs.home.d/*.el")))

;; email
(defconst mu4e-system-path "/usr/share/emacs/site-lisp/mu4e")
(use-package mu4e
  :straight (:type built-in)
  :load-path mu4e-system-path
  :commands 'mu4e
  :defer t
  :custom
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
;; do not show trailing whitespace when rendering emails
(add-hook 'mu4e-vide-mode (lambda () (setq show-trailing-whitespace nil)))

(use-package outlook
  :after mu4e
  :init
  (require 'outlook-mu4e))

(use-package mu4e-jump-to-list
  :after mu4e
  :defer t)

(use-package mu4e-contrib
  :straight (:type built-in)
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
  :straight (:type built-in)
  :after mu4e
  :defer t
  :load-path mu4e-system-path)

(use-package mu4e-icalendar
  :straight (:type built-in)
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
  (setq mu4e-views-completion-method 'ivy)
  (setq mu4e-views-default-view-method "text")
  (mu4e-views-mu4e-use-view-msg-method "text")
  (setq mu4e-views-auto-view-selected-message nil))

(use-package mu4e-column-faces
  :after mu4e
  :config (mu4e-column-faces-mode))

;; calendar
(use-package calfw
  :disabled
  :ensure t
  :defer t)

(use-package org-web-tools
  :ensure t
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
  :commands elfeed
  :bind (("C-c 3" . elfeed)
         :map elfeed-search-mode-map
         ("R" . --elfeed-mark-all-as-read)
         ("I" . elfeed-protocol-owncloud-reinit)
         ("O" . elfeed-protocol-owncloud-update-older)
         ("S" . elfeed-protocol-owncloud-update-star)
         ("U" . elfeed-protocol-owncloud-update)
         ("f" . --elfeed-firefox-open)
         ("e" . --elfeed-eww-open))
  :init
  (use-package elfeed-protocol
    :custom
    ;; allow to use nextcloud news
    (elfeed-protocol-enabled-protocols '(owncloud))
    :ensure t)
  (advice-add 'elfeed :after #'elfeed-protocol-enable)
  :custom
  (shr-max-image-proportion 0.3)
  (elfeed-search-filter "+unread @1-week-ago")
  (elfeed-use-curl t)
  (elfeed-log-level 'debug)
  (elfeed-feeds '(("owncloud+https://dmacvicar@cloud.mac-vicar.eu"
                   :use-authinfo t)))
  :ensure t
  :defer t)


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
(add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :feed-url "pinboard\\.in"
                              :add 'saved))
;; eww
(use-package shr-tag-code-highlight
  :defer t
  :straight (:host github :repo "dmacvicar/shr-tag-code-highlight.el")
  :after shr
  :config
  (add-to-list 'shr-external-rendering-functions
               '(code . shr-tag-code-highlight)))

(add-hook 'eww-mode (lambda ()
		      (setq show-trailing-whitespace nil)))

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
                       (expand-file-name  "emacs/osm/" (xdg-cache-home))))
  :init
  ;; Load Org link support
  (with-eval-after-load 'org
    (require 'osm-ol)))

;; contacts completion
;; http://pragmaticemacs.com/emacs/tweaking-email-contact-completion-in-mu4e/
;;need this for hash access
(require 'subr-x)

;;my favourite contacts - these will be put at front of list
(setq dmacvicar/contact-file "~/.favorite-contacts.txt")

(use-package pulseaudio-control
  :defer t
  :commands pulseaudio-control-select-sink-by-name)

(use-package mastodon
  :straight (:host nil :type git :repo "https://codeberg.org/martianh/mastodon.el.git")
  :custom
  (mastodon-instance-url "https://social.mac-vicar.eu")
  (mastodon-active-user "duncan")
  :defer t)

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
