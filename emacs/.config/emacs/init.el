;; -*- coding: utf-8; lexical-binding: t -*-

;; make startup faster by avoiding gc pauses
(setq gc-cons-threshold (* 50 1000 1000))
(defconst emacs-start-time (current-time))

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

(bind-keys*
 ("M-RET" . switch-to-buffer)
 ("C-M-j" . switch-to-buffer))

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
  :init
   (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :bind
  ("C-s" . consult-line)
  :defer t)

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

(use-package shell-pop
  :custom
  (shell-pop-universal-key "C-t")
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
  (add-hook 'go-mode-hook #'lsp-deferred)
  (add-hook 'enh-ruby-mode-hook #'lsp-deferred)
  (add-hook 'c-mode-hook #'lsp-deferred)
  (add-hook 'c++-mode-hook #'lsp-deferred)
  (add-hook 'python-mode-hook #'lsp-deferred)
  (add-hook 'java-mode-hook #'lsp-deferred)
  (add-hook 'ruby-mode-hook #'(lambda ()
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
  (lsp-completion-enable t)
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

;; faster syntax hightlighting
(use-package tree-sitter-langs
  :ensure t
  :defer t
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  ;; poly-mode breaks with tree-sitter (or viceversa?)
  ;; https://github.com/emacs-tree-sitter/elisp-tree-sitter/issues/139
  (add-hook 'tree-sitter-after-on-hook
            (lambda ()
              (tree-sitter-hl-mode (if (bound-and-true-p polymode-mode) -1 1)))))

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
  :defer t)
;; like play.golang.org
(use-package go-playground
  :defer t)
;; generates tests from funcs
(use-package go-gen-test
  :defer t)

;; browse HN
(use-package hackernews
  :defer t)

;; org mode
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
  :defer t)
(use-package org-ql
  :defer t)

(use-package org-superstar              ; supersedes `org-bullets'
  :ensure
  :after org
  :config
  (setq org-superstar-remove-leading-stars t)
  (setq org-superstar-headline-bullets-list '(" ")) ;; '("🞛" "◉" "○" "▷")
  (setq org-superstar-item-bullet-alist
        '((?+ . ?•)
          (?* . ?➤)
          (?- . ?–)))
  :hook (org-mode . org-superstar-mode))

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
  :straight (:type built-in)
  :requires (org-plus-contrib)
  :commands
  (org-babel-execute:http
   org-babel-expand-body:http))
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
  :bind (:map mu4e-compose-mode-map ("<tab>" . dmacvicar/ivy-select-and-insert-contact))
  :custom
  (smtpmail-queue-dir (expand-file-name "~/Mail/queue/cur"))
  (message-signature-file (expand-file-name "~/.signature"))
  (mu4e-compose-dont-reply-to-self t)
  (mu4e-view-use-gnus nil)
  (mu4e-view-prefer-html t)
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

;; contacts completion
;; http://pragmaticemacs.com/emacs/tweaking-email-contact-completion-in-mu4e/
;;need this for hash access
(require 'subr-x)

;;my favourite contacts - these will be put at front of list
(setq dmacvicar/contact-file "~/.favorite-contacts.txt")

(defun dmacvicar/read-contact-list ()
  "Return a list of email addresses"
  (with-temp-buffer
    (when (file-exists-p dmacvicar/contact-file)
      (insert-file-contents dmacvicar/contact-file))
    (split-string (buffer-string) "\n" t)))

;;ivy contact completion
;;based on http://kitchingroup.cheme.cmu.edu/blog/2015/03/14/A-helm-mu4e-contact-selector/
(defun dmacvicar/ivy-select-and-insert-contact (&optional start)
  (interactive)
  ;;make sure mu4e contacts list is updated - I was having
  ;;intermittent problems that this was empty but couldn't see why
  ;;(mu4e~request-contacts)
  (let ((mail-abbrev-mode-regexp mu4e~compose-address-fields-regexp)
        (eoh ;; end-of-headers
         (save-excursion
           (goto-char (point-min))
           (search-forward-regexp mail-header-separator nil t)))
        ;;append full sorted contacts list to favourites and delete duplicates
        (contacts-list
         (delq nil (delete-dups (append (dmacvicar/read-contact-list) (hash-table-keys mu4e~contacts))))))
    (when (and eoh (> eoh (point)) (mail-abbrev-in-expansion-header-p))
      (let* ((end (point))
             (start
              (or start
                  (save-excursion
                    (re-search-backward "\\(\\`\\|[\n:,]\\)[ \t]*")
                    (goto-char (match-end 0))
                    (point))))
             (contact
              (ivy-read "Contact: "
                        contacts-list
                        :re-builder #'ivy--regex
                        :sort nil
                        :initial-input (buffer-substring-no-properties start end))))
        (unless (equal contact "")
          (kill-region start end)
          (insert contact))))))

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
