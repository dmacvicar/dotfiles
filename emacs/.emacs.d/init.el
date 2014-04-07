;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Base
;;
(require 'cl)
(require 'package)
;;(add-to-list 'package-archives
;;             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar required-packages
  '(flycheck
    flycheck-pos-tip
    powerline
    tabbar-ruler
    linum
    highlight-indentation
    nav
    projectile
    flx-ido
    ido-vertical-mode
    less-css-mode
    xclip
    multi-term)
  "A list of packages to ensure are installed at launch.")

(defun required-packages-installed-p ()
  "Check if all packages in `required-packages' are installed."
  (every #'package-installed-p required-packages))

(defun require-package (package)
  "Install PACKAGE unless already installed."
  (unless (memq package required-packages)
    (add-to-list 'required-packages package))
  (unless (package-installed-p package)
    (package-install package)))

(defun require-packages (packages)
  "Ensure PACKAGES are installed.
Missing packages are installed automatically."
  (mapc #'require-package packages))

(defun install-required-packages ()
  "Install all packages listed in `required-packages'."
  (unless (required-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Emacs is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (require-packages required-packages)))

;; run package installation
(install-required-packages)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance
;;
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
; For now I am setting the Mono font in gnome, so this
; is not needed
;(set-face-attribute 'default nil :font "Consolas:style=Regular")
(set-default-font "DejaVu Sans Mono")
;(set-face-attribute 'default nil :height 100)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(menu-bar-mode -1)
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
(require 'highlight-indentation)
(setq highlight-indentation-offset 2)
;(set-face-background 'highlight-indentation-face "#e3e3d3")
;(set-face-background 'highlight-indentation-current-column-face "#c3b3b3")

; line numbers
(add-hook 'find-file-hook (lambda () (linum-mode 1)))
(linum-mode t)
(setq linum-format " %d ")

; improved bar at the botton
(require 'powerline)
(powerline-default-theme)

(load-theme 'zenburn t)
(setq show-trailing-whitespace t)
(setq-default show-trailing-whitespace t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Behavior
;;

;; X11 keyboard in console
(xclip-mode 1)
; enable projectile globally
(projectile-global-mode)
;; annoying scrolling
;(setq scroll-step 1)
;(setq scroll-conservatively 10000)
;(setq auto-window-vscroll nil)
;(setq scroll-margin 1
;scroll-conservatively 0
;scroll-up-aggressively 0.01
;scroll-down-aggressively 0.01)

;; FIX Scrolling
;; from
;; http://web.archive.org/web/20061025212623/http://www.cs.utexas.edu/users/hllu/EmacsSmoothScrolling.html
;; only thing that works
(setq truncate-lines t)

(defun point-of-beginning-of-bottom-line ()
  (save-excursion
    (move-to-window-line -1)
    (point)))

(defun point-of-beginning-of-line ()
  (save-excursion
    (beginning-of-line)
    (point)))

(defun next-one-line () (interactive)
  (if (= (point-of-beginning-of-bottom-line) (point-of-beginning-of-line))
      (progn (scroll-up 1)
             (next-line 1))
    (next-line 1)))

(defun point-of-beginning-of-top-line ()
  (save-excursion
    (move-to-window-line 0)
    (point)))

(defun previous-one-line () (interactive)
  (if (= (point-of-beginning-of-top-line) (point-of-beginning-of-line))
      (progn (scroll-down 1)
             (previous-line 1))
    (previous-line 1)))

(global-set-key (kbd "<down>") 'next-one-line)
(global-set-key (kbd "<up>") 'previous-one-line)

;;;;;;;;;;;;;;
(flycheck-mode)
(setq flycheck-highlighting-mode 'lines)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-check-syntax-automatically '(mode-enabled save idle-change))
(eval-after-load 'flycheck
  '(custom-set-variables
   '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

;(require 'ido-vertical-mode)
(require 'flx-ido)
(ido-mode 1)
;(ido-vertical-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

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
(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.saves"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups


(defun window-width-to-font-size (window-width)
  ;; Insert a calculation to turn window width into 79 chars.
)

;;(setq prelude-clean-whitespace-on-save nil)

(global-set-key (kbd "M-RET") 'electric-buffer-list)
(add-hook 'Buffer-menu-mode-hook 'buffer-disable-undo)
;; (global-set-key [(meta return)] 'electric-buffer-list)
(cua-mode 1)
(setq shift-select-mode nil)
(setq x-alt-keysym 'meta)

(require 'nav)
(nav-disable-overeager-window-splitting)

; highlight the current line
(global-hl-line-mode t)

; tabs
(setq tabbar-ruler-global-tabbar t) ; If you want tabbar
;(setq tabbar-ruler-global-ruler t) ; if you want a global ruler
;(setq tabbar-ruler-popup-menu t) ; If you want a popup menu.
;(setq tabbar-ruler-popup-toolbar t) ; If you want a popup toolbar
;(setq tabbar-ruler-popup-scrollbar t) ; If you want to only show the
(require 'tabbar-ruler)
(tabbar-ruler-group-by-projectile-project)

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; only open the speedbar by default in X11
;;(when window-system
;;   (sr-speedbar-open))

;; language specific tweaks

;;; Colourise CSS colour literals
(when (eval-when-compile (>= emacs-major-version 24))
  ;; rainbow-mode needs color.el, bundled with Emacs >= 24.
  (require-package 'rainbow-mode)
  (dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook))
    (add-hook hook 'rainbow-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specific languages

(defun my-c-mode-hook ()
  (setq c-basic-offset 4)
  (c-set-offset 'substatement-open 0))
(add-hook 'c-mode-common-hook 'my-c-mode-hook)
