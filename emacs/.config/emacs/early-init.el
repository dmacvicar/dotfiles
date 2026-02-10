(setq package-enable-at-startup nil)

;; skip regex matching on file operations during init
(defvar duncan/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
  (lambda () (setq file-name-handler-alist duncan/file-name-handler-alist)))

;; disable UI elements early to prevent flicker
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq frame-inhibit-implied-resize t)

(require 'xdg)

(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
      (expand-file-name  "emacs/eln-cache" (xdg-cache-home)))))

