(setq package-enable-at-startup nil)

;; disable UI elements early to prevent flicker
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(require 'xdg)

(setq straight-base-dir
      (convert-standard-filename
       (expand-file-name  "emacs" (xdg-cache-home))))

(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
      (expand-file-name  "emacs/eln-cache" (xdg-cache-home)))))

