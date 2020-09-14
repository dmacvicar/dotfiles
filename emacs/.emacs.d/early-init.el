;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))
(defconst emacs-start-time (current-time))
(require 'package)
; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
; should be fixed in Emacs 26.3+
;(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq package-archives
      '(("elpa" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/"))
      package-archive-priorities
      '(("org" . 10)
        ("melpa" . 5)
        ("elpa" . 0)))
