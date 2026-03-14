;;; ical-vdir-diary.el --- Generate diary files from local calendar caches -*- lexical-binding: t; -*-

;; Author: duncan

;;; Commentary:

;; Support both the older vdirsyncer cache layout and the newer pimsync
;; layout. Prefer pimsync when both are present.

;;; Code:

(declare-function xdg-cache-home "ext:xdg")

(require 'icalendar)
(require 'seq)
(require 'subr-x)

(defun duncan/icalendar-normalize-recurring-overrides ()
  "Add EXDATEs for RECURRENCE-ID overrides and drop cancelled overrides.

This avoids duplicate diary entries when a recurring master VEVENT does
not include EXDATEs for its overridden instances."
  (let ((overrides-by-uid (make-hash-table :test 'equal))
        (cancelled-overrides nil)
        (masters nil))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "BEGIN:VEVENT" nil t)
        (let ((start (match-beginning 0)))
          (when (search-forward "END:VEVENT" nil t)
            (let* ((end (match-end 0))
                   (start-marker (copy-marker start))
                   (end-marker (copy-marker end t))
                   uid recurrence-id-line has-rrule cancelled)
              (save-excursion
                (goto-char start)
                (while (re-search-forward "^[A-Z-]+\\(?:;[^:]*\\)?:.*$" end t)
                  (let ((line (buffer-substring-no-properties
                               (line-beginning-position)
                               (line-end-position))))
                    (cond
                     ((string-prefix-p "UID:" line)
                      (setq uid (substring line 4)))
                     ((string-prefix-p "RECURRENCE-ID" line)
                      (setq recurrence-id-line line))
                     ((string-prefix-p "RRULE:" line)
                      (setq has-rrule t))
                     ((string-prefix-p "STATUS:CANCELLED" line)
                      (setq cancelled t))))))
              (when (and uid recurrence-id-line)
                (let ((exdate-line
                       (replace-regexp-in-string "^RECURRENCE-ID" "EXDATE"
                                                 recurrence-id-line)))
                  (puthash uid
                           (cons exdate-line (gethash uid overrides-by-uid))
                           overrides-by-uid)
                  (when cancelled
                    (push (cons start-marker end-marker) cancelled-overrides))))
              (when (and uid has-rrule)
                (push (list uid start-marker end-marker) masters)))))))
    (dolist (master masters)
      (let* ((uid (nth 0 master))
             (start-marker (nth 1 master))
             (end-marker (nth 2 master))
             (exdates (delete-dups (gethash uid overrides-by-uid)))
             (existing-exdates nil))
        (when exdates
          (save-excursion
            (goto-char start-marker)
            (while (re-search-forward "^EXDATE.*$" end-marker t)
              (push (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))
                    existing-exdates)))
          (let ((missing nil))
            (dolist (line exdates)
              (unless (member line existing-exdates)
                (push line missing)))
            (when missing
              (save-excursion
                (goto-char end-marker)
                (beginning-of-line)
                (insert (mapconcat #'identity (nreverse missing) "\n") "\n")))))))
    (dolist (entry cancelled-overrides)
      (delete-region (car entry) (cdr entry)))))

(defun duncan/calendar-sync-directory ()
  "Return the active calendar sync directory.

Prefer pimsync when its calendar cache exists, otherwise fall back to
the older vdirsyncer cache layout."
  (let ((pimsync-directory (expand-file-name "pimsync/calendars/" (xdg-cache-home)))
        (vdirsyncer-directory (expand-file-name "vdirsyncer/calendars/" (xdg-cache-home))))
    (cond
     ((file-directory-p pimsync-directory) pimsync-directory)
     ((file-directory-p vdirsyncer-directory) vdirsyncer-directory)
     (t pimsync-directory))))

(defun duncan/calendar-import-diary-file (diary-file-path)
  "Escape diary directives in DIARY-FILE-PATH after ical import."
  (with-temp-buffer
    (insert-file-contents diary-file-path)
    (goto-char (point-min))
    (while (not (eobp))
      (let ((line-start (point))
            (line-end (line-end-position)))
        ;; Escape percent-encoded URLs so diary formatting doesn't treat them as directives.
        (unless (looking-at-p "&%%")
          (let ((line (buffer-substring line-start line-end)))
            (delete-region line-start line-end)
            (insert (replace-regexp-in-string "%" "%%" line))))
        (forward-line 1)))
    (write-region (point-min) (point-max) diary-file-path nil 'silent)))

(defun duncan/import-ics-buffer-to-diary (diary-file-path)
  "Import the current ICS buffer to DIARY-FILE-PATH."
  (duncan/icalendar-normalize-recurring-overrides)
  (when (file-exists-p diary-file-path)
    (message "deleting old diary file")
    (delete-file diary-file-path t)
    (with-temp-buffer (write-file diary-file-path)))
  (icalendar-import-buffer diary-file-path t t)
  (duncan/calendar-import-diary-file diary-file-path))

(defun duncan/import-flat-ics-calendar (ics-file-path diary-file-path)
  "Import a single flat ICS-FILE-PATH to DIARY-FILE-PATH."
  (message "importing %s" ics-file-path)
  (with-temp-buffer
    (insert-file-contents ics-file-path)
    (duncan/import-ics-buffer-to-diary diary-file-path)))

(defun duncan/import-pimsync-calendar-directory (calendar-directory diary-file-path)
  "Import all ICS files in CALENDAR-DIRECTORY to DIARY-FILE-PATH."
  (let ((ics-files (directory-files calendar-directory t "\\.ics$")))
    (when ics-files
      (message "importing %s (%d events)"
               (file-name-nondirectory (directory-file-name calendar-directory))
               (length ics-files))
      (with-temp-buffer
        (insert "BEGIN:VCALENDAR\nVERSION:2.0\nPRODID:-//duncan//pimsync calendar export//EN\n")
        (dolist (ics-file-path ics-files)
          (let ((ics-contents
                 (with-temp-buffer
                   (insert-file-contents ics-file-path)
                   (buffer-string))))
            ;; pimsync stores one VCALENDAR-wrapped item per file; strip the wrapper
            ;; so the combined buffer remains a single valid VCALENDAR.
            (setq ics-contents
                  (replace-regexp-in-string
                   "\\`BEGIN:VCALENDAR\r?\n" "" ics-contents))
            (setq ics-contents
                  (replace-regexp-in-string
                   "\r?\nEND:VCALENDAR\r?\n?\\'" "" ics-contents))
            (insert ics-contents)
            (unless (bolp)
              (insert "\n"))))
        (insert "END:VCALENDAR\n")
        (duncan/import-ics-buffer-to-diary diary-file-path)))))

(defun duncan/generate-diary-from-calendars ()
  "Generate diary files and calendars-diary from .ics files."
  (interactive)
  (let* ((ics-directory (duncan/calendar-sync-directory))
         (calendars-diary-directory (expand-file-name "emacs/diary/calendars/" (xdg-cache-home)))
         (calendars-diary-file diary-file)
         (calendar-date-style 'european))
    (if (file-directory-p ics-directory)
        (progn
          (unless (file-exists-p calendars-diary-directory)
            (make-directory calendars-diary-directory t))
          (with-temp-file calendars-diary-file
            (dolist (entry (directory-files ics-directory t "^[^.].*"))
              (cond
               ((and (file-regular-p entry) (string-match-p "\\.ics\\'" entry))
                (let* ((diary-file-name (file-name-base entry))
                       (diary-file-path (concat calendars-diary-directory diary-file-name ".ics")))
                  (duncan/import-flat-ics-calendar entry diary-file-path)
                  (insert (format "#include \"%s\"\n" diary-file-path))))
               ((file-directory-p entry)
                (let* ((calendar-id (file-name-nondirectory (directory-file-name entry)))
                       (displayname-file (expand-file-name "displayname" entry))
                       (calendar-name
                        (if (file-exists-p displayname-file)
                            (string-trim
                             (with-temp-buffer
                               (insert-file-contents displayname-file)
                               (buffer-string)))
                          calendar-id))
                       (diary-file-name
                        (replace-regexp-in-string "[^[:alnum:]_.-]+" "_" calendar-name))
                       (diary-file-path (concat calendars-diary-directory diary-file-name ".ics")))
                  (when (directory-files entry nil "\\.ics$")
                    (duncan/import-pimsync-calendar-directory entry diary-file-path)
                    (insert (format "#include \"%s\"\n" diary-file-path)))))))))
      (message "No calendar sync directory found at %s or %s; skipping diary generation"
               (expand-file-name "pimsync/calendars/" (xdg-cache-home))
               (expand-file-name "vdirsyncer/calendars/" (xdg-cache-home))))))

(provide 'ical-vdir-diary)

;;; ical-vdir-diary.el ends here
