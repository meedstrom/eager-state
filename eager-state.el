;;; eager-state.el --- Pre-empt kill-emacs-hook with a crash-only design  -*- lexical-binding: t -*-

;; Copyright (C) 2024-2025 Martin Edström

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; Author: Martin Edström <meedstrom91@gmail.com>
;; Created: 2024-03-24
;; Version: 0.2.0
;; Keywords: convenience
;; Homepage: https://github.com/meedstrom/eager-state
;; Package-Requires: ((emacs "29.1") (llama "0.5.0"))

;;; Commentary:

;; For justification, please see file README.org, or equivalently,
;; Info node "(eager-state)".

;; An example setup looks like:

;;     (require 'eager-state)
;;     (setq eager-state-kill-emacs-hook-subset
;;           '(bookmark-exit-hook-internal
;;             savehist-autosave
;;             transient-maybe-save-history
;;             org-clock-save
;;             org-id-locations-save
;;             save-place-kill-emacs-hook
;;             recentf-save-list
;;             recentf-cleanup
;;             doom-cleanup-project-cache-h
;;             doom-persist-scratch-buffers-h))

;; Note how the above list is cribbed from real-world values of
;; `kill-emacs-hook'.

;; The `require' statement starts a timer that periodically tries to run
;; the above listed functions.

;; You can add any symbol to the list, even if it is not yet defined as a
;; function.  A given symbol will only be funcalled if it is present both in
;; that list, and in `kill-emacs-hook'.

;; To add new functions that you don't want on `kill-emacs-hook', add
;; them to `eager-state-sync-hook' instead.

;;; Code:

(require 'cl-lib)
(require 'llama)
(require 'benchmark)

(defgroup eager-state nil
  "Eagerly persist data onto disk."
  :group 'convenience
  :link '(url-link :tag "GitHub source" "https://github.com/meedstrom/eager-state")
  :link '(info-link :tag "Info manual" "(eager-state)"))

(defcustom eager-state-inhibit nil
  "If t, do not auto-enable `eager-state-mode' at load time.
Must be set before loading eager-state.el."
  :type 'boolean)

(defcustom eager-state-kill-emacs-hook-subset nil
  "Subset of `kill-emacs-hook' to pre-empt.
These symbols need not be known to be defined,
and are only funcalled if also present in `kill-emacs-hook'."
  :type 'hook
  :options '(bookmark-exit-hook-internal
             doom-cleanup-project-cache-h
             doom-persist-scratch-buffers-h
             org-babel-remove-temporary-directory
             org-babel-remove-temporary-stable-directory
             org-clock-save
             org-id-locations-save
             org-persist-clear-storage-maybe
             org-persist-gc
             org-persist-write-all
             recentf-cleanup
             recentf-save-list
             save-place-kill-emacs-hook
             savehist-autosave
             tramp-dump-connection-properties
             transient-maybe-save-history))

(defcustom eager-state-kill-emacs-query-functions-subset nil
  "Subset of `kill-emacs-query-functions' to pre-empt.
These symbols need not be known to be defined,
and are only funcalled if also present in `kill-emacs-query-functions'."
  :type 'hook
  :options '(desktop-kill
             eshell-save-some-history
             eshell-save-some-last-dir))

(defcustom eager-state-faster-shutdown nil
  "If t, do not run any function in the subset lists at Emacs shutdown.

In other words, all members of
- `eager-state-kill-emacs-query-functions-subset' and
- `eager-state-kill-emacs-hook-subset'
are removed from the corresponding hooks before those hooks run.

Also removed from `kill-emacs-hook' are all functions that happen to be
members of `eager-state-sync-hook'."
  :type 'boolean)

(defcustom eager-state-sync-hook nil
  "Functions called intermittently to sync data to disk."
  :type 'hook)

(defcustom eager-state-idle-delay 15
  "Be idle at least this long before actually syncing.
An idle timer of this duration starts every `eager-state-periodic-delay'.

If this value exceeds `eager-state-periodic-delay', Eager-State will
only use a repeating idle timer, and no periodic timer at all."
  :type 'number)

(defcustom eager-state-periodic-delay 60
  "Minimum delay between attempts to sync, in seconds.
Set to 0 if you want to only use `eager-state-idle-delay'."
  :type 'number)

(defvar eager-state--periodic-timer (timer-create))
(defvar eager-state--idle-timer (timer-create))
(defun eager-state--check ()
  "Adjust timers and maybe call `eager-state--sync'."
  (let ((idle (float-time (or (current-idle-time) 0))))
    (if (>= eager-state-idle-delay
            eager-state-periodic-delay)
        ;; If user set `eager-state-idle-delay' higher than
        ;; `eager-state-periodic-delay', switch to using idle timer only.
        (progn
          (cancel-timer eager-state--periodic-timer)
          (if (and (member eager-state--idle-timer timer-idle-list)
                   (eq (round (timer--time eager-state--idle-timer))
                       (round eager-state-idle-delay)))
              (when (> idle eager-state-idle-delay)
                (eager-state--sync))
            (cancel-timer eager-state--idle-timer)
            (setq eager-state--idle-timer
                  (run-with-idle-timer eager-state-idle-delay
                                       t
                                       #'eager-state--check))))
      (cancel-timer eager-state--idle-timer)
      (if (and (member eager-state--periodic-timer timer-list)
               (eq (round (timer--repeat-delay eager-state--periodic-timer))
                   (round eager-state-periodic-delay)))
          (if (> idle eager-state-periodic-delay)
              ;; Don't repeatedly sync during a long idle
              nil
            (setq eager-state--idle-timer
                  (run-with-idle-timer eager-state-idle-delay
                                       nil
                                       #'eager-state--sync)))
        (cancel-timer eager-state--periodic-timer)
        (setq eager-state--periodic-timer
              (run-with-timer eager-state-periodic-delay
                              eager-state-periodic-delay
                              #'eager-state--check))))))

(defun eager-state--sync ()
  "Run relevant hooks silently, and profile time elapsed for each one."
  (let ((functions
         (append (seq-intersection kill-emacs-query-functions
                                   eager-state-kill-emacs-query-functions-subset)
                 (seq-intersection kill-emacs-hook
                                   eager-state-kill-emacs-hook-subset)
                 eager-state-sync-hook))
        (save-silently t)
        (inhibit-message t)
        (standard-output #'ignore)
        (buf (get-buffer-create " *eager-state*")))
    (message "eager-state: Syncing...")
    (cl-letf (((symbol-function 'write-region)
               (let ((real-write-region (symbol-function 'write-region)))
                 (##apply real-write-region %1 %2 %3 &4 (or &5 'quiet) &*))))
      (with-current-buffer buf
        (goto-char (point-max))
        (insert (format-time-string "\n%F %T: Starting...\n"))
        (dolist (func functions)
          (insert (if (symbolp func) (symbol-name func) "anonymous lambda")
                  "...")
          (let ((elapsed (benchmark-elapse (funcall func))))
            (cl-assert (eq buf (current-buffer)))
            (insert (format " took %.2fs" elapsed)
                    "\n")))))
    (message "eager-state: Syncing...done")))

(defun eager-state--trim-kill-emacs-hook (&rest _)
  "Maybe delete members of `kill-emacs-hook'."
  (when eager-state-faster-shutdown
    (dolist (func (append eager-state-sync-hook
                          eager-state-kill-emacs-hook-subset))
      (setq kill-emacs-hook (delete func kill-emacs-hook)))))

(defun eager-state--trim-kill-emacs-query-functions (&rest _)
  "Maybe delete members of `kill-emacs-query-functions'."
  (when eager-state-faster-shutdown
    (dolist (func eager-state-kill-emacs-query-functions-subset)
      (setq kill-emacs-query-functions (delete func kill-emacs-query-functions)))))

;;;###autoload
(define-minor-mode eager-state-mode
  "Regularly write various kinds of persistable cache to disk.

Users can add to `eager-state-kill-emacs-hook-subset' any members of
`kill-emacs-hook' that they want to run preemptively every now and then.
Typical members relate to Save-Place, Recentf and similar.

Package developers can add to `eager-state-sync-hook' what they would
normally add to `kill-emacs-hook'.  That avoids needing to define new
timers and timer intervals, unifying all that under the user options
`eager-state-periodic-delay' and `eager-state-idle-delay'.

Package developers should not enable this mode.  Simply `require'
the library, and it is turned on by default unless the user changes
that, which should be respected.

-----"
  :require 'eager-state
  :global t
  (if eager-state-mode
      (progn
        (advice-add #'kill-emacs :before #'eager-state--trim-kill-emacs-hook)
        (advice-add #'save-buffers-kill-emacs :before #'eager-state--trim-kill-emacs-query-functions)
        (cancel-timer eager-state--periodic-timer)
        (cancel-timer eager-state--idle-timer)
        (eager-state--check))
    (advice-remove #'kill-emacs #'eager-state--trim-kill-emacs-hook)
    (advice-remove #'save-buffers-kill-emacs #'eager-state--trim-kill-emacs-query-functions)
    (cancel-timer eager-state--periodic-timer)
    (cancel-timer eager-state--idle-timer)))

(unless eager-state-inhibit
  (eager-state-mode))

(provide 'eager-state)
;;; eager-state.el ends here
