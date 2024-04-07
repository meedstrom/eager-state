;;; eager-state.el --- Eagerly cache recentf and other data -*- lexical-binding: t -*-

;; Author: Martin Edström <meedstrom91@gmail.com>
;; Created: 2024-03-24
;; Version: 0.1
;; Keywords: tools
;; Homepage: https://github.com/meedstrom/eager-state
;; Package-Requires: ((emacs "25.1"))

;; Copyright (C) 2024 Martin Edström
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Ensure survival of data such as recentf, that normally rely on
;; `kill-emacs-hook'.
;;
;; It's sloppy design to put data-syncs on `kill-emacs-hook'.  Most of
;; the time my Emacs goes down, it's a crash, SIGTERM, kernel panic,
;; system freeze, segfault, yanked power cord, accidental power button
;; event, or drained battery.  I'm not sure who is the mythical user
;; who regularly types C-x C-c to bring down a fully functional Emacs.
;;
;; I'm missing some data every time I start Emacs: I can't find org
;; notes by org-id, recentf suffers partial amnesia, and so on.
;;
;; This mode fixes all that by preemptively running most of
;; `kill-emacs-hook' while the user is idle.
;;
;; --------------------------------------------------------------
;; End-user setup:
;;
;; (require 'eager-state)
;; (eager-state-preempt-kill-emacs-hook-mode)
;;
;; You may also configure `eager-state-preempt-functions': add members
;; from your own `kill-emacs-hook' that look like they would belong.
;;
;; As an optional bonus, knowing that the data is kept well-synced
;; allows you to kill/restart Emacs much faster:
;;
;; (advice-add #'kill-emacs :before (lambda () (setq kill-emacs-hook nil))

;;; Code:

(defgroup eager-state nil
  "Eagerly cache recentf and other data."
  :group 'development)

(defcustom eager-state-sync-hook nil
  "Functions to run after 3 idle minutes, or at shutdown."
  :type '(repeat sexp))

(defcustom eager-state-enable-hook nil
  "Functions to run when enabling `eager-state-mode'."
  :type '(repeat sexp))

(defcustom eager-state-disable-hook nil
  "Functions to run when disabling `eager-state-mode'."
  :type '(repeat sexp))

;; Suggestions welcome!  We want this list as big as possible.
(defcustom eager-state-preempt-functions
  '(bookmark-exit-hook-internal
    savehist-autosave
    transient-maybe-save-history
    org-roam-db--close-all
    org-clock-save
    org-id-locations-save
    org-persist-gc
    org-persist-write-all
    org-persist-clear-storage-maybe
    org-babel-remove-temporary-stable-directory
    org-babel-remove-temporary-directory
    save-place-kill-emacs-hook
    recentf-save-list
    recentf-cleanup
    doom-cleanup-project-cache-h
    doom-persist-scratch-buffers-h)
  "The subset of `kill-emacs-hook' you want to run regularly.
For `eager-state-preempt-kill-emacs-hook-mode' to call a function
regularly, that function must be in both this list and in
`kill-emacs-hook'.  It is fine to add functions that have not yet
been defined.

The net effect is that you can fill this list with functions from
packages you no longer use, and they will simply be ignored, plus
that even for the packages that do exist -- let's say org -- will
not do their sync business until you have loaded org, because
that's when org adds its business to `kill-emacs-hook'."
  :type '(repeat sexp))

(defvar eager-state--kill-emacs-hook-subset nil
  "Internal variable, do not set!
Dynamic (global) variable holding hooks to run, used by
`eager-state-update'.  Naively, that function could just let-bind
a list of hooks internally, but `run-hooks' will not work on
lexically scoped variables.")

(defun eager-state-update ()
  "Write histories and caches to disk."
  (run-hooks 'eager-state-sync-hook)
  (when eager-state-preempt-kill-emacs-hook-mode
    (setq eager-state--kill-emacs-hook-subset
          (seq-intersection kill-emacs-hook eager-state-preempt-functions))
    (run-hooks 'eager-state--kill-emacs-hook-subset)))

(defun eager-state-kill-emacs-hook ()
  "Variant of `eager-state-update'.
Since this variant is meant to run on `kill-emacs-hook', it will
not run `eager-state-preempt-functions', as that would cause many
functions to run twice."
  (run-hooks 'eager-state-sync-hook))

;;;###autoload
(define-minor-mode eager-state-preempt-kill-emacs-hook-mode
  "Start running most of `kill-emacs-hook' regularly."
  :global t
  (if eager-state-preempt-kill-emacs-hook-mode
      (when (not eager-state-mode)
        (eager-state-preempt-kill-emacs-hook-mode 0)
        (message "%s" "`eager-state-mode' not enabled, so not enabling `eager-state-preempt-kill-emacs-hook-mode'"))))

(defvar eager-state-timer (timer-create)
  "The timer used by `eager-state-mode'.")

(define-minor-mode eager-state-mode
  "Regularly write caches to disk."
  :global t
  :require 'eager-state
  (cancel-timer eager-state-timer)
  (if eager-state-mode
      (progn
        (setq eager-state-timer
              (run-with-idle-timer (* 3 60) t #'eager-state-update))
        (add-hook 'kill-emacs-hook #'eager-state-kill-emacs-hook)
        (run-hooks 'eager-state-enable-hook))
    (remove-hook 'kill-emacs-hook #'eager-state-kill-emacs-hook)
    (run-hooks 'eager-state-disable-hook)))

(defvar eager-state-prevent nil
  "Prevent auto-enabling `eager-state-mode' at load time.")

;; NOTE: It's a code-smell to enable a mode right upon loading the file,
;; but necessary because it's meant as a library for other packages, none
;; of which should have to check that the mode is on.  They should be able
;; to just write:
;;
;;    (require 'eager-state)
;;    (add-hook 'eager-state-sync-hook #'your-persist-function)
;;
;; and be done with it.  Plus, if the end-user turned `eager-state-mode' off,
;; that choice should be respected, so it would be inappropriate to expect
;; every package to turn it on for us.
;;
;; Anyway, since we have a nil default value for `eager-state-sync-hook',
;; the mode actually does nothing until some other mode you enabled adds
;; something there.
(unless eager-state-prevent
  (eager-state-mode)
  ;; Run only the first time the file is loaded
  (setq eager-state-prevent t))

(provide 'eager-state)
;;; eager-state.el ends here
