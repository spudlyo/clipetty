;;; clipetty-checkdoc.el --- Clipetty: Checkdoc runner  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Sebastian Wiesner and Flycheck contributors

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;; Version: 0.1
;; URL: https://github.com/spudlyo/clipetty
;; Package-Requires: ((emacs "25.1"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides checkdoc linting for Clipetty.  It's intended for
;; non-interactive use, see "run-tests.sh".
;;
;; This code originally came from Flycheck, modified for use in Clipetty.

;;; Code:

(unless (version<= "25" emacs-version)
  (user-error "Emacs 25 required for checkdoc"))

(require 'seq)
(require 'subr-x)
(require 'checkdoc)

(defun clipetty-checkdoc-get-current-errors ()
  "Get the current checkdoc errors.

Return a list of all error messages from checkdoc, and erase the
error message buffer, so that the next checkdoc check starts
fresh without previous errors.

Each error is just a string with the complete human-readable
location and error message."
  (with-current-buffer checkdoc-diagnostic-buffer
    (unwind-protect
        (progn
          (goto-char (point-min))
          ;; Skip over the checkdoc header
          (re-search-forward (rx line-start "***" (1+ not-newline)
                                 ": checkdoc-current-buffer"))
          (forward-line 1)
          (let ((text (buffer-substring-no-properties (point) (point-max))))
            (and (not (string-empty-p text))
                 (split-string text "\n"))))
      (kill-buffer))))

(defun clipetty-checkdoc-file (filename)
  "Run checkdoc on FILENAME and return a list of errors.

Each error is just a string with the complete human-readable
location and error message."
  (load-file filename)
  (with-temp-buffer
    ;; Visit the file to make sure that the filename is set, as some checkdoc
    ;; lints only apply for buffers with filenames
    (insert-file-contents filename 'visit)
    (set-buffer-modified-p nil)
    ;; Switch to Emacs Lisp mode to give checkdoc the proper syntax table, etc.
    (delay-mode-hooks (emacs-lisp-mode))
    (setq delay-mode-hooks nil)
    (let ((checkdoc-arguments-in-order-flag nil))
      (checkdoc-current-buffer 'take-notes))
    (clipetty-checkdoc-get-current-errors)))

(defun clipetty-checkdoc-batch ()
  "Run checkdoc on FILE and exit."
  (unless noninteractive
    (error "`clipetty-checkdoc-batch' is to be used only with -batch"))
  (let ((errors (seq-mapcat #'clipetty-checkdoc-file
                            command-line-args-left)))
    (seq-do (lambda (err) (message "%s" err)) errors)
    (kill-emacs (if errors 1 0))))

(provide 'clipetty-checkdoc)
;;; clipetty-checkdoc.el ends here
