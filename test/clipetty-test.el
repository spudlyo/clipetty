;;; clipetty-test.el --- Unit tests for Clipetty

;; Copyright (C) 2019-2020 Mike Hamrick

;; Author: Mike Hamrick <mikeh@muppetlabs.com>
;; Maintainer: Mike Hamrick <mikeh@muppetlabs.com>
;; Created: 27 Dec 2019
;; Modified: 27 Dec 2020
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/spudlyo/clipetty

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file contains Clipetty unit tests.

;;; Code:

(require 'ert)

(ert-deftest clipetty-test-clipetty--get-tmux-ssh-tty ()
  "Test the `clipetty--get-tmux-ssh-tty' function."
  (let ((clipetty-tmux-ssh-tty "not-a-valid-command"))
    (should (equal (clipetty--get-tmux-ssh-tty) nil)))
  (let ((clipetty-tmux-ssh-tty "echo SSH_TTY=foo"))
    (should (equal (clipetty--get-tmux-ssh-tty) "foo")))
  (let ((clipetty-tmux-ssh-tty "echo NOPE_TTY=foo"))
  (should (equal (clipetty--get-tmux-ssh-tty) nil)))
  (let ((clipetty-tmux-ssh-tty "echo GOOB=bar")
        (clipetty-tmux-ssh-tty-regexp "GOOB=\\([^\n]+\\)"))
    (should (equal (clipetty--get-tmux-ssh-tty) "bar"))))

(ert-deftest clipetty-test-clipetty--tty ()
  "Test the `clipetty--tty' function."
  (should (equal (clipetty--tty nil nil) (terminal-name)))
  (let ((clipetty-tmux-ssh-tty "echo SSH_TTY=from-tmux"))
    (should (equal (clipetty--tty "/dev/tty" t) "from-tmux")))
  (let ((clipetty-tmux-ssh-tty "echo NOPE"))
    (should (equal (clipetty--tty "from-ssh-tty" t) "from-ssh-tty")))
  (should (equal (clipetty--tty "from-ssh-tty" nil) "from-ssh-tty")))

(ert-deftest clipetty-test-clipetty--make-dcs ()
  "Test the `clipetty--make-dcs' function."
  (let ((tmux-dcs   (concat clipetty--tmux-dcs-start "foo" clipetty--dcs-end))
        (screen-dcs (concat clipetty--screen-dcs-start "foo" clipetty--dcs-end)))
    (should (equal (clipetty--make-dcs "foo") tmux-dcs))
    (should (equal (clipetty--make-dcs "foo" nil) tmux-dcs))
    (should (equal (clipetty--make-dcs "foo" t) screen-dcs))))

(ert-deftest clipetty-test-clipetty--dcs-wrap ()
  "Test the `clipetty--dcs-wrap' function."
  (let ((tmux-dcs   (concat clipetty--tmux-dcs-start "foo" clipetty--dcs-end))
        (screen-dcs (concat clipetty--screen-dcs-start "foo" clipetty--dcs-end)))
    ;; no screen or tmux indicators
    (should (equal (clipetty--dcs-wrap "foo" nil nil nil) "foo"))
    (should (equal (clipetty--dcs-wrap "foo" nil nil t) "foo"))
    (let ((clipetty-assume-nested-mux t))
      (should (equal (clipetty--dcs-wrap "foo" nil nil t) "foo")))
    ;; tmux indicated
    (should (equal (clipetty--dcs-wrap "foo" t nil nil) tmux-dcs))
    (should (equal (clipetty--dcs-wrap "foo" t nil t) "foo"))
    (let ((clipetty-assume-nested-mux t))
      (should (equal (clipetty--dcs-wrap "foo" t nil t) tmux-dcs)))
    ;; screen indicated
    (let ((clipetty-screen-regexp "screen"))
      (should (equal (clipetty--dcs-wrap "foo" nil "xscreen" nil) screen-dcs)))
    (should (equal (clipetty--dcs-wrap "foo" nil "xscreen" nil) "foo"))
    (should (equal (clipetty--dcs-wrap "foo" nil "screen" nil) screen-dcs))
    (should (equal (clipetty--dcs-wrap "foo" nil "screen" t) "foo"))
    (let ((clipetty-assume-nested-mux t))
      (should (equal (clipetty--dcs-wrap "foo" nil "screen" t) screen-dcs)))))

(ert-deftest clipetty-test-clipetty--osc ()
  "Test the `clipetty--osc' function."
  (let* ((bin      (base64-encode-string (encode-coding-string "foo" 'binary)))
         (osc-bin  (concat clipetty--osc-start bin clipetty--osc-end))
         (osc-foo  (concat clipetty--osc-start "foo" clipetty--osc-end)))
    (should (equal (clipetty--osc "foo") osc-foo))
    (should (equal (clipetty--osc "foo" t) osc-bin))))

(ert-deftest clipetty-test-clipetty-mode ()
  "Test the `clipetty-mode' function."
  (let ((old-icf interprogram-cut-function))
    (should (and (clipetty-mode)
                 (equal interprogram-cut-function #'clipetty-cut)))
    (should (and (clipetty-mode 1)
                 (equal interprogram-cut-function #'clipetty-cut)))
    (should (and (clipetty-mode +1)
                 (equal interprogram-cut-function #'clipetty-cut)))
    (should (and (not (clipetty-mode 0))
                 (equal interprogram-cut-function old-icf)))
    (should (and (not (clipetty-mode -1))
                 (equal interprogram-cut-function old-icf)))))

;;; Checkdoc test

;; This code originally came from Flycheck, modified for use in Clipetty.
;; Copyright (C) 2016  Sebastian Wiesner and Flycheck contributors

(unless (version<= "25" emacs-version)
  (user-error "Emacs 25 required for checkdoc"))

(require 'seq)
(require 'subr-x)
(require 'checkdoc)

(defun clipetty-test-checkdoc-get-current-errors ()
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

(defun clipetty-test-checkdoc-file (filename)
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
    (clipetty-test-checkdoc-get-current-errors)))

(defun clipetty-test-checkdoc-batch ()
  "Run checkdoc on FILE and exit."
  (unless noninteractive
    (error "`clipetty-test-checkdoc-batch' is to be used only with -batch"))
  (let ((errors (seq-mapcat #'clipetty-test-checkdoc-file
                            command-line-args-left)))
    (seq-do (lambda (err) (message "%s" err)) errors)
    (kill-emacs (if errors 1 0))))

(provide 'clipetty-test)
;;; clipetty-test.el ends here
