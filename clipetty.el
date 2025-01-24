;;; clipetty.el --- Send every kill from a TTY frame to the system clipboard -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Mike Hamrick

;; Author: Mike Hamrick <mikeh@muppetlabs.com>
;; Maintainer: Mike Hamrick <mikeh@muppetlabs.com>
;; Created: 25 Dec 2019
;; Modified: 04 Jan 2020
;; Version: 0.0.2
;; Package-Requires: ((emacs "25.1"))
;; Keywords: terminals convenience
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

;; Sends ANSI "Operating System Command" (OSC) 52 escape sequences,
;; to manipulate the Operating System's Clipboard from an Emacs TTY
;; frame.  For use with the `interprogram-cut-function' variable.

;; Please see README.org from the same repository for documentation.

;;; Code:

(defgroup clipetty nil
  "Send every kill from a TTY frame to the system clipboard."
  :group 'terminals
  :group 'convenience
  :link '(emacs-commentary-link :tag "Commentary" "clipetty.el")
  :link '(emacs-library-link :tag "Lisp File" "clipetty.el"))

(defconst clipetty--max-cut 320000
  "The maximum length of a string you can send to the clipboard via OSC52.")

(defconst clipetty--osc-start "\e]52;c;"
  "The initial OSC 52 escape sequence.")

(defconst clipetty--osc-end "\a"
  "The end OSC 52 escape sequence.")

(defun clipetty--tty ()
  "Return which TTY we should send our OSC payload to."
  (terminal-name))

(defun clipetty--osc (string)
  "Return an OSC 52 escape sequence out of STRING."
  (let ((bin (base64-encode-string (encode-coding-string string 'binary) t)))
    (concat clipetty--osc-start bin clipetty--osc-end)))

(defun clipetty--emit (string)
  "Emit STRING to an appropriate tty."
  (message (int-to-string (length string)))
  (if (<= (length string) clipetty--max-cut)
      (write-region
       string
       nil
       (clipetty--tty)
       t
       0)
    (message "Selection too long to send to terminal %d" (length string))
    (sit-for 1)))

(defun clipetty-cut (orig-fun string)
  "If in a terminal frame, convert STRING to a series of OSC 52 messages.
Since this is intended to be used with `add-function', ORIG-FUN is
the original `interprogram-cut-function' that we're advising."
  (unless (display-graphic-p)
    (clipetty--emit (clipetty--osc string)))
  ;; Always chain to the original cut function.
  (funcall orig-fun string))

;;;###autoload
(define-minor-mode clipetty-mode
  "Minor mode to send every kill from a TTY frame to the system clipboard."
  :lighter " Clp"
  :init-value nil
  :global nil
  (if clipetty-mode
      (add-function :around (local 'interprogram-cut-function) #'clipetty-cut)
    (remove-function (local 'interprogram-cut-function) #'clipetty-cut)))

;;;###autoload
(define-globalized-minor-mode global-clipetty-mode
  clipetty-mode
  (lambda () (clipetty-mode +1)))

;;;###autoload
(defun clipetty-kill-ring-save ()
  "Enables Clipetty just for this save.
It can be annoying to have Clipetty overwrite your system
clipboard every time you kill something.  This function wraps
Clipetty around the `kill-ring-save' function and can be invoked
explicitly."
  (interactive)
  (when (use-region-p)
    (if clipetty-mode
        (kill-ring-save (region-beginning) (region-end))
      (clipetty-mode)
      (kill-ring-save (region-beginning) (region-end))
      (clipetty-mode 0))))

(provide 'clipetty)
;;; clipetty.el ends here
