;;; clipetty.el --- Send every kill from a TTY frame to the system clipboard -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Mike Hamrick

;; Author: Mike Hamrick <mikeh@muppetlabs.com>
;; Maintainer: Mike Hamrick <mikeh@muppetlabs.com>
;; Created: 25 Dec 2019
;; Modified: 30 Dec 2029
;; Version: 0.1
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

(defcustom clipetty-assume-nested-mux nil
  "Non-nil means if we detect a remote multiplexer, we assume it's nested.
Nesting is the practice of running a terminal multiplexer inside
a terminal multiplexer, which is what you'd be doing if you ran
tmux both locally and on remote hosts you SSH into."
  :type 'boolean
  :group 'clipetty)

(defcustom clipetty-tmux-ssh-tty "tmux show-environment SSH_TTY"
  "The command we send to tmux to determine the SSH_TTY.
This default assumes that tmux is on your PATH.  If tmux lives
elsewhere for you, or it is named something else, you can change
it here."
  :type 'string
  :group 'clipetty)

(defcustom clipetty-screen-regexp "^screen"
  "This regexp is matched against TERM to test for the presence of GNU screen.
If you've configured GNU screen to use an unusual terminal type,
you can change this regular expression so Clipetty will recognize
when you're running in screen."
  :type 'regexp
  :group 'clipetty)

(defcustom clipetty-tmux-ssh-tty-regexp "^SSH_TTY=\\([^\n]+\\)"
  "This regexp is used to capture the SSH_TTY from output of tmux.
Unless you're inventing a new method for determining the SSH_TTY, after
a detach / re-attach it's unlikely you'll need to change this."
  :type 'regexp
  :group 'clipetty)

(defconst clipetty-max-cut 74994
  "The maximum length of a string you can send to the clipboard via OSC52.
The max OSC 52 message is 10,000 bytes.  This means we can
support base64 encoded strings of up to 74,994 bytes long.")

(defconst clipetty-screen-dcs-start "\eP"
  "The start DCS escape sequence that GNU screen recognizes.")

(defconst clipetty-tmux-dcs-start "\ePtmux;\e"
  "The start DCS escape sequence that Tmux recognizes.")

(defconst clipetty-dcs-end "\e\\"
  "The end DCS escape sequence that everyone recognizes.")

(defconst clipetty-osc-start "\e]52;c;"
  "The initial OSC 52 escape sequence.")

(defconst clipetty-osc-end "\a"
  "The end OSC 52 escape sequence.")

(defvar clipetty-original-icf nil
  "Keep the original ICF to restore on `clipetty-off' function.")

(defun clipetty-get-tmux-ssh-tty ()
  "Query tmux for its local SSH_TTY environment variable and return it.
Return nil if tmux is unable to locate the environment variable"
  (let ((tmux-ssh-tty (shell-command-to-string clipetty-tmux-ssh-tty)))
    (if (and (not (eq tmux-ssh-tty nil))
             (string-match clipetty-tmux-ssh-tty-regexp tmux-ssh-tty))
        (match-string 1 tmux-ssh-tty)
    nil)))

(defun clipetty-tty (ssh-tty tmux)
  "Return which TTY we should send our OSC payload to.
Both the SSH-TTY and TMUX arguments should come from the selected
frame's environment."
  (if (not ssh-tty)
      (terminal-name)
    (if tmux
        (let ((tmux-ssh-tty (clipetty-get-tmux-ssh-tty)))
          (if tmux-ssh-tty tmux-ssh-tty ssh-tty))
      ssh-tty)))

(defun clipetty-make-dcs (string &optional screen)
  "Return STRING, wrapped in a Tmux flavored Device Control String.
Return STRING, wrapped in a GNU screen flavored DCS, if SCREEN is non-nil."
  (let ((dcs-start clipetty-tmux-dcs-start))
    (when screen (setq dcs-start clipetty-screen-dcs-start))
    (concat dcs-start string clipetty-dcs-end)))

(defun clipetty-dcs-wrap (string tmux term ssh-tty)
  "Return STRING wrapped in an appropriate DCS if necessary.
The arguments TMUX, TERM, and SSH-TTY should come from the selected
frame's environment."
  (let ((screen (if term (string-match-p clipetty-screen-regexp term) nil))
        (dcs    string))
    (cond (screen (setq dcs (clipetty-make-dcs string t)))
          (tmux   (setq dcs (clipetty-make-dcs string))))
    (if ssh-tty (if clipetty-assume-nested-mux dcs string) dcs)))

(defun clipetty-osc (string &optional encode)
  "Return an OSC 52 escape sequence out of STRING.
Optionally base64 encode it first if you specify non-nil for ENCODE."
  (let ((bin (base64-encode-string (encode-coding-string string 'binary) t)))
    (concat clipetty-osc-start (if encode bin string) clipetty-osc-end)))

(defun clipetty-emit (string)
  "Emit STRING, optionally wrapped in a DCS, to an appropriate tty."
  (let ((tmux    (getenv "TMUX" (selected-frame)))
        (term    (getenv "TERM" (selected-frame)))
        (ssh-tty (getenv "SSH_TTY" (selected-frame))))
    (if (<= (length string) clipetty-max-cut)
        (write-region
         (clipetty-dcs-wrap string tmux term ssh-tty)
         nil
         (clipetty-tty ssh-tty tmux)
         t
         0)
      (message "Selection too long to send to terminal %d" (length string))
      (sit-for 1))))

(defun clipetty-p ()
  "Return non-nil if Clipetty is enabled."
(eq interprogram-cut-function #'clipetty-cut))

(defun clipetty-on ()
  "Turn Clipetty on.
Stash the old value of `interprogram-cut-function' to `clipetty-original-icf'
and assign `clipetty-cut'"
  (when (not (clipetty-p))
    (setq clipetty-original-icf interprogram-cut-function)
    (setq interprogram-cut-function #'clipetty-cut)))

(defun clipetty-off ()
  "Turn Clipetty off by restoring the original `interprogram-cut-function'."
  (when (clipetty-p)
    (setq interprogram-cut-function clipetty-original-icf)))

(defun clipetty-toggle ()
  "Toggle assignment of `clipetty-cut' to the `interprogram-cut-function'.
Return non-nil if Clipetty is enabled as the result of the toggle."
  (if (clipetty-p)
      (ignore (clipetty-off))
    (clipetty-on) t))

(defun clipetty-cut (string)
  "If in a terminal frame, convert STRING to a series of OSC 52 messages."
  (if (display-graphic-p)
      (gui-select-text string)
    ;; An exclamation mark is an invalid base64 string. This signals to the
    ;; Kitty terminal emulator to reset the clipboard.  Other terminals will
    ;; simply ignore this.
    ;;
    ;; TODO: Support longer than `clipetty-max-cut' length messages in Kitty.
    (clipetty-emit (clipetty-osc "!"))
    (clipetty-emit (clipetty-osc string t))))

;;;###autoload
(defun clipetty-kill-ring-save ()
  "Enables Clipetty just for this save.
It can be annoying to have Clipetty overwrite your system
clipboard every time you kill something.  This function wraps
Clipetty around the `kill-ring-save' function and can be invoked
explicitly."
  (interactive)
  (when (use-region-p)
    (if (clipetty-p)
        (kill-ring-save (region-beginning) (region-end))
      (clipetty-toggle)
      (kill-ring-save (region-beginning) (region-end))
      (clipetty-toggle))))

;;;###autoload
(define-minor-mode clipetty-mode
  "Minor mode to send every kill from a TTY frame to the system clipboard."
  :lighter " Clp"
  :group 'clipetty
  :init-value nil
  :global nil
  (make-local-variable 'interprogram-cut-function)
  (if clipetty-mode
      (clipetty-on)
    (clipetty-off)))

;;;###autoload
(define-globalized-minor-mode global-clipetty-mode
  clipetty-mode
  (lambda () (clipetty-mode +1))
  :group 'clipetty)

(provide 'clipetty)
;;; clipetty.el ends here
