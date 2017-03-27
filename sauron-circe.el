;;; sauron-circe.el --- Add desktop notifications to Circe.         -*- lexical-binding: t; -*-

;; Copyright (C) 2014 - 2016 Ruben Maher

;; Version: 0.1
;; Author: Sébastien Le Maguer 
;; URL: https://github.com/seblemaguer/sauron-circe
;; Package-Requires: ((emacs "24.4") (circe "2.3") (alert "1.2"))

;; This program is free software: you can redistribute it and/or modify
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

;; Sauron support for circe IRC client

;;; Contributors:

;; Michael McCracken <michael.mccracken@gmail.com>
;; Syohei YOSHIDA <syohex@gmail.com>
;; Steve Purcell <steve@sanityinc.com>
;; Chris Barrett

;;; Code:

(require 'circe)
(require 'sauron)

(defgroup sauron-circe nil
  "Add Sauron notifications to Circe."
  :prefix "sauron-circe-"
  :group 'circe)

(defvar sauron-circe-running nil
  "*internal* whether sauron-circe is running.")


(defvar sauron-circe-wait-list nil
  "An alist of nicks that have triggered notifications in the last
`sauron-circe-wait-for' seconds.")

(defcustom sauron-circe-alert-style 'libnotify
  "`alert' style to use.  See `alert-styles' for a list of possibilities.  You
can also define your own."
  :type 'symbol
  :group 'sauron-circe)

(defcustom sauron-circe-wait-for 90
  "The number of seconds to wait before allowing some nick in
`sauron-circe-wait-list' to trigger a notification again."
  :type 'integer
  :group 'sauron-circe)

(defcustom sauron-circe-watch-strings nil
  "A list of strings which can trigger a notification.  You don't need to put
your nick here, it is added automagically by
`sauron-circe-nicks-on-all-networks' when it checks the values in
`circe-network-options'."
  :type '(repeat string)
  :group 'sauron-circe)

(defun sauron-circe-PRIVMSG (nick userhost _command target text)
  (when (sauron-circe-should-notify nick userhost target text)
    (sauron-circe-notify nick text target)))

(defun sauron-circe-JOIN (nick userhost _command channel
                                      &optional accountname realname)
  (when (sauron-circe-should-notify nick userhost channel "")
    (sauron-circe-notify nick (concat "/JOIN " channel) channel)))

(defun sauron-circe-QUIT (nick userhost _command
                                      &optional channel reason)
  (when (sauron-circe-should-notify
         nick userhost (or channel "") (or reason ""))
    (sauron-circe-notify nick "/QUIT" (or channel ""))))

(defun sauron-circe-PART (nick userhost _command channel
                                      &optional reason)
  (when (sauron-circe-should-notify nick userhost channel (or reason ""))
    (sauron-circe-notify
     nick (concat "/PART (" channel ")") (or channel ""))))

(defun sauron-circe-should-notify (nick userhost channel body)
  "If NICK is not in either `circe-ignore-list' or `circe-fool-list' (only
applicable if `lui-fools-hidden-p'), CHANNEL is either in `tracking-buffers'
\(i.e., not currently visible) or Emacs is not currently focused by the window
manager (detected if `sauron-circe-check-window-focus' is true), NICK has
not triggered a notification in the last `sauron-circe-wait-for' seconds
and NICK matches any of `sauron-circe-watch-strings', show a desktop
notification."
  (unless (cond ((circe--ignored-p nick userhost body))
                ((and (circe--fool-p nick userhost body)
                      (lui-fools-hidden-p))))
    ;; Checking `tracking-buffers' has the benefit of excluding
    ;; `tracking-ignored-buffers'.  Also if a channel is in `tracking-buffers',
    ;; it is not currently focused by Emacs.
    (when (or (member channel tracking-buffers) ;; message to a channel
               (member nick tracking-buffers)) ;; private message
    ;; (when (cond ((or (member channel tracking-buffers) ;; message to a channel
    ;;                  (member nick tracking-buffers))) ;; private message
                ;; ((and sauron-circe-check-window-focus
                ;;       (not sauron-circe-emacs-focused))))
      (when (sauron-circe-not-getting-spammed-by nick)
        (when (catch 'return
                (dolist (n sauron-circe-watch-strings)
                  (when (or (string-match n nick)
                            (string-match n body)
                            (string-match n channel))
                    (throw 'return t))))
          (progn
            (if (assoc nick sauron-circe-wait-list)
                (setf (cdr (assoc nick sauron-circe-wait-list))
                      (float-time))
              (setq sauron-circe-wait-list
                    (append sauron-circe-wait-list
                            (list (cons nick (float-time))))))
            t))))))

(defun sauron-circe-notify (nick body channel)
  "Show a desktop notification from NICK with BODY."
    (sauron-add-event
     'circe 3 ;; FIXME: default is 3 for alert just to check
           (format "%s (%s): %s" nick channel body)))

(defun sauron-circe-not-getting-spammed-by (nick)
  "Return an alist with NICKs that have triggered notifications in the last
`sauron-circe-wait-for' seconds, or nil if it has been less than
`sauron-circe-wait-for' seconds since the last notification from NICK."
  (if (assoc nick sauron-circe-wait-list)
      (sauron-circe-wait-a-bit nick) t))

(defun sauron-circe-wait-a-bit (nick)
  "Has it has been more than `sauron-circe-wait-for' seconds since
the last message from NICK?"
  (let* ((last-time (assoc-default
                     nick
                     sauron-circe-wait-list
                     (lambda (x y)
                       (string-equal y x))))
         (seconds-since (- (float-time) last-time)))
    (when (< sauron-circe-wait-for seconds-since)
      (setf (cdr (assoc nick sauron-circe-wait-list)) (float-time))
      t)))

(defun sauron-circe-nicks-on-all-networks ()
  "Get a list of all nicks in use according to `circe-network-options'."
  (delete-dups (mapcar (lambda (opt)
                          (plist-get (cdr opt) :nick))
                        circe-network-options)))

;;;###autoload
(defun sauron-circe-start ()
  "Turn on notifications."
  (interactive)
  (unless sauron-circe-running
    (progn
      (setq sauron-circe-watch-strings
            (append sauron-circe-watch-strings
                    (sauron-circe-nicks-on-all-networks)))
      (advice-add 'circe-display-PRIVMSG :after 'sauron-circe-PRIVMSG)
      (advice-add 'circe-display-channel-quit :after 'sauron-circe-QUIT)
      (advice-add 'circe-display-JOIN :after 'sauron-circe-JOIN)
      (advice-add 'circe-display-PART :after 'sauron-circe-PART)
      (setq sr-mu4e-running t))))


;;;###autoload
(defun sauron-circe-stop ()
  "Turn off notifications."
  (interactive)

  (when sauron-circe-running
    (progn
      (setq sauron-circe-wait-list nil
            sauron-circe-watch-strings nil)
      (advice-remove 'circe-display-PRIVMSG 'sauron-circe-PRIVMSG)
      (advice-remove 'circe-display-channel-quit 'sauron-circe-QUIT)
      (advice-remove 'circe-display-JOIN 'sauron-circe-JOIN)
      (advice-remove 'circe-display-PART 'sauron-circe-PART)
      (setq sauron-circe-running nil))))

(provide 'sauron-circe)
;;; sauron-circe.el ends here
