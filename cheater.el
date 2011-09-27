;;; cheater.el --- A collection of cheat sheets for the daily needs

;; Copyright (C) 2011 Samuel Tonini

;; Author: Samuel Tonini <tonini.samuel@gmail.com>
;; URL:
;; Version: 0.1
;; Created: 27 September 2011
;; Keywords: emacs cheat-sheets
;; EmacsWiki:

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

(defvar cheater--cheat-sheets-directory
  (concat (file-name-directory (or load-file-name buffer-file-name)) "cheat-sheets"))

(defvar cheater--cheat-sheets-files
  (directory-files cheater--cheat-sheets-directory nil "\\.cheat$"))

(defun cheater ()
  "Look up for Cheat Sheets"
  (interactive)
  (let ((completing-read-func (if (null ido-mode)
                                  'completing-read
                                'ido-completing-read)))
    (setq cheat-sheet
          (funcall completing-read-func
                   "cheat sheet: "
                   (cheater-remove-file-extension cheater--cheat-sheets-files)
                   nil
                   t)))
  (let ((cheater-buffer-name (format "*cheater %s*" cheat-sheet)))
    (unless (get-buffer cheater-buffer-name)
      (let ((cheater-buffer (get-buffer-create cheater-buffer-name)))
        (display-buffer cheater-buffer)
        (cheater-setup-cheat-sheet-buffer cheater-buffer cheat-sheet)))
    (display-buffer cheater-buffer-name)))

(defun cheater-setup-cheat-sheet-buffer (buffer cheat-sheet)
  "setup BUFFER to load the cheat-sheet."
  (with-current-buffer buffer
    (erase-buffer)
    (insert-file-contents (cheater-cheat-sheet-filepath cheat-sheet))
    (ansi-color-apply-on-region (point-min) (point-max))
    (goto-char (point-min))
    (setq buffer-read-only t)))

(defun cheater-cheat-sheet-filepath (name)
  "returns the absolute cheat sheet file path for NAME."
  (concat cheater--cheat-sheets-directory "/" name ".cheat"))

(defun cheater-remove-file-extension (filenames)
  "remove the '.cheat' file extension for each file in FILENAMES"
  (mapcar (lambda (filename)
          (first (split-string filename ".cheat"))) filenames))

(provide 'cheater)
