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
  (concat (file-name-directory (or load-file-name buffer-file-name)) "cheat-sheets")
  "absolute path to the cheat-sheets directory.")

(defvar cheater--cheat-sheets-files
  (directory-files cheater--cheat-sheets-directory nil "\\.cheat$")
  "all *.cheat files in the cheat sheets directory.")

(defvar cheater--ansi-code-hash (make-hash-table :test 'equal)
  "hash table with the ansi color and effect escape sequences.")

(defvar cheater--color-list '("black" "red" "green" "yellow" "blue" "magenta" "cyan" "white")
  "list of all available ansi colors.")

(defun cheater-cheat-sheet-filepath (name)
  "returns the absolute cheat sheet file path for NAME."
  (concat cheater--cheat-sheets-directory "/" name ".cheat"))

(defun cheater-remove-file-extension (filenames)
  "remove the '.cheat' file extension for each file in FILENAMES"
  (mapcar (lambda (filename)
            (first (split-string filename ".cheat"))) filenames))

(defun cheater-setup-cheat-sheet-buffer (buffer cheat-sheet)
  "setup BUFFER to load the cheat-sheet."
  (with-current-buffer buffer
    (erase-buffer)
    (insert-file-contents (cheater-cheat-sheet-filepath cheat-sheet))
    (ansi-color-apply-on-region (point-min) (point-max))
    (goto-char (point-min))
    (setq buffer-read-only t)))


(defun cheater-title ()
  "format region with red and bold ansi escape sequence."
  (interactive)
  (cheater-apply-ansi-escape-to-region (concat
                                        (gethash "redf" cheater--ansi-code-hash)
                                        (gethash "bold" cheater--ansi-code-hash))))

(defun cheater-subtitle ()
  "format region with bold and underline ansi escape sequence."
  (interactive)
  (cheater-apply-ansi-escape-to-region (concat
                                        (gethash "bold" cheater--ansi-code-hash)
                                        (gethash "underline" cheater--ansi-code-hash))))

(defun cheater-underline ()
  "format region with underline ansi escape sequence."
  (interactive)
  (cheater-apply-ansi-escape-to-region (gethash "underline" cheater--ansi-code-hash)))

(defun cheater-bold ()
  "format region with bold ansi escape sequence."
  (interactive)
  (cheater-apply-ansi-escape-to-region (gethash "bold" cheater--ansi-code-hash)))

(defun cheater-italic ()
  "format region with italic ansi escape sequence."
  (interactive)
  (cheater-apply-ansi-escape-to-region (gethash "italic" cheater--ansi-code-hash)))

(defun cheater-apply-foreground-color ()
  "format region with an ansi escape sequence foreground color."
  (interactive)
  (cheater-apply-ansi-escape-to-region (gethash
                                        (concat (cheater-color-autocompleter "Foreground Color: ") "f")
                                        cheater--ansi-code-hash)))

(defun cheater-apply-background-color ()
  "format region with an ansi escape sequence background color."
  (interactive)
  (cheater-apply-ansi-escape-to-region (gethash
                                        (concat (cheater-color-autocompleter "Background Color: ") "b")
                                        cheater--ansi-code-hash)))

(defun cheater-color-autocompleter (color-level)
  "ansi color autocompleter."
  (let ((completing-read-func (if (null ido-mode)
                                  'completing-read
                                'ido-completing-read)))
    (setq color
          (funcall completing-read-func
                   color-level
                   cheater--color-list
                   nil
                   t))))

(defun cheater-apply-ansi-escape-to-region (ansi-code)
  "apply the ANSI-CODE sequences to the region."
  (save-excursion
    (goto-char (region-beginning)) (insert ansi-code)
    (goto-char (region-end)) (insert (cond
                                      ((<= (length ansi-code) 5) (gethash "reset" cheater--ansi-code-hash))
                                      ((>= (length ansi-code) 8) (concat (gethash "reset" cheater--ansi-code-hash)
                                                                         (gethash "reset" cheater--ansi-code-hash)))))))

(defun cheater-init-ansi-code-hash ()
  "initialize the hash for the ansi escape sequences."
  ;; foreground-color-sequences
  (puthash "blackf" "[30m" cheater--ansi-code-hash)
  (puthash "redf" "[31m" cheater--ansi-code-hash)
  (puthash "greenf" "[32m" cheater--ansi-code-hash)
  (puthash "yellowf" "[33m" cheater--ansi-code-hash)
  (puthash "bluef" "[34m" cheater--ansi-code-hash)
  (puthash "magentaf" "[35m" cheater--ansi-code-hash)
  (puthash "cyanf" "[36m" cheater--ansi-code-hash)
  (puthash "whitef" "[37m" cheater--ansi-code-hash)
  ;; background-color-sequences
  (puthash "blackb" "[40m" cheater--ansi-code-hash)
  (puthash "redb" "[41m" cheater--ansi-code-hash)
  (puthash "greenb" "[42m" cheater--ansi-code-hash)
  (puthash "yellowb" "[43m" cheater--ansi-code-hash)
  (puthash "blueb" "[44m" cheater--ansi-code-hash)
  (puthash "magentab" "[45m" cheater--ansi-code-hash)
  (puthash "cyanb" "[46m" cheater--ansi-code-hash)
  (puthash "whiteb" "[47m" cheater--ansi-code-hash)
  ;; effects-sequences
  (puthash "bold" "[1m" cheater--ansi-code-hash)
  (puthash "underline" "[4m" cheater--ansi-code-hash)
  (puthash "italic" "[3m" cheater--ansi-code-hash)
  (puthash "italic" "[3m" cheater--ansi-code-hash)
  (puthash "reset" "[0m" cheater--ansi-code-hash))

(defvar cheater-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c f t")   'cheater-title)
    (define-key map (kbd "C-c f s")   'cheater-subtitle)
    (define-key map (kbd "C-c f i")   'cheater-italic)
    (define-key map (kbd "C-c f b")   'cheater-bold)
    (define-key map (kbd "C-c f u")   'cheater-underline)
    (define-key map (kbd "C-c c f")   'cheater-apply-foreground-color)
    (define-key map (kbd "C-c c b")   'cheater-apply-background-color)
    map)
  "Keymap used in cheater-mode.")

;;;###autoload
(defun cheater-mode ()
  "Major mode for editing Cheat Sheets."
  (interactive)
  (use-local-map cheater-mode-map)
  (setq mode-name "Cheater")
  (setq major-mode 'cheater-mode)

  (if (fboundp 'run-mode-hooks)
      (run-mode-hooks 'cheater-mode-hook)
    (run-hooks 'cheater-mode-hook)))

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

(cheater-init-ansi-code-hash)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cheat$" . cheater-mode))

(provide 'cheater)
