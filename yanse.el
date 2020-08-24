;;; yanse.el --- Convert between color formats in Emacs.          -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Wang Kai

;; Author: Wang Kai <kaiwkx@gmail.com>
;; Keywords: extensions, tools
;; URL: https://github.com/kaiwk/yanse
;; Package-Requires: ((emacs "26"))
;; Version: 0.1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; yanse is a tool for color formats conversion.
;;
;; Currently, it supports:
;; - RGB triplet
;; - Hex format
;;
;;; Code:


(require 'thingatpt)

(defconst yanse--rgb-regex (rx "rgb(" (group (* (any "," blank num))) ")"))
(defconst yanse--hex-regex (rx "#" (group (* (any "a-f" "A-F" num)))))

(defun yanse--parse-rgb (rgb-str)
  "Parse RGB from RGB-STR."
  (let ((rgb-str (thread-first rgb-str string-trim downcase)))
    (save-match-data
      (if (string-match yanse--rgb-regex rgb-str)
          (match-string 1 rgb-str)))))

(defun yanse--rgb-listp (rgb-list)
  "Verify RGB-LIST."
  (and
   (= (length rgb-list) 3)
   (or
    (seq-reduce (lambda (acc elt) (and (ignore-errors (string-to-number elt)) acc))
                rgb-list t)
    (seq-reduce (lambda (acc elt) (and (ignore-errors (numberp elt)) acc))
                rgb-list t))))

(defun yanse--decimal-to-hex (digits)
  "Convert DIGITS list to hex list."
  (cond
   ((stringp (car digits)) (mapcar (lambda (d) (format "%.2x" (string-to-number d))) digits))
   ((numberp (car digits)) (mapcar (lambda (d) (format "%.2x" d)) digits))))

(defun yanse--split-string-trim (str sep)
  "Split STR with SEP and trim each element."
  (unless (or (null str) (string-empty-p str))
    (mapcar #'string-trim (split-string str sep))))

;;;###autoload
(defun yanse-rgb-to-hex ()
  "RGB from BEGIN to END in hex format."
  (interactive)
  (save-match-data
    (when (thing-at-point-looking-at yanse--rgb-regex)
      (let* ((begin (match-beginning 0))
             (end (match-end 0))
             (rgb (buffer-substring-no-properties begin end))
             (rgbs (yanse--split-string-trim (yanse--parse-rgb rgb) ","))
             (hex (upcase
                   (if (yanse--rgb-listp rgbs)
                       (concat
                        "#"
                        (thread-first rgbs
                          yanse--decimal-to-hex
                          string-join))
                     ""))))
        (unless (string-empty-p hex)
          (kill-region begin end)
          (insert hex))))))

;;;###autoload
(defun yanse-hex-to-rgb ()
  "Hex from BEGIN to END in RGB format."
  (interactive)
  (save-match-data
    (when (thing-at-point-looking-at yanse--hex-regex)
      (let* ((begin (match-beginning 0))
             (end (match-end 0))
             (hex (buffer-substring-no-properties begin end)))
        (let* ((r (substring hex 1 3))
               (g (substring hex 3 5))
               (b (substring hex 5 7))
               (rgb (concat
                     "rgb("
                     (string-join
                      (list
                       (number-to-string (string-to-number r 16))
                       (number-to-string (string-to-number g 16))
                       (number-to-string (string-to-number b 16)))
                      ", ")
                     ")")))
          (when rgb
            (kill-region begin end)
            (insert rgb)))))))

(defun yanse--rgb-at-point-p ()
  "Return t if text under point match `yanse--rgb-regex'."
  (save-match-data
    (thing-at-point-looking-at yanse--rgb-regex)))

(defun yanse--hex-at-point-p ()
  "Return t if text under point match `yanse--hex-regex'."
  (save-match-data
    (thing-at-point-looking-at yanse--hex-regex)))

;;;###autoload
(defun yanse-cycle ()
  "Cycle color formats under point."
  (interactive)
  (cond
   ((yanse--rgb-at-point-p) (yanse-rgb-to-hex))
   ((yanse--hex-at-point-p) (yanse-hex-to-rgb))))

(provide 'yanse)

;;; yanse.el ends here
