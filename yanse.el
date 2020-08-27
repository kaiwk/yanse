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
(defconst yanse--hsl-regex (rx "hsl(" (group (* (any "," blank num "%"))) ")"))

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
  "RGB to Hex."
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
  "Hex to RGB."
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

;;;###autoload
(defun yanse-hsl-to-rgb ()
  "HSL to RGB."
  (interactive)
  ;; from: https://www.niwa.nu/2013/05/math-behind-colorspace-conversions-rgb-hsl/
  (when (yanse--hsl-at-point-p)
    (let* ((range (save-match-data
                    (when (thing-at-point-looking-at yanse--hsl-regex)
                      (list (match-beginning 0) (match-end 0)))))
           (begin (nth 0 range))
           (end (nth 1 range))
           (content (save-match-data
                      (when (thing-at-point-looking-at yanse--hsl-regex)
                        (buffer-substring-no-properties (match-beginning 1) (match-end 1)))))
           (hsl (yanse--split-string-trim content ","))
           (hue (/ (string-to-number (nth 0 hsl)) 360.0))
           (sat (/ (string-to-number (substring (nth 1 hsl) 0 -1)) 100.0))
           (lum (/ (string-to-number (substring (nth 2 hsl) 0 -1)) 100.0))
           (temp-1 (if (< lum 0.5) (* lum (+ 1.0 sat)) (- (+ lum sat) (* lum sat))))
           (temp-2 (- (* 2 lum) temp-1))
           (temp-r (let ((res (+ hue 0.333))) (if (> res 1) (- res 1) res)))
           (temp-g hue)
           (temp-b (let ((res (- hue 0.333))) (if (< res 0) (+ res 1) res)))
           (compute-color
            (lambda (temp-color temp-1 temp-2)
              (number-to-string
               (round (* 255
                         (cond
                          ((< (* temp-color 6) 1) (+ temp-2 (* (- temp-1 temp-2) 6 temp-color)))
                          ((< (* temp-color 2) 1) temp-1)
                          ((< (* temp-color 3) 2) (+ temp-2 (* (- temp-1 temp-2) 6 (- 0.666 temp-color))))
                          (t temp-2)))))))
           (r (funcall compute-color temp-r temp-1 temp-2))
           (g (funcall compute-color temp-g temp-1 temp-2))
           (b (funcall compute-color temp-b temp-1 temp-2))
           (rgb (concat "rgb(" (string-join (list r g b) ", ") ")")))
      (kill-region begin end)
      (insert rgb))))

;;;###autoload
(defun yanse-rgb-to-hsl ()
  "RGB to HSL"
  (interactive)
  ;; from: https://www.niwa.nu/2013/05/math-behind-colorspace-conversions-rgb-hsl/
  (when (yanse--rgb-at-point-p)
    (let* ((range (save-match-data
                    (when (thing-at-point-looking-at yanse--rgb-regex)
                      (list (match-beginning 0) (match-end 0)))))
           (begin (nth 0 range))
           (end (nth 1 range))
           (content (save-match-data
                      (when (thing-at-point-looking-at yanse--rgb-regex)
                        (buffer-substring-no-properties (match-beginning 1) (match-end 1)))))
           (rgb (yanse--split-string-trim content ","))
           (r (/ (string-to-number (nth 0 rgb)) 255.0))
           (g (/ (string-to-number (nth 1 rgb)) 255.0))
           (b (/ (string-to-number (nth 2 rgb)) 255.0))
           (min (seq-min (list r g b)))
           (max (seq-max (list r g b)))
           (lum (round (* (/ (+ min max) 2.0) 100.0)))
           (sat (round (* (if (<= lum 50.0) (/ (- max min) (+ max min)) (/ (- max min) (- 2.0 max min))) 100.0)))
           (hue (round (* (cond
                           ((= max r) (/ (- g b) (- max min)))
                           ((= max g) (+ 2.0 (/ (- b r) (- max min))))
                           ((= max b) (+ 4.0 (/ (- r g) (- max min)))))
                          60.0)))
           (hsl (concat "hsl(" (string-join
                                (list
                                 (number-to-string hue)
                                 (concat (number-to-string sat) "%")
                                 (concat (number-to-string lum) "%"))
                                ", ") ")")))
      (kill-region begin end)
      (insert hsl))))

(defun yanse--percentage-to-number (percentage)
  "Percentage string to number."
  (/ (string-to-number (substring percentage 0 -1)) 100.0))

(defun yanse--rgb-at-point-p ()
  "Return t if text under point match `yanse--rgb-regex'."
  (save-match-data
    (thing-at-point-looking-at yanse--rgb-regex)))

(defun yanse--hex-at-point-p ()
  "Return t if text under point match `yanse--hex-regex'."
  (save-match-data
    (thing-at-point-looking-at yanse--hex-regex)))

(defun yanse--hsl-at-point-p ()
  "Return t if text under point match `yanse--hsl-regex'"
  (save-match-data
    (thing-at-point-looking-at yanse--hsl-regex)))

;;;###autoload
(defun yanse-cycle ()
  "Cycle color formats under point."
  (interactive)
  (cond
   ((yanse--rgb-at-point-p) (yanse-rgb-to-hex))
   ((yanse--hex-at-point-p) (yanse-hex-to-rgb))))

(provide 'yanse)

;;; yanse.el ends here
