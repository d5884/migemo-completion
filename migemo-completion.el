;;; mcomp.el --- Migemo completion feature.

;; Copyright (C) 2015  Daisuke Kobayashi

;; Author: Daisuke Kobayashi <d5884jp@gmail.com>
;; Version: 0.1
;; Keywords: extensions
;; Package-Requires: ((migemo "1.9") (cl-lib "0.5"))

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

;; This package provides completion style by migemo.

;; Usage:

;; Put into init.el below:
;;
;;  (require 'migemo-completion)
;;  ;; activate in file category
;;  (add-to-list 'completion-category-overrides '(file (styles migemo)))


;;; Code:

(require 'migemo)
(require 'cl-lib)

(defgroup migemo-completion nil
  "A group for migemo-completion."
  :group 'completion)

(defcustom migemo-completion-minibuffer-toggle-key (kbd "M-m")
  "Toggle key for migemo-completion in minibuffer."
  :group 'migemo-completion
  :type 'key-sequence)

(defcustom migemo-completion-minibuffer-indicator-list '("[MIGEMO]" . "")
  "Cons cell of minibuffer indicator for migemo-completion status.
Car part is active state. Cdr part is inactive state."
  :group 'migemo-completion
  :type '(cons string string))

(defface migemo-completion-minibuffer-indicator-face '((t :inherit highlight))
  "Face of migemo-completion minibuffer inidicator."
  :group 'migemo-completion)

(defvar migemo-completion-active-p t
  "If non-nil, migemo-completion is activated.")

(defun migemo-completion-toggle-active (&optional flag)
  "Toggle migemo-completion active state.
With a prefix argument FLAG, disalbe migemo-completion if FLAG is
negative, and enable it otherwise."
  (interactive)
  (setq migemo-completion-active-p
        (cond
         ((and (numberp flag) (not (natnump flag)))
          nil)
         (flag
          t)
         (t
          (not migemo-completion-active-p))))
  (migemo-completion--update-minibuffer-prompt))

(defun migemo-completion-try-completion (string table pred point)
  "Try to complete STRING using completion table TABLE with `migemo'.
Only the elements of table that satisfy predicate PRED are considered.
POINT is the position of point within STRING.
The return value can be either nil to indicate that there is no completion,
t to indicate that STRING is the only possible completion,
or a pair (NEWSTRING . NEWPOINT) of the completed result string together with
a new position for point."
  (if (not migemo-completion-active-p)
      nil
    (condition-case err
        (let* ((case-fold-search completion-ignore-case)
               (boundaries (completion-boundaries string table pred ""))
               (prefix (substring string 0 (car boundaries)))
               (input (substring string (car boundaries)))
               (bare-candidates (migemo-completion--all-candidates prefix input table pred))
               (candidates (if minibuffer-completing-file-name
                               (completion-pcm--filename-try-filter bare-candidates)
                             bare-candidates))
               (common-part (migemo-completion--extract-common-prefix candidates)))
          (cond
           ((null candidates)
            nil)
           ((and (eq (length candidates) 1)
                 (equal input (car candidates)))
            t)
           ((migemo-completion--string-prefix-p common-part input)
            (cons string (length string)))
           (t
            (cons (concat prefix common-part) (length (concat prefix common-part))))))
      (invalid-regexp
       ;; (completion--message (error-message-string err))
       nil))))

(defun migemo-completion-all-completions (string table pred point)
  "List the possible completions of STRING in completion table TABLE with `migemo'.
Only the elements of table that satisfy predicate PRED are considered.
POINT is the position of point within STRING.
The return value is a list of completions and may contain the base-size
in the last `cdr'."
  (if (not migemo-completion-active-p)
      nil
    (condition-case err
        (let* ((case-fold-search completion-ignore-case)
               (boundaries (completion-boundaries string table pred ""))
               (prefix (substring string 0 (car boundaries)))
               (input (substring string (car boundaries)))
               (candidates (migemo-completion--all-candidates prefix input table pred)))
          (completion-hilit-commonality candidates point (car boundaries)))
                                        ; XX custom hilit/C-a error
      (invalid-regexp
       ;; (completion--message (error-message-string err))
       nil))))


;; Internals

(defun migemo-completion--all-candidates (prefix input table pred)
  "List the possible completions of PREFIX with migemized INPUT restriction
in completion table TABLE.
Only the elements of table that satisfy predicate PRED are considered."
  (let* ((case-fold-search completion-ignore-case)
         (regexp (migemo-completion--get-pattern input))
         (completion-regexp-list (cons regexp completion-regexp-list))
         (candidates (all-completions prefix table pred)))
    (if (functionp table)
        (cl-remove-if-not (apply-partially #'string-match-p regexp)
                          candidates)
      ;; The internal functions already obeyed completion-regexp-list.
      candidates)))

(defun migemo-completion--get-pattern (str)
  "Return migemized regexp pattern from STR."
  (save-match-data
    (let ((case-fold-search nil))
      (string-match "^\\([^a-zA-Z0-9-]*\\)\\([a-zA-Z0-9-]*\\)" str)
      (concat "^"
              (regexp-quote (match-string 1 str))
              (migemo-get-pattern (match-string 2 str))
              (regexp-quote (substring str (match-end 0)))))))

(defun migemo-completion--get-common-prefix (s1 s2)
  "Return the longest common prefix string of S1 and S2."
  (let ((cmp (compare-strings s1 nil nil s2 nil nil completion-ignore-case)))
    (if (eq cmp t)
        s1
      (setq cmp (1- (abs cmp)))
      (if (zerop cmp)
          ""
        (substring s1 0 cmp)))))

(defun migemo-completion--extract-common-prefix (strs)
  "Extract common prefix from list of STRS."
  (if strs
      (cl-reduce #'migemo-completion--get-common-prefix strs)
    ""))

(defun migemo-completion--string-prefix-p (s1 s2)
  "Return non-nil if S1 is a prefix of S2."
  (string-prefix-p s1 s2 completion-ignore-case))

;; minibuffer indicator

(defvar migemo-completion--indicator-overlay nil
  "Indicator overlay.")

(defvar migemo-completion--toggle-original-function nil
  "Original function bound `migemo-completion-minibuffer-toggle-key'.")
(make-variable-buffer-local 'migemo-completion--toggle-original-function)

(defun migemo-completion--keymap-ancestor-p (map)
  "Return non-nil if MAP is ancestor of current local map."
  (cl-loop with x = (current-local-map)
           for y = x then (keymap-parent y)
           while y
           when (eq y map)
           return t))

(defun migemo-completion--update-minibuffer-prompt ()
  "Update migemo-completion state on minibuffer prompt."
  (when (window-minibuffer-p (selected-window))
    (if (and migemo-completion--indicator-overlay
             (overlayp migemo-completion--indicator-overlay))
        (move-overlay migemo-completion--indicator-overlay 0 1)
      (setq migemo-completion--indicator-overlay (make-overlay 0 1)))
    (overlay-put migemo-completion--indicator-overlay 'before-string
                 (if migemo-completion-active-p
                     (concat
                      (propertize (car migemo-completion-minibuffer-indicator-list)
                                  'face 'migemo-completion-minibuffer-indicator-face)
                      " ")
                   (or (cdr migemo-completion-minibuffer-indicator-list) "")))))

(defun migemo-completion--enabled-context-p ()
  "Return non-nil if migemo-completion is in enabled context in minibuffer."
  (and (memq 'migemo (completion--styles (and (functionp minibuffer-completion-table)
                                              (funcall minibuffer-completion-table
                                                       "" nil 'metadata))))
       (migemo-completion--keymap-ancestor-p minibuffer-local-completion-map)))

(defun migemo-completion--minibuffer-setup ()
  "Minibuffer setup function for migemo completion."
  (when (migemo-completion--enabled-context-p)
    (migemo-completion--update-minibuffer-prompt)
    ;; save original function
    (setq migemo-completion--toggle-original-function
          (local-key-binding migemo-completion-minibuffer-toggle-key))
    (local-set-key migemo-completion-minibuffer-toggle-key 'migemo-completion-toggle-active)
    (add-hook 'minibuffer-exit-hook 'migemo-completion--minibuffer-exit-function nil t)))

(defun migemo-completion--minibuffer-exit-function ()
  "Minibuffer exit function for migemo completion."
  ;; restore original function
  (local-set-key migemo-completion-minibuffer-toggle-key
                 migemo-completion--toggle-original-function))

;; install (but not activated)
(add-hook 'minibuffer-setup-hook 'migemo-completion--minibuffer-setup)

;; add to styles database
(add-to-list 'completion-styles-alist
             '(migemo
               migemo-completion-try-completion
               migemo-completion-all-completions
	       "Completion with Migemo. I.e. completing \"ha\" matches \"haru\", \"はる\" and \"春\"."))

(provide 'migemo-completion)

;;; migemo-completion.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
;; End:
