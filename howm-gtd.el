;;; howm-gtd.el --- GTD with howm

;; Copyright (C) 2010  Takahiro HIMURA

;; Author: Takahiro HIMURA <taka@himura.jp>
;; Keywords: calendar, docs

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

;; 

;;; Code:

(require 'howm)
(require 'action-lock)
(require 'cl)

(eval-after-load "action-lock"
  '(progn
     ;; monkey patch to action-lock-font-lock
     (defun action-lock-font-lock ()
       (cheat-font-lock-mode action-lock-silent)
       (if (null action-lock-original-font-lock-keywords)
           (setq action-lock-original-font-lock-keywords font-lock-keywords)
         (setq font-lock-keywords action-lock-original-font-lock-keywords))
       (when action-lock-rules
         (let* ((entries (mapcar (lambda (pair)
                                   (let* ((regexp (car pair))
                                          (matcher (action-lock-matcher regexp))
                                          (pos (or (caddr pair) 0))
                                          (face (or (cadddr (cdr pair)) 'action-lock-face))
                                          (hilit (list pos face
                                                       'prepend)))
                                     (cons matcher hilit)))
                                 action-lock-rules)))
           (cheat-font-lock-append-keywords entries)
           ;;       (cheat-font-lock-prepend-keywords entries)
           (cheat-font-lock-fontify t)
           )))))

(defvar howm-gtd-type-spec
  '(("NEXT"    (:face howm-gtd-face-next :keys (?n) :order 0))
    ("INBOX"   (:face howm-gtd-face-inbox :keys (?i) :order -1))
    ("WAIT"    (:face howm-gtd-face-misc :keys (?w) :order -2))
    ("PROJECT" (:face howm-gtd-face-misc :keys (?p) :order -3))
    ("SOMEDAY" (:face howm-gtd-face-misc :keys (?s) :order -4))
    ("REVIEW"  (:face howm-gtd-face-misc :keys (?r) :order -5))
    ("CANCEL"  (:face howm-gtd-face-done :keys (?c ?x) :order nil))
    ("DONE"    (:face howm-gtd-face-done :keys (?d) :order nil)))
  "")

(defvar howm-gtd-default-type "DONE"
  "")

(defvar action-lock-switch-time-default-tag ""
  "")

(howm-defun-memoize howm-gtd-all-types ()
  (mapcar #'car howm-gtd-type-spec))

(howm-defun-memoize howm-gtd-activated-type-regexp ()
  (regexp-opt
   (delq nil
         (mapcar (lambda (x) (if (howm-gtd-get-parameter x :order) x nil))
                 (howm-gtd-all-types)))
   t))

(howm-defun-memoize howm-gtd-get-parameter (type key)
  (cadr (member key (cadr (assoc type howm-gtd-type-spec)))))

(howm-defun-memoize howm-gtd-menu-key-list ()
  (apply 'append
         (mapcar (lambda (type)
                   (mapcar (lambda (k) (cons k type))
                           (howm-gtd-get-parameter type :keys)))
                 (howm-gtd-all-types))))

(defface howm-gtd-face-misc
  '((t
     (:foreground "ForestGreen" :weight bold)))
  "howm howm-gtd misc face")
(defface howm-gtd-face-inbox
  '((t
     (:foreground "Firebrick" :weight bold)))
  "howm howm-gtd inbox face")
(defface howm-gtd-face-next
  '((t
     (:foreground "Blue" :weight bold)))
  "howm howm-gtd next face")
(defface howm-gtd-face-done
  '((t
     (:foreground "gray" :weight bold)))
  "howm howm-gtd done face")

(defun action-lock-switch-time (label-list)
  (let ((time-format "[%Y-%m-%d %H:%M]")
        (time-regexp (let ((dd "[0-9]\\{2\\}"))
                       (format "\\(\\[%s%s-%s-%s %s:%s\\]\\(.*\\) \\)?"
                               dd dd dd dd dd dd))))
    (let ((regexp (concat time-regexp (regexp-opt label-list t))))
      (list regexp
            `(lambda (&optional dummy)
               (let* ((b (match-beginning 0))
                      (e (match-end 0))
                      (s (match-string-no-properties 3))
                      (tag (or (match-string-no-properties 2) action-lock-switch-time-default-tag))
                      (next (or (action-lock-item-menu (howm-gtd-menu-key-list)) howm-gtd-default-type)))
                 (delete-region b e)
                 (insert (format-time-string (concat ,time-format tag)) " " next)
                 (goto-char b)))
            3 nil #'(action-lock-get-gtd-type 3)))))

(defun action-lock-get-gtd-type (kwd)
  (if (numberp kwd) (setq kwd (match-string kwd)))
  (or (howm-gtd-get-parameter kwd :face) 'gtd-face-misc))

(defun action-lock-item-menu (itemlist)
  (interactive "P")
  (message (mapconcat (lambda (item) (format "(%c)%s" (car item) (cdr item))) itemlist ", "))
  (let* ((sw (selected-window)) (c (read-char)))
    (select-window sw)
    (cdr (assoc c itemlist))))

(setq action-lock-default-rules
       (cons (action-lock-switch-time (howm-gtd-all-types))
             action-lock-default-rules))

(defun howm-gtd-menu-all ()
  (let* ((types (concat "[+]? " (howm-gtd-activated-type-regexp)))
         (r (howm-reminder-regexp types))
         (rg (howm-reminder-regexp-grep types))
         (summarizer (howm-reminder-summarizer r))
         (folder (howm-reminder-search-path-folder)))
    (howm-menu-general "todo" 'todo
                       (howm-sort #'howm-gtd-priority #'howm-gtd-priority-gt
                                  (howm-view-search-folder-items rg folder summarizer)))))
(setq howm-menu-allow (cons 'howm-gtd-menu-all howm-menu-allow))

(defun howm-gtd-get-type (str)
  (let* ((rx (regexp-opt (howm-gtd-all-types) t)))
    (if (string-match rx str)
        (match-string 1 str)
      nil)))

(defun howm-gtd-priority (item)
  (let* ((p (howm-todo-parse item))
         (late (car p))
         (type (second p))
         (lazy (third p))
         (description (fifth p))
         (f (or (cdr (assoc type howm-todo-priority-func))
                #'howm-todo-priority-unknown))
         (priority (funcall f late lazy item))
         (type (howm-gtd-get-type description)))
    (cons type priority)))

(defun howm-gtd-priority-gt (e1 e2)
  (let ((e1t (howm-gtd-get-parameter (car e1) :order)) ; FIXME
        (e2t (howm-gtd-get-parameter (car e2) :order)))
    (if (and e1t e2t)
        (cond ((> e1t e2t) t)
              ((< e1t e2t) nil)
              ((> (cdr e1) (cdr e2)) t)     ; FIXME
              (t nil))
      nil)))

(defun howm-gtd-menu (gtdtype)
  (let* ((types (format "[+]? %s" gtdtype))
         (r (howm-reminder-regexp types))
         (rg (howm-reminder-regexp-grep types))
         (summarizer (howm-reminder-summarizer r))
         (folder (howm-reminder-search-path-folder)))
    (howm-menu-general "todo" 'todo
                       (howm-view-search-folder-items rg folder summarizer))))
(setq howm-menu-allow (cons 'howm-gtd-menu howm-menu-allow))

(provide 'howm-gtd)
;;; howm-gtd.el ends here
