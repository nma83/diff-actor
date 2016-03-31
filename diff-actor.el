;;; diff-actor.el -- Act out diff's between files

;; Copyright (C) 2016  Narendra Acharya

;; Author: Narendra Acharya (nma83 at github.com)

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
;; Given 2 files as input, diff-actor generates the diff between them
;; and acts out the changes as live edits in an Emacs buffer.
;; The first file is used as the starting point and edits are made on
;; it to lead to the second file.
;; Has wrapper functions to pick up the 2 files from revision history
;; of various VCSs like git and p4.
;;

;;; Installation:
;;
;; Install diff-actor.el in load-path
;; Basic file diff acting:
;;  (diff-actor file-name-1 file-name-2)
;; This function uses the Emacs variable diff-command for diff-ing
;;
;; VCS (vcs-name) wrappers:
;;  (diff-actor-vcs-name file-name &optional ver-1 ver-2)
;; This function uses the VCS specific diff command for diff-ing
;; ver1 and ver2 of file-name. Default values:
;;  ver1 - first version
;;  ver2 - top of tree

;;; Requirements
;;

;;; Links
;;

;; Constants
(defconst diff-actor-diff-header "\\(^[0-9].*\n\\)")

;; Delay between edits in sec
(defvar diff-actor-edit-delay 2)
;; Finer delays in msec
(defvar diff-actor-char-delay 100)
(defvar diff-actor-word-delay 200)
(defvar diff-actor-line-delay 500)

;; Entry points
(defun diff-actor-file (file-name-1 file-name-2 &optional real-file-name)
  (setq diff-actor-real-file-name file-name-1)
  (diff-actor-act file-name-1 (diff-actor-get-diff file-name-1 file-name-2)
                  real-file-name))

;; ... more for VCSs
;; P4 versions diff, requires p4
;; Versions #1 and #head are diff-ed
(defun diff-actor-p4-vers (&optional file-name ver-1 ver-2)
  (interactive)
  (if (file-executable-p p4-executable)
      (let ((diff-file (or file-name (read-file-name "Enter file name: ")))
            (diff-ver-1 (or ver-1 "1"))
            (diff-ver-2 (or ver-2 "head"))
            (tmp-file-1 (make-temp-file "diff-actor"))
            (tmp-file-2 (make-temp-file "diff-actor")))
        (setq diff-actor-real-file-name diff-file)
        (message "printing to %s, %s" tmp-file-1 tmp-file-2)
        (call-process p4-executable nil (list :file tmp-file-1)
                      nil "print" "-q" (concat diff-file "#" diff-ver-1))
        (call-process p4-executable nil (list :file tmp-file-2)
                      nil "print" "-q" (concat  diff-file "#" diff-ver-2))
        (diff-actor-file tmp-file-1 tmp-file-2))))

;; Git versions diff, requires git
;; Versions HEAD-1 and HEAD and diff-ed
(defun diff-actor-git-vers (&optional file-name ver-1 ver-2)
  (interactive)
  (if (file-executable-p (executable-find magit-git-executable))
      (let ((diff-file (or file-name (read-file-name "Enter file name: ")))
            (diff-ver-1 (or ver-1 "HEAD^"))
            (diff-ver-2 (or ver-2 "HEAD"))
            (tmp-file-1 (make-temp-file "diff-actor"))
            (tmp-file-2 (make-temp-file "diff-actor")))
        (message "printing to %s, %s" tmp-file-1 tmp-file-2)
        (call-process magit-git-executable nil (list :file tmp-file-1)
                      nil "show" (concat diff-ver-1 ":" diff-file))
        (call-process magit-git-executable nil (list :file tmp-file-2)
                      nil "show" (concat diff-ver-2 ":" diff-file))
        (diff-actor-file tmp-file-1 tmp-file-2 diff-file))))

;; Backend functions
(defun diff-actor-act (file-name-1 diff-string real-file-name)
  (let ((buf-name (or real-file-name file-name-1)))
    (switch-to-buffer (concat buf-name " (act)"))
    (insert-file-contents file-name-1)
    (mapc 'diff-actor-act-chunk (diff-actor-diff-chunks diff-string))
    (message "done acting")))

(defun diff-actor-act-chunk (chunk)
  (let ((decoded-header
         (diff-actor-decode-header (plist-get chunk 'header)))
        (old (plist-get chunk 'old))
        (new (plist-get chunk 'new)))
    (cond
     ((eq (plist-get decoded-header 'action) 'add)
      (diff-actor-act-add decoded-header old new))
     ((eq (plist-get decoded-header 'action) 'delete)
      (diff-actor-act-delete decoded-header old new))
     ((eq (plist-get decoded-header 'action) 'change)
      (diff-actor-act-change decoded-header old new)))
    (sit-for (random diff-actor-edit-delay))))

(defun diff-actor-act-change (header old new)
  (let ((old-lines (plist-get header 'old))
        (new-lines (plist-get header 'new))
        (new-stripped (substring new 0 -1)))
    (goto-line (nth 0 new-lines))
    ;;(insert (format "c %s to %s" old-lines new-lines))
    (kill-line (diff-actor-lines-in-range old-lines))
    (diff-actor-insert new-stripped)))

(defun diff-actor-act-delete (header old new)
  (let ((old-lines (plist-get header 'old))
        (new-lines (plist-get header 'new)))
    (goto-line (nth 0 new-lines))
    ;;(insert (format "delete %s to %s" old-lines new-lines))
    (kill-line (diff-actor-lines-in-range old-lines))))

(defun diff-actor-act-add (header old new)
  (let ((old-lines (plist-get header 'old))
        (new-lines (plist-get header 'new))
        (old-stripped (substring old 0 -1)))
    ;; the added lines will be assigned to 'old by the chunker
    (goto-line (nth 0 new-lines))
    ;;(insert (format "a %s to %s" old-lines new-lines))
    (diff-actor-insert old-stripped)))

;; The insert function that mimics typing
(defun diff-actor-insert (text)
  (let ((lines (split-string text "\n")))
    (dolist (line lines)
      (let ((words (split-string line " ")))
        (dolist (word words)
          (dotimes (chari (length word))
            (insert (elt word chari))
            (diff-actor-wait-msec diff-actor-char-delay))
          (insert " ")
          (diff-actor-wait-msec diff-actor-word-delay))
        (insert "\n")
        (diff-actor-wait-msec diff-actor-line-delay)))
    (if (> (length lines) 0) (backward-delete-char 1))))

(defun diff-actor-wait-msec (msec)
  (sit-for (/ (float (random msec)) 1000)))

(defun diff-actor-lines-in-range (range)
  (if (nth 1 range)
      (1+ (- (nth 1 range) (nth 0 range)))
    1))

(defun diff-actor-decode-header (header)
  (let ((act-header (substring header 1))
        (ret-header) (ranges))
    (setq ranges (split-string act-header "[acd]"))
    (setq ret-header (plist-put ret-header 'old
                                (mapcar 'string-to-number
                                        (split-string (car ranges) ","))))
    (setq ret-header (plist-put ret-header 'new
                                (mapcar 'string-to-number
                                        (split-string (car (cdr ranges)) ","))))
    (cond
     ((string-match "a" act-header)
      (setq ret-header (plist-put ret-header 'action 'add)))
     ((string-match "c" act-header)
      (setq ret-header (plist-put ret-header 'action 'change)))
     ((string-match "d" act-header)
      (setq ret-header (plist-put ret-header 'action 'delete))))
    ret-header))

(defun diff-actor-get-diff (file-name-1 file-name-2)
  (with-temp-buffer
    (when (and (file-readable-p file-name-1)
             (file-readable-p file-name-2))
      (call-process diff-command nil t nil file-name-1 file-name-2)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun diff-actor-diff-chunks (diff-string)
  ;; Return a list of diff-chunk's
  ;; diff-chunk is a plist with diff location and old/new content
  ;; Dup header line
  (setq diff-string-mod
        (replace-regexp-in-string diff-actor-diff-header "\\&H\\&"
                                          diff-string))
  (let ((diff-chunks (split-string diff-string-mod diff-actor-diff-header t)))
    (mapcar 'diff-actor-diff-chunk diff-chunks)))

(defun diff-actor-diff-chunk (diff-chunk)
  ;; Return a plist for this diff-chunk
  (let ((diff-lines (split-string diff-chunk "\n"))
        (curr-diff) (diff-part 'old))
    (dolist (line diff-lines curr-diff)
      (let ((line-trunc (if (string-equal "" line) line
                         (substring line 2))))
        (cond
         ((diff-actor-start-of-diff line)
          ;;(when curr-diff
          ;;  (setq ret-value (append ret-value (list curr-diff))))
          (setq curr-diff (list 'header line
                                'old ""
                                'new "")))
         ((diff-actor-sep-of-diff line)
          (setq diff-part 'new))
         (t
          (if curr-diff
              (if (eq diff-part 'old)
                  (let ((curr-diff-old (plist-get curr-diff 'old)))
                    (setq curr-diff (plist-put curr-diff 'old
                                               (concat curr-diff-old
                                                       line-trunc "\n"))))
                (let ((curr-diff-new (plist-get curr-diff 'new)))
                  (setq curr-diff (plist-put curr-diff 'new
                                             (concat curr-diff-new
                                                     line-trunc "\n"))))))))))))

;; Diff region detection
(defun diff-actor-start-of-diff (line)
  (string-match "^H" line))
(defun diff-actor-sep-of-diff (line)
  (string-match "^---" line))

(provide 'diff-actor)

;; diff-actor.el ends here
