;;; od2ae --- Convert org-drill entries to anki-editor entries -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Robert Irelan <rirelan@gmail.com>
;;
;; Description: Convert org-drill entries to anki-editor entries
;; Author: Robert Irelan
;; Version: 0.1.0
;; Package-Requires: ((emacs "26") (anki-editor "0.3.3") (org-drill "2.7.0") (org "9.3") (org-ql "0.6"))
;; URL: https://github.com/telotortium/emacs-od2ae
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This package is for people who would like to convert their org-drill
;;  cards to anki-editor entries to allow exporting the cards to Anki.
;;
;;  With this package, you can export a single card by running
;;  ‘od2ae-convert-entry-to-anki’, or export everything in your agenda by
;;  running ‘od2ae-convert-all-notes’.
;;
;;  The scheduling information will be exported in a format that can be
;;  read by the Anki add-on
;;  https://github.com/telotortium/anki-import-scheduling-info-from-csv
;;  in order to transfer the scheduling info from your org-drill cards
;;  to the corresponding Anki cards. That data will be written to
;;  ‘od2ae-scheduling-info-buffer-name’, but you will need to save this buffer
;;  yourself.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'anki-editor)
(require 'org)
(require 'org-drill)
(require 'org-element)
(require 'org-macs)
(require 'org-ql)

(defcustom od2ae-deck "Default"
  "Anki deck to create new notes in.")
(defcustom od2ae-scheduling-info-buffer-name "*od2ae-scheduling-info*"
 "Buffer to put card scheduling info into for importing into Anki.

Write this buffer to a file and import it into Anki using the addon
Anki Import Scheduling Info From CSV:
https://github.com/telotortium/anki-import-scheduling-info-from-csv.")
(defconst od2ae-note-type-to-convert-function
  '(("simple" . od2ae-convert-simple)
    ("twosided" . od2ae-convert-twosided)
    ("hide1cloze" . od2ae-convert-cloze)
    ("hide2cloze" . od2ae-convert-cloze)
    ("show1cloze" . od2ae-convert-cloze)
    ("show2cloze" . od2ae-convert-cloze)
    ("multicloze" . od2ae-convert-cloze)
    ("hidefirst" . od2ae-convert-cloze)
    ("hidelast" . od2ae-convert-cloze)
    ("hide1_firstmore" . od2ae-convert-cloze)
    ("show1_lastmore" . od2ae-convert-cloze)
    ("show1_firstless" . od2ae-convert-cloze))
  "Alist mapping org-drill note type to function to convert contents to Anki")

(defun od2ae-convert-entry-to-anki ()
  "Create an Anki card from an org-drill entry at current point."
  (interactive)
  (org-with-point-at (point)
    (org-back-to-heading)
    (when (org-entry-get (point) "ANKI_NOTE_ID")
      (user-error "Headline at %S must not have already been added to Anki."
                  (point-marker)))
    (unless (member org-drill-question-tag (org-get-tags nil 'local))
      (user-error "Headline at %S must contain org-drill question tag ‘%s'"
                  (point-marker) org-drill-question-tag))
    (let* ((card-type (org-entry-get (point) "DRILL_CARD_TYPE")))
      ;; Handle unset card type - if cloze is present, it’s a Cloze card;
      ;; otherwise a simple card.
      (when (null card-type)
        (org-with-point-at (point)
          (org-narrow-to-element)
          (if (re-search-forward org-drill-cloze-regexp nil t)
              (setq card-type "hide1cloze")
            (setq card-type "simple"))))
      ;; Convert the card.
      (funcall (alist-get card-type od2ae-note-type-to-convert-function
                          nil nil #'equal))
      (org-entry-put (point) "ANKI_DECK" od2ae-deck)
      (anki-editor-push-notes '(4))
      (when-let ((reason (org-entry-get (point) "ANKI_FAILURE_REASON")))
        (signal 'od2ae
                (format "Pushing note at %S failed: %s"
                        (point-marker) reason)))
      (od2ae-scheduling-info-export)
      (atomic-change-group
        (org-drill-strip-entry-data)
        (org-toggle-tag org-drill-question-tag 'off)))))

(defun od2ae-convert-all-notes ()
  (interactive)
  "Convert all org-drill notes to anki-editor and export them."
  (org-ql-select
    (org-agenda-files)
    `(and
      (tags-local ,org-drill-question-tag)
      (not (tags-local "PRIVATE")))
    :action
    (lambda ()
      (org-goto-marker-or-bmk (point-marker))
      (redraw-display)
      (condition-case-unless-debug
          err
          (let ((org-attach-id-dir-orig org-attach-id-dir))
            (unwind-protect
                (progn
                  (setq org-attach-id-dir
                        (file-truename org-attach-id-dir))
                  (od2ae-convert-entry-to-anki))
              (setq org-attach-id-dir org-attach-id-dir-orig)))
        (error
         (warn "error at %S: %S" (point-marker) err)))
      (sleep-for 1))))

(defun od2ae-scheduling-info-export ()
  "Export scheduling info of ‘org-drill' card at point to CSV.

Will write to buffer in ‘od2ae-scheduling-info-buffer-name’."
  (interactive)
  (org-with-point-at (point)
    (org-back-to-heading)
    (when-let*
        ((note (string-to-number
                (org-entry-get (point) "ANKI_NOTE_ID")))
         (cards
          (alist-get 'cards
                     (nth 0
                          (anki-editor--anki-connect-invoke-result
                           'notesInfo
                           `((notes ,note))))))
         (scheduled (or (org-element-property :scheduled
                                              (org-element-at-point))
                        'new))
         (due
          (if (eq scheduled 'new)
              ""
            (format-time-string
             "%Y-%m-%d"
             (org-timestamp-to-time scheduled))))
         (ease
          (if (eq scheduled 'new) "2.5"
            (org-entry-get (point) "DRILL_EASE")))
         (interval
          (if (eq scheduled 'new) "0"
            (org-entry-get (point) "DRILL_LAST_INTERVAL")))
         (reps
          (if (eq scheduled 'new) "0"
            (org-entry-get (point) "DRILL_TOTAL_REPEATS"))))
      (with-current-buffer (get-buffer-create od2ae-scheduling-info-buffer-name)
        (goto-char (point-max))
        (dolist (card cards)
          (unless (and (bolp) (eolp))
            (move-end-of-line)
            (newline))
          (insert
           (format
            "%d,%s,%d,%d,%d"
            card
            due
            (round (string-to-number interval))
            (round (* 1000 (string-to-number ease)))
            (round (string-to-number reps))))
          (newline))))))

(defun od2ae-convert-cloze ()
  "Convert an org-drill Cloze card to one compatible with anki-editor."
  (atomic-change-group
    (org-with-point-at (point)
      (org-back-to-heading)
      (org-narrow-to-subtree)
      (save-excursion
        (org-entry-put (point) "ANKI_NOTE_TYPE" "Cloze")
        (let* ((cloze-count 1)
               (beginning-marker (make-marker))
               (end-marker (make-marker))
               cloze-beginning)
          ;; Skip as much metadata as we can.
          (od2ae-skip-all-meta-data)
          (newline)
                    ;; Insert Text field of Anki card
          (let ((org-insert-heading-respect-content nil))
            (org-insert-heading))
          (save-excursion
             (insert "Text")
             (newline))
          (org-demote)
          (save-excursion
            (while (re-search-forward org-drill-cloze-regexp nil t)
              (let ((hint (match-string-no-properties 2)))
                (unless (string-blank-p hint)
                  ;; Strip leading hint separator
                  (setq hint (substring hint
                                        (length org-drill-hint-separator)
                                        (length hint)))
                  ;; Delete hint (with separator)
                  (delete-region (match-beginning 2)
                                 (match-end 2))
                  ;; Move before matched region and retry.
                  (goto-char (match-beginning 0))
                  (forward-char -1)
                  (re-search-forward org-drill-cloze-regexp))
                (setq cloze-beginning
                      (+ (match-beginning 0)
                         (length org-drill-left-cloze-delimiter)))
                (set-marker beginning-marker cloze-beginning)
                (set-marker end-marker (match-end 2))
                (delete-region (match-beginning 3) (match-end 3))
                (delete-region (match-beginning 0) cloze-beginning)
                (anki-editor-cloze
                 (marker-position beginning-marker)
                 (marker-position end-marker)
                 cloze-count
                 hint)
                (setq cloze-count (+ 1 cloze-count))))
            (set-marker beginning-marker nil)
            (set-marker end-marker nil))))
      (org-next-visible-heading 1)
      (let ((pt (point)))
        (org-forward-heading-same-level 1)
        (if (= pt (point))
            ;; No heading at this level. Just insert new heading
            (progn
              (org-end-of-subtree)
              (org-insert-heading-respect-content)
              (insert "Extra"))
          ;; Already a heading - just rename it then.
          (org-edit-headline "Extra"))))))

(defun od2ae-convert-simple ()
  "Convert an org-drill simple card to one compatible with anki-editor."
  (atomic-change-group
    (org-with-point-at (point)
      (org-back-to-heading)
      (org-narrow-to-subtree)
      (save-excursion
        (org-entry-put (point) "ANKI_NOTE_TYPE" "Basic")
        (progn
          ;; Skip as much metadata as we can.
          (od2ae-skip-all-meta-data)
          (newline)
          ;; Insert Front field of Anki card
          (let ((org-insert-heading-respect-content nil))
            (org-insert-heading))
          (save-excursion
             (insert "Front")
             (newline))
          (org-demote)))
      (org-next-visible-heading 1)
      (let ((pt (point)))
        (org-forward-heading-same-level 1)
        (if (= pt (point))
            ;; No heading at this level. Just insert new heading
            (progn
              (org-end-of-subtree)
              (org-insert-heading-respect-content)
              (insert "Back"))
          ;; Already a heading - just rename it then.
          (org-edit-headline "Back"))))))

(cl-defun od2ae-convert-twosided ()
  "Convert an org-drill twosided card to one compatible with anki-editor."
  (atomic-change-group
    (catch 'break
      (org-with-point-at (point)
        (org-back-to-heading)
        (org-narrow-to-subtree)
        (save-excursion
          (org-entry-put (point) "ANKI_NOTE_TYPE" "Basic (and reversed card)")
          (let ((pt (point)))
            (org-next-visible-heading 1)
            (if (= pt (point))
                (throw 'break nil)))
          (org-edit-headline "Front")
          (let ((pt (point)))
             (org-forward-heading-same-level 1)
             (if (= pt (point))
                 (throw 'break nil)))
          (org-edit-headline "Back")
          (let ((pt (point)))
             (org-forward-heading-same-level 1)
             (if (= pt (point))
                 (throw 'break nil)))
          (org-edit-headline "Notes")
          nil)))))

(defun od2ae-skip-all-meta-data ()
  "Skip PROPERTIES and LOGBOOK drawer, as well as other metadata.

Similar to ‘org-end-of-meta-data’ with FULL argument, but also skips PROPERTIES
and LOGBOOK drawers if they are later than that."
  (let ((meta-data-pt
         (org-with-point-at (point)
           (org-narrow-to-subtree)
           (org-end-of-meta-data 'full)
           (point)))
        (property-pt
         (org-with-point-at (point)
           (org-narrow-to-subtree)
           (re-search-forward org-property-drawer-re nil t)
           (point)))
        (logbook-pt
         (org-with-point-at (point)
           (org-narrow-to-subtree)
           (re-search-forward org-logbook-drawer-re nil t)
           (point))))
    (goto-char (max meta-data-pt property-pt logbook-pt))))

(provide 'od2ae)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; od2ae.el ends here
