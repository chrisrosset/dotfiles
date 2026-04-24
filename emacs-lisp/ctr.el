;;; ctr.el --- Common Technical Routines (jk) -*- lexical-binding: t; -*-
;; URL: https://github.com/chrisrosset/dotfiles
;; Package-Version: 1.0.0
;; Package-Requires: ((emacs "29.1") (dash "2.20.0") (org "9.7.0") (s "1.0.0"))

;;; Commentary:
;;; it's pronounced `quatre'

;;; Code:

(defun ctr-alist-s-get (key alist &optional default remove)
  "Like alist-get but specifically for alists with string keys."
  (alist-get key alist default remove #'string=))

(defun ctr-org-props-alist-get (key alist)
  "Like alist-get but case-insensitive."
  (alist-get key alist nil nil #'string-equal-ignore-case))

(defun ctr-org-entry-properties-with-point (&rest args)
  "Like `org-entry-properties' but add a `POINT' property."
  (cons (cons "POINT" (point))
        (apply #'org-entry-properties args)))

(defun ctr-org-completing-read-date
    (prompt collection &optional predicate require-match initial-input hist def inherit-input-method)
  (let ((org-read-date-prefer-future nil))
    (format-time-string (org-time-stamp-format nil t)
                        (org-read-date nil t nil prompt nil def))))

(defun ctr-org-person-property? (prop-name)
  "Predicate for matching properties used to capture contact names."
  (or (string-equal-ignore-case prop-name "Initiator")
      (s-matches? "[pP][1-9][0-9]*" prop-name)))

(defun ctr-org-entry-get-date-prop (entry)
  "Get the `Date' property from this entry."
  (ctr-org-props-alist-get "Date" entry))

(defun ctr-org-entry-get-people-props (&optional props)
  "Get all org properties which refer to a person.

If PROPS are provided, analyze that. Otherwise, use the entry at point."
  (--> (or props (org-entry-properties))
       (-filter (lambda (prop-pair) (ctr-org-person-property? (car prop-pair))) it)))

(defun ctr-org-entry-get-people-names ()
  ""
  (-map #'cdr (ctr-org-entry-get-people-props)))

(defun ctr-org-p-prop-max ()
  ""
  (--> (ctr-org-entry-get-people-props)
       (-map #'car it)
       (-select (lambda (s) (s-starts-with? "P" s t)) it)
       (-map (lambda (s) (s-chop-left 1 s)) it)
       (-map #'string-to-number it)
       (cons 0 it) ; for not found
       (-max it)
       ))

(defun ctr-org-get-all-people ()
  ""
  (->> (org-map-entries #'ctr-org-entry-get-people-names)
       (-flatten)
       (-sort #'string<)
       (-uniq)))

(defun ctr-allowed-names-for-people-properties (name)
  ""
  (when (ctr-org-person-property? name)
    (cons ":ETC" ; allows for new names
          (ctr-org-get-all-people))))

(defun ctr-org-add-next-person-property ()
  "Call `org-set-property' with a deduced person (PX) property name."
  (interactive)
  (let* ((key (concat "P" (number-to-string (1+ (ctr-org-p-prop-max)))))
         (val (org-completing-read (concat key " Value: ") (ctr-org-get-all-people))))
    (org-set-property key val))
  )

(defsubst ctr--string-gt (lhs rhs)
  "string> seems to cause parsing issues when used inside of threading macros."
  (string> lhs rhs))

(defun ctr-org-last-touchpoint-for-person ()
  "Move point to the last contact with this person."
  (interactive)
  (let ((name (org-completing-read "Name: " (ctr-org-get-all-people))))
    (-some--> (org-map-entries #'ctr-org-entry-properties-with-point)
      ;; Only entries with a Date field (sanity check)
      (-filter #'ctr-org-entry-get-date-prop it)
      ;; Only select entries which match this name
      (-filter (lambda (entry)
                 (-contains? (-map #'cdr (ctr-org-entry-get-people-props entry)) name))
               it)
      (-max-by (-on #'string> #'ctr-org-entry-get-date-prop) it)
      (goto-char (ctr-alist-s-get "POINT" it))
      )))


(add-to-list 'org-property-allowed-value-functions
             #'ctr-allowed-names-for-people-properties)

(add-to-list 'org-property-set-functions-alist
             `("Date" . ,#'ctr-org-completing-read-date))


(provide 'ctr)
;;; ctr.el ends here
