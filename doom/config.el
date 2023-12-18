;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(setq in-termux (if (executable-find "termux-setup-storage") t nil))

(defun ctr/termux? ()
  in-termux)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Christopher Rosset"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

(use-package! alert
  :config
  (when in-termux
    (setq alert-termux-command "~/bin/async-notify")
    (setq alert-default-style 'termux)))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(use-package! org
  :init

  :config
  (setq org-directory "~/org")
  (setq org-ellipses "…")
  (setq org-log-done "time")
  (setq org-log-done-with-time t)

  ;; Logging notes with `org-add-note' will save them in the :LOGBOOK: drawer
  ;; instead of entering them at point. This is useful for separating updates
  ;; and logical content of a node.
  ;;
  ;; https://scripter.co/using-org-logbook-notes-to-record-blog-post-updates/
  (setq org-log-into-drawer t)

  (setq org-roam-directory "~/org/roam")
  (setq +org-capture-todo-file "agenda/todo.org")

  (let ((mytemps '(("t" "Personal todo" entry
                    (file+headline +org-capture-todo-file "Inbox")
                    "* TODO %?\n%i\n%T"))))
    (dolist (current mytemps)
      (cl-delete-if (lambda (template) (equal (car template) (car current))) org-capture-templates)
      (push current org-capture-templates)))

  (defun ctr/org-table-yank-current-cell ()
    (interactive)
    (when (org-at-table-p)
      (kill-new
       (string-trim
        (substring-no-properties (org-table-get-field))))))

  (defun ctr/org-table-kill-current-cell ()
    (interactive)
    (when (org-at-table-p)
      (ctr/org-table-yank-current-cell)
      (org-table-blank-field)))

  (map! :map org-mode-map
        :leader
        :prefix "m b"
        "y" #'ctr/org-table-yank-current-cell
        "Y" #'ctr/org-table-kill-current-cell)

  ;; This piece of code customizes the display of org-agenda items to display
  ;; the TODO of an item before the breadcrumbs instead of between the
  ;; breadcrumbs and the node itself. This is the default:
  ;;
  ;;     Parent Node 1/Parent Node 2/DONE Child Node
  ;;
  ;; And this is the result with org-agenda-breadcrumbs-level set to 1;
  ;;
  ;;     DONE Parent Node 1/Parent Node 2/Child Node
  ;;
  ;; Sourced from:
  ;; https://list.orgmode.org/CAGEgU=hGnXj7TSGV6pvdSeWFWP_iVwe8WRu+uh8Hjh_7NNRKLw@mail.gmail.com/T/
  (defun ctr/org-agenda-breadcrumb-setup ()

    (defvar org-agenda-breadcrumbs-level 1
      "Highest level subtree to include in Org agenda breadcrumb.")

    (defun org-agenda-breadcrumbs-string ()
      "Create formatted string with outline of Org subtree at point.

The outline goes up to subtree level
`org-agenda-breadcrumbs-level` and the subtree headings are
separated by `org-agenda-breadcrumbs-separator`."
      (org-format-outline-path (nthcdr (1- org-agenda-breadcrumbs-level)
                                       (org-get-outline-path))
                               (1- (frame-width))
                               nil org-agenda-breadcrumbs-separator))

    (defun org-agenda-insert-breadcrumbs-before-text (args)
      "In Org agenda, insert outline breadcrumbs just before heading text in ARGS.

This is an advice function for use with `org-agenda-format-item`
by doing:

(advice-add #'org-agenda-format-item :filter-args
            #'org-agenda-insert-breadcrumbs-before-text)

Since ARGS is the list of arguments to be passed to
`org-agenda-format-item`, the second list element of ARGS
contains the heading text to be modified."
      (org-with-point-at (org-get-at-bol 'org-marker)
        (let* ((txt (org-get-heading t t t t))
               (index (or (cl-search txt (cadr args)) 0))
               (bc (let ((s (org-agenda-breadcrumbs-string)))
                     (if (eq "" s)
                         ""
                       (concat s org-agenda-breadcrumbs-separator)))))
          (setf (substring (cadr args) index index) bc)
          args)))

    (advice-add #'org-agenda-format-item :filter-args
                #'org-agenda-insert-breadcrumbs-before-text))

  (defun ctr/org-get-short-title ()
    "Retrieve buffer's title but use +short if available."
    (let ((short (org-collect-keywords '("short"))))
      (if short
          (cadar short)
        (org-get-title))))

  (defun ctr/org-agenda-category ()
    "Custom formatter for the category (file) of an org item."

    (s-pad-right org-agenda-category-width
                 " "
                 (concat
                  (s-truncate (1- org-agenda-category-width) (ctr/org-get-short-title) "…")
                  ":")))

  (defun ctr/org-agenda-todo-padding ()
    "If an item doesn't have a TODO keyword, return space padding for
visual alignment. My org state keywords are all 4 characters wide
by convention.

Using this in org-agenda-prefix-format you can get this:

  Category:          Scheduled:  TODO With a keyword
  Category:          Scheduled:       Without a keyword
"

    (if (org-entry-get (point) "TODO") "" (s-repeat 5 " ")))

  ;; org-agenda specific setup
  (setq org-agenda-breadcrumbs-separator "/")
  (setq org-agenda-category-width 16) ; custom
  (setq org-agenda-files `(,(concat org-directory "/agenda") ,org-roam-directory))
  (setq org-agenda-prefix-format-default
        '((agenda . " %i %(ctr/org-agenda-category) %-12s %(ctr/org-agenda-todo-padding)")
          (todo   . " %i %(ctr/org-agenda-category)")
          (tags   . " %i %(ctr/org-agenda-category)")
          (search . " %i %(ctr/org-agenda-category)")))
  (setq org-agenda-prefix-format-narrow
        '((agenda . " %i %(ctr/org-agenda-todo-padding)")
          (todo   . " %i ")
          (tags   . " %i ")
          (search . " %i ")))

  (setq org-agenda-prefix-format org-agenda-prefix-format-default)
  (setq org-agenda-span 45)
  (setq org-agenda-start-day "-1d")
  (setq org-agenda-start-on-weekday nil)
  (ctr/org-agenda-breadcrumb-setup)


  (org-roam-db-autosync-mode)
  (setq org-roam-node-display-template-default org-roam-node-display-template)
  (setq org-roam-node-display-template-narrow #("${doom-hierarchy:*}"))

  (defun symbol-suffix (sym str)
    "Given a symbol and a string suffix, return the symbol whose name
matches the original name, hyphen, suffix."
    (intern (format "%s-%s" (symbol-name sym) str)))

  (defmacro adjust-for-width (sym)
    `(set ,sym (if (< (frame-width) 60)
                   (symbol-suffix ,sym "narrow")
                 (symbol-suffix ,sym "default"))))

  (defun adjust-for-display-width ()
    "Adjust some settings to optimize display for a narrow display."
    (adjust-for-width 'org-agenda-prefix-format)
    (adjust-for-width 'org-roam-node-display-template))

  (add-hook 'window-configuration-change-hook #'adjust-for-display-width)


  ) ; org / org-roam


(when (ctr/termux?)
  (use-package! alert
    :config
    (setq alert-default-style 'termux)))


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq world-clock-list
      '(("America/Los_Angeles" "San Francisco")
        ("America/Chicago" "Chicago")
        ("America/New_York" "New York")
        ("UTC" "UTC")
        ("Europe/London" "London")
        ("Europe/Warsaw" "Warsaw")
        ("Asia/Singapore" "Singapore")
        ("Asia/Tokyo" "Tokyo")))

(when (ctr/termux?)
  ;; Makes URLs open in the browser app.
  (setq browse-url-browser-function 'browse-url-xdg-open))

;; https://discourse.doomemacs.org/t/how-to-re-bind-keys/56
;; https://rameezkhan.me/posts/2020/2020-07-03--adding-keybindings-to-doom-emacs/

(map! "C-x /" #'comment-or-uncomment-region)
(map! :n "g s l" #'evil-avy-goto-line)

;; Doom binds `g =' instead of `g +' in normal mode. I think this is because =
;; is the same key as + but doesn't require holding shift. However, + is easier
;; to remember and easier to access on a virtual keyboard. So let's have both.
;;
;; https://docs.doomemacs.org/latest/modules/editor/evil/#/configuration/increment-decrement-number-at-point
;; https://github.com/doomemacs/doomemacs/blob/986398504d09e585c7d1a8d73a6394024fe6f164/modules/editor/evil/config.el#L475
(map! :n "g +" #'evil-numbers/inc-at-pt)

(after! org
  (map! :map org-mode-map
        :leader
        (:prefix ("m z" . "custom")
                 "n" #'org-add-note)))
