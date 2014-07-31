;; Emacs initialization file

;;;; Functions

;; Form for executing code only if a particular library is available
(defmacro with-library (symbol &rest body)
  `(if (require ',symbol nil t)
      (progn
        ,@body)
    (message (format "%s is not available." ',symbol))))




;;;; Plugin / package setup
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(load-file "~/.emacs.d/init-package.el")

;;;; General Settings

(with-library monokai-theme
  (load-theme 'monokai t))

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq require-final-newline t)
(show-paren-mode t)

(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)
(setq mouse-drag-copy-region t)

;; tab settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

(column-number-mode t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
;(menu-bar-mode -1)

(fset 'yes-or-no-p 'y-or-n-p)

;; recently open files
(require 'recentf)
(recentf-mode t)
(setq recentf-max-menu-items 25)


(require 'tramp)
(setq tramp-terminal-type "dumb")

;;;; General Keybindings

(global-set-key (kbd "C-c r") 'recentf-open-files)
(global-set-key (kbd "C-c f") 'ff-find-other-file)
(global-set-key (kbd "C-c c") (lambda ()
				(interactive)
				(pop-to-buffer "*compilation*")
				(compile (read-string "Compile command: " compile-command))))

;;;; Programming Setup Section

;; Camel case support

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(subword-mode)

(require 'cc-mode)

(c-add-style
 "mycppstyle"
 '((c-basic-offset . 4)
   (c-comment-only-line-offset . 0)
   (c-offsets-alist
    (access-label -4)
    (defun-open . 0)
    (defun-close . 0)
    (statement-block-intro . +)
    (substatement-open . 0)
    (substatement-label . 0)
    (label . 0)
    (statement-cont . 0)
    (inline-open . 0)
    (inline-close . 0)
    (innamespace . 0)
    (inextern-lang . 0)
    (extern-lang-open . 0)
    (extern-lang-close . 0))))

;(add-hook 'c++-mode-hook (lambda () (c-set-style "mycppstyle")))
(setq c-default-style (cons '(c++-mode . "mycppstyle") c-default-style))

;; scroll the output of a compilation window until an error is encountered
(setq compilation-scroll-output 'first-error)
