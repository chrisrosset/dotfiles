;; Emacs initialization file

;;;; Plugin / package setup


(load-file "~/.emacs.d/init-package.el")

;;;; Functions

;; Form for executing code only if a particular library is available
(defmacro with-library (symbol &rest body)
  `(if (require ',symbol nil t)
      (progn
        ,@body)
    (message (format "I guess we don't have %s available." ',symbol))))

;;;; General Settings

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq require-final-newline t)
(show-paren-mode t)

;; tab settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

(column-number-mode t)

(scroll-bar-mode -1)
(tool-bar-mode -1)

(fset 'yes-or-no-p 'y-or-n-p)

;; recently open files
(require 'recentf)
(recentf-mode t)
(setq recentf-max-menu-items 25)

;;;; General Keybindings

(global-set-key (kbd "C-c r") 'recentf-open-files)
(global-set-key (kbd "C-c f") 'ff-find-other-file)
(global-set-key (kbd "C-c c") (lambda ()
				(interactive)
				(pop-to-buffer "*compilation*")
				(compile (read-string "Compile command: " compile-command))))

;;;; Programming Setup Section

;; Camel case support
(subword-mode)

(require 'cc-mode)

(defconst my-cc-style
    '("linux"
          (c-offsets-alist . ((innamespace . [0])))))

(c-add-style "my-cc-mode" my-cc-style)

(setq c-default-style
      (append '((c++-mode . "my-cc-mode")
            c-default-style)))

(setq c-default-style "my-cc-mode"
      c-basic-offset 4)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))



;; scroll the output of a compilation window until an error is encountered
(setq compilation-scroll-output 'first-error)
