;; Emacs initialization file

;;;; Plugin / package setup

;; Form for executing code only if a particular library is available
(defmacro with-library (symbol &rest body)
  `(if (require ',symbol nil t)
      (progn
        ,@body)
    (message (format "%s is not available." ',symbol))))

(defun settings/packages-installed-p ()
  (require 'cl)
  (loop for pkg in settings/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(defvar settings/packages
  '(evil
    fiplr
    haskell-mode
    magit
    rainbow-delimiters
    smartparens
    undo-tree
    yasnippet))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(unless (settings/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg settings/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(load-file "~/.emacs.d/init-package.el")


;;;; Visual Settings

(with-library monokai-theme
  (load-theme 'monokai t))

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(column-number-mode t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

(show-paren-mode t) ; highlight matching parens


;;;; General Settings

(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)
(setq mouse-drag-copy-region t)

(setq require-final-newline t)

;; tab settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

(fset 'yes-or-no-p 'y-or-n-p)

(with-library recentf ; recently open files
  (recentf-mode t)
  (setq recentf-max-menu-items 25))

(with-library tramp
  (setq tramp-terminal-type "dumb"))


;;;; General Keybindings

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-c r") 'recentf-open-files)
(global-set-key (kbd "C-c /") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c f") 'ff-find-other-file)
(global-set-key (kbd "C-c c") (lambda ()
                                (interactive)
                                (pop-to-buffer "*compilation*")
                                (compile (read-string "Compile command: " compile-command))))


;;;; Programming Setup Section

(subword-mode) ; Camel case word support
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode)) ; treat .h files as C++
(setq compilation-scroll-output 'first-error)

(with-library cc-mode
  (c-add-style "mycppstyle"
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
  (setq c-default-style (cons '(c++-mode . "mycppstyle") c-default-style)))



