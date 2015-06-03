;; init-package.el
;;
;; This file is used for initializing package specific settings.

(defun settings/packages-installed-p ()
  (require 'cl)
  (loop for pkg in settings/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(defvar settings/packages
  '(evil
    evil-surround
    fiplr
    haskell-mode
    helm
    magit
    monokai-theme
    rainbow-delimiters
    smart-mode-line
    switch-window
    undo-tree
    windmove
    window-numbering
    yasnippet))

(with-library package
  ;(add-to-list 'package-archives '("melpa"     . "http://melpa.org/packages/")  t)
  ;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (package-initialize)
  (unless (settings/packages-installed-p)
    (message "%s" "Refreshing package database...")
    (package-refresh-contents)
    (dolist (pkg settings/packages)
      (when (not (package-installed-p pkg))
        (package-install pkg)))))


;; evil
;; - vi emulation mode
(setq evil-want-C-u-scroll t)
(with-library evil
  (loop for (mode . state) in '((git-commit-mode . insert)
                                (git-rebase-mode . emacs)
                                (term-mode . emacs))
        do (evil-set-initial-state mode state))
  (define-key evil-motion-state-map (kbd "RET" ) nil)
  (define-key evil-normal-state-map (kbd "SPC u") 'universal-argument)
  (evil-mode 1)

  (with-library evil-surround
    (global-evil-surround-mode 1)))

;; fiplr
;; - fuzzy file search within version control
(with-library fiplr
  (global-set-key (kbd "C-x f") 'fiplr-find-file))

(with-library helm-config
  (setq helm-split-window-in-side-p t)
  (setq helm-ff-file-name-history-use-recentf t)
  (setq helm-M-x-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-buffer-max-length 40)
  (global-set-key (kbd "M-x")     'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x C-r") 'helm-recentf)
  (global-set-key (kbd "C-x b")   'helm-buffers-list)
  (helm-mode 1))

;; magit
;; - magical git interface
(with-library magit
  (global-set-key (kbd "C-c m") 'magit-status))

;; monokai-theme
;; - sublime-like theme
(with-library monokai-theme
  (load-theme 'monokai t))

;; rainbow-delimiters
;; - colorize pairs of brackets with different colors
(with-library rainbow-delimiters
  (global-rainbow-delimiters-mode))

;; smart-mode-line
;; - Vim's Powerline, now in Emacs!
(with-library smart-mode-line
  (sml/setup)
  (sml/apply-theme 'dark))

;; switch-window
;; - switch between windows using window numbers
(with-library switch-window
  (global-set-key (kbd "C-x o") 'switch-window))

;; undo-tree
;; - vim-like undo and redo functionality
(with-library undo-tree
  (global-undo-tree-mode))

;; windmove
;; - keybindings for moving between adjacent windows
(with-library windmove
  (windmove-default-keybindings))

;; window-numbering
;; - display windows' numbers in the status line (useful with switch-window)
(with-library window-numbering
  (window-numbering-mode))

;; yasnippet
;; - defining and expanding snippets
(with-library yasnippet
  (yas-global-mode 1))

