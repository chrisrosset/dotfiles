;; init-package.el
;;
;; This file is used for initializing package specific settings.

;; evil
;; - vi emulation mode
(with-library evil
  (loop for (mode . state) in '((git-commit-mode . insert)
                                (git-rebase-mode . emacs)
                                (term-mode . emacs))
        do (evil-set-initial-state mode state))
  (define-key evil-motion-state-map (kbd "RET" ) nil)
  (evil-mode 1))

;; fiplr
;; - fuzzy file search within version control
(with-library fiplr
  (global-set-key (kbd "C-x f") 'fiplr-find-file))

;; magit
(with-library magit
  (global-set-key (kbd "C-c m") 'magit-status))

;; rainbow-delimiters
;; - colorize pairs of brackets with different colors
(with-library rainbow-delimiters
  (global-rainbow-delimiters-mode))

;; smart-mode-line
;; - Vim's Powerline, now in Emacs!
(with-library smart-mode-line
  (sml/setup)
  (sml/apply-theme 'dark))

;; smartparents
;; - smart handling of parentheses
(with-library smartparens
  (require 'smartparens-config)
  (smartparens-global-mode t))
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

