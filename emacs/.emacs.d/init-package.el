;; init-package.el
;;
;; This file is used for initializing package specific settings.

(require 'package)

(add-to-list 'package-archives
             '("melpa"     . "http://melpa.org/packages/")  t)

(package-initialize)

(unless (package-installed-p 'use-package)
        (message "Installing use-package")
        (package-refresh-contents)
        (package-install 'use-package))

(eval-when-compile
    (setq use-package-always-ensure t)
    (require 'use-package)
    (require 'bind-key))

;; ag
;; - Silver Searcher
(use-package ag
  :bind (("C-c a g" . ag)))

;; avy
;; - fast jumping around txt using labels
(use-package avy
  :bind (("C-c a j" . avy-goto-line)
         ("C-c a k" . avy-goto-line)
         ("C-c a w" . avy-goto-word-1)
         ("C-c a e" . avy-goto-word-1)
         ("C-c a h" . avy-goto-char-2)
         ("C-c a l" . avy-goto-char-2)))

;; evil
;; - vi emulation mode
(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (define-key evil-motion-state-map (kbd "RET" ) nil)
  (define-key evil-normal-state-map (kbd "SPC u") 'universal-argument)
  (add-hook 'with-editor-mode-hook 'evil-insert-state)
  (let (mode)
    (dolist (mode '((term-mode . emacs)
                    (git-timemachine-mode . emacs)))
            (evil-set-initial-state (car mode) (cdr mode))))
  (evil-mode 1))

;; evil-anzu
;; - display current match and the total number of matches in the modeline
(use-package evil-anzu
  :config
  (global-anzu-mode))


(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; fiplr
;; - fuzzy file search within version control
(use-package fiplr
  :config
  (global-set-key (kbd "C-x f") 'fiplr-find-file))

(use-package flx-ido
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode 1)
  (setq ido-use-faces nil) ; disable ido faces to see flx highlights.
  (setq ido-enable-flex-matching t))

(use-package git-timemachine
  :config
  (defadvice git-timemachine-mode (after toggle-evil activate)
    ; "Turn off `evil-local-mode' when enabling
    ; `git-timemachine-mode', and turn it back on when disabling
    ; `git-timemachine-mode'."
    (evil-local-mode (if git-timemachine-mode -1 1))))

(use-package helm
  :init
  (require 'helm-config)
  (setq helm-split-window-in-side-p t)
  (setq helm-ff-file-name-history-use-recentf t)
  (setq helm-M-x-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-buffer-max-length 40)
  :config
  (global-set-key (kbd "M-x")     'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x C-r") 'helm-recentf)
  (global-set-key (kbd "C-x b")   'helm-buffers-list)
  (helm-mode 1))

;; js2-mode
;; - better javascript mode
(use-package js2-mode
  :config
  (setq js2-basic-offset 4)
  (add-hook 'js-mode-hook 'js2-minor-mode))

;; magit
;; - magical git interface
(use-package magit
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  ;; https://github.com/magit/magit/issues/2388
  (setq magit-diff-expansion-threshold 999.0)
  :config
  (global-set-key (kbd "C-c m") 'magit-status))

;; monokai-theme
;; - sublime-like theme
(use-package monokai-theme
  :config
  (load-theme 'monokai t))

;; projectile
;; - indexing and searching engine
(use-package projectile
  :config
  (setq projectile-indexing-method 'native))

;; rainbow-delimiters
;; - colorize pairs of brackets with different colors
(use-package rainbow-delimiters
  :config
  (rainbow-delimiters-mode))

;; smart-mode-line
;; - Vim's Powerline, now in Emacs!
(use-package smart-mode-line
  :config
  (sml/setup)
  (sml/apply-theme 'dark))

;; switch-window
;; - switch between windows using window numbers
(use-package switch-window
  :config
  (global-set-key (kbd "C-x o") 'switch-window))

;; undo-tree
;; - vim-like undo and redo functionality
(use-package undo-tree
  :config
  (global-undo-tree-mode))

;; windmove
;; - keybindings for moving between adjacent windows
(use-package windmove
  :config
  (windmove-default-keybindings))

;; window-numbering
;; - display windows' numbers in the status line (useful with switch-window)
(use-package window-numbering
  :config
  (window-numbering-mode))

;; yasnippet
;; - defining and expanding snippets
(use-package yasnippet
  :config
  (yas-global-mode 1)
  (add-hook 'term-mode-hook (lambda()
                              (yas-minor-mode -1))))

