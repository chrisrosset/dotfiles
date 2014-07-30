;; init-package.el
;;
;; This file is used for initializing package specific settings.

;; evil
;; - vi emulation mode
(setq evil-want-C-u-scroll t)
(with-library evil
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

;; smartparents
;; - smart handling of parentheses
(with-library smartparens
  (require 'smartparens-config)
  (smartparens-global-mode t))

;; yasnippet
;; - defining and expanding snippets
(with-library yasnippet
  (yas-global-mode 1))

