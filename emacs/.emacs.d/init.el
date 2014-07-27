;; Emacs initialization file

;;;; Plugin / package setup


(load-file "~/.emacs.d/init-package.el")

;;;; Functions

;; Form for executing code only if a particular library is available
;; source: http://www.emacswiki.org/emacs/LoadingLispFiles
(defmacro with-library (symbol &rest body)
  `(condition-case nil
       (progn
	 (require ',symbol)
	 ,@body)
     (error (message (format "I guess we don't have %s available." ',symbol))
	    nil)))
(put 'with-library 'lisp-indent-function 1)

;;;; General Keybindings

(global-set-key (kbd "C-c f") 'ff-find-other-file)
(global-set-key (kbd "C-c c") (lambda ()
				(interactive)
				(pop-to-buffer "*compilation*")
				(compile (read-string "Compile command: " "make -k" ))))
