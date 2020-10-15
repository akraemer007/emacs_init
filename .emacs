;; main purpose of this file is to point towards the init.el
;; the process goes .emacs -> init.el -> myinit.org
(setq use-spacemacs nil)   ; or nil 

(cond ((eq window-system 'w32)
       (setq user-emacs-directory "c:/emacs/.emacs.d/"))
      ((eq window-system 'ns)
       (if use-spacemacs
           (setq user-emacs-directory "~/.spacemacs.d/")
         (setq user-emacs-directory "~/.emacs.d/"))))
(load (expand-file-name "init.el" user-emacs-directory))
