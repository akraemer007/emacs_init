(setq use-spacemacs nil)   ; or nil 

(cond ((eq window-system 'w32)
       (setq user-emacs-directory "c:/emacs/.emacs.d/"))
      ((eq window-system 'ns)
       (if use-spacemacs
           (setq user-emacs-directory "~/.spacemacs.d/")
         (setq user-emacs-directory "~/.emacs.d/"))))
(load (expand-file-name "init.el" user-emacs-directory))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(counsel-mode t)
 '(custom-safe-themes
   (quote
    ("2d035eb93f92384d11f18ed00930e5cc9964281915689fa035719cab71766a15" "2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "e964832f274625fa45810c688bdbe18caa75a5e1c36b0ca5ab88924756e5667f" "6bacece4cf10ea7dd5eae5bfc1019888f0cb62059ff905f37b33eec145a6a430" "d71aabbbd692b54b6263bfe016607f93553ea214bc1435d17de98894a5c3a086" "bc836bf29eab22d7e5b4c142d201bcce351806b7c1f94955ccafab8ce5b20208" "1ed5c8b7478d505a358f578c00b58b430dde379b856fbcb60ed8d345fc95594e" "8a0c35b74b0407ca465dd8db28c9136d5f539868d4867565ee214ac85ceb0d3a" "730a87ed3dc2bf318f3ea3626ce21fb054cd3a1471dcd59c81a4071df02cb601" "58c3313b4811ed8b30239b1d04816f74d438bcb72626d68181f294b115b7220d" "3577ee091e1d318c49889574a31175970472f6f182a9789f1a3e9e4513641d86" "7f6d4aebcc44c264a64e714c3d9d1e903284305fd7e319e7cb73345a9994f5ef" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "76c5b2592c62f6b48923c00f97f74bcb7ddb741618283bdb2be35f3c0e1030e3" default)))
 '(package-selected-packages
   (quote
    (poet-theme racket-mode wc-mode writeroom-mode powershell sound-wav org-journal org-super-agenda doom-themes evil-magit telephone-line shackle treemacs-evil treemacs nord-theme org-download zenburn-theme hy-mode lispyville persp-projectile counsel-projectile perspective yasnippet-snippets which-key use-package spell-fu projectile ox-pandoc org-pomodoro org-bullets magit lispy golden-ratio general flycheck flx evil-surround evil-numbers evil-commentary elpy color-theme-sanityinc-tomorrow))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
