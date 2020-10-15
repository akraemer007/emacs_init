(if (eq window-system 'w32)
    (setq filepath "c:/emacs/.emacs.d/myinit.org")
  (setq filepath "~/.emacs.d/myinit.org"))

;; Package configs
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(org-babel-load-file (expand-file-name filepath))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (general which-key expand-region flycheck wc-mode writeroom-mode racket-mode hy-mode anaconda-mode company-jedi jedi elpy lispyville evil-commentary evil-surround yasnippet-snippets use-package telephone-line sound-wav shackle powershell ox-pandoc org-pomodoro org-journal org-download org-bullets magit golden-ratio gnu-elpa-keyring-update git-gutter flx evil-numbers evil doom-themes counsel-projectile))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )