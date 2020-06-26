(let ((file-name-handler-alist nil))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;;(setq debug-on-error t)

;; (when (require 'gcmh nil t)
;;   (gcmh-mode 1))

(setq gc-cons-threshold most-positive-fixnum)

(setq workstation 'catapult) ;; options: catapult, personal_macbook

(setq user-full-name "Andrew Kraemer")
(setq user-mail-address "andrew.h.kraemer@gmail.com")

(cond ((eq window-system 'w32)
       (setq ak/custpath-home "c:/Users/akraemer/"
	     ak/custpath-dropbox-org "c:/Users/akraemer/Dropbox/org/"
	     ak/custpath-phone-notes "c:/Users/akraemer/Dropbox/org/phone_inbox.org"
	     ak/custpath-journal-path "c:/Users/akraemer/Dropbox/journal/"
	     ak/custpath-emacsd "c:/emacs/.emacs.d/"
	     ak/custpath-org-notes "c:/emacs/notes/"
	     ak/custpath-py-default-env "c:/Users/akraemer/Anaconda3/envs/py37"
	     ak/custpath-flake8 "c:/Users/akraemer/Anaconda3/Scripts/flake8.exe"
	     ak/custpath-aspell "c:/msys64/mingw64/bin/aspell.exe"))
      ((eq window-system 'ns)
       (setq ak/custpath-home "/Users/AndrewKraemer/"
	     ak/custpath-dropbox-org "/Users/AndrewKraemer/Dropbox/org/"
	     ak/custpath-phone-notes "/Users/AndrewKraemer/Dropbox/org/phone_inbox.org"
	     ak/custpath-journal-path "/Users/AndrewKraemer/Dropbox/journal/"
	     ak/custpath-emacsd "~/.emacs.d/"
	     ak/custpath-org-notes "~/emacs/notes/"
	     ak/custpath-py-default-env "/Users/AndrewKraemer/anaconda3/envs/py37"
	     ak/custpath-aspell "/usr/local/bin/aspell")))

;;; Code:
;; Minimal UI
(scroll-bar-mode  -1)
(tool-bar-mode    -1)
(tooltip-mode     -1)
(blink-cursor-mode 0)
(fringe-mode 0)
(setq initial-scratch-message nil)
(setq ring-bell-function 'ignore)
(setq redisplay-dont-pause t ;; Scrolling smoothness
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 10000
  scroll-preserve-screen-position 1)
(setq temporary-file-directory (concat ak/custpath-emacsd "tmp")) ;; Don't save flycheck locally

;; standardizes all yes/no quetions to y/n.
(fset 'yes-or-no-p 'y-or-n-p)

;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode 1)

;;match parens automatically
(electric-pair-mode +1)

;; Pretty lambda symbol
(global-prettify-symbols-mode 1)

(when (eq window-system 'w32)
  (setq gc-cons-threshold (* 511 1024 1024))
  (setq gc-cons-percentage 0.5)
  (run-with-idle-timer 5 t #'garbage-collect)
  (setq garbage-collection-messages nil))

(cond ((eq window-system 'w32)
       (setq w32-pass-lwindow-to-system nil)
       (setq w32-lwindow-modifier 'super))
      ((eq window-system 'ns)
       (setq mac-command-modifier 'meta)
       (setq mac-option-modifier 'super)))

(use-package files
  :init
  (setq backup-directory-alist `(("." . ,(concat ak/custpath-emacsd "backups"))))
  (setq confirm-nonexistent-file-or-buffer nil))

(set-selection-coding-system
  (if (eq system-type 'windows-nt)
      'utf-16-le  ;; https://rufflewind.com/2014-07-20/pasting-unicode-in-emacs-on-windows
    'utf-8))

;; (set-face-attribute 'default nil :family "Consolas" :height 110)
;; (set-face-attribute 'default nil :family "Hack" :height 105)
(cond ((eq window-system 'w32)
       (set-face-attribute 'default nil
			   :family "Source Code Pro"
			   :weight 'Regular
			   ;; :weight 'bold
			   ;; :width 'normal
			   :height 107))
      ((eq window-system 'ns)
       (set-face-attribute 'default nil
			   :family "Source Code Pro"
			   :weight 'normal
			   ;; :weight 'bold
			   ;; :width 'normal
			   :height 140)))

(use-package doom-themes
  :ensure t
  :config (load-theme 'doom-nord t))

(use-package color-theme-sanityinc-tomorrow
  :disabled
  :ensure t
  :config
  (load-theme 'sanityinc-tomorrow-eighties t))

(use-package telephone-line
 :init (telephone-line-mode 1))

(use-package evil
  :ensure t
   ;; c-u to scroll up
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  (add-hook 'prog-mode-hook #'turn-on-evil-mode))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode))

(use-package evil-numbers
  :ensure t)

(use-package winner
  :init (setq winner-boring-buffers
        '("*Completions*"
          "*Compile-Log*"
          "*inferior-lisp*"
          "*Fuzzy Completions*"
          "*Apropos*"
          "*dvc-error*"
          "*Help*"
          "*cvs*"
          "*Buffer List*"
          "*Ibuffer*"))
  :config (winner-mode 1))

(use-package shackle
  :ensure t
  :defer t
  :config
  ;; (setq shackle-default-rule '(:select t))
  (shackle-mode t))

(use-package golden-ratio
  :ensure t)

(use-package ivy
  :ensure t
  :init
  (setq ivy-use-virtual-buffers t
	enable-recursive-minibuffers t
        ivy-initial-inputs-alist nil ;; don't use ^ w/ ivy M-x 
	ivy-re-builders-alist
	  '((swiper . regexp-quote)
	    (t      . ivy--regex-fuzzy)))
  :config
  (setq ivy-ignore-buffers '("\\` " "\\`\\*")) ;; hide star buffers note above
  (ivy-mode 1))

;; fuzzy matching for ivy
(use-package flx
  :ensure t)

(use-package counsel
  :ensure t)

(with-eval-after-load 'org (setq org-agenda-files
				`(,ak/custpath-org-notes ,ak/custpath-phone-notes)))

(with-eval-after-load 'org
  (add-to-list 'org-modules 'org-habit t))

(setq evil-org-key-theme '(textobjects navigation additional insert todo))

(setq org-todo-keywords
      (quote ((sequence "IN_PROGRESS(i)" "NEXT(n)" "TODO(t)" "WAITING(w@/)" "DONE(d)"))))

(setq org-refile-targets '((nil :maxlevel . 4)
			   (org-agenda-files :maxlevel . 4)))

(defvar my/org-meeting-template "** Meeting about %^{something}
  SCHEDULED: %<%Y-%m-%d %H:%M>
  *Attendees:*
  - [X] Nick Anderson
  - [ ] %?
  *Agenda:*
  -
  -
  *Notes:*
  ")

(setq org-capture-templates
    `(;; Note the backtick here, it's required so that the defvar based tempaltes will work!
      ;;http://comments.gmane.org/gmane.emacs.orgmode/106890

      ("t" "To-do" entry (file+headline ,(concat ak/custpath-org-notes "gtd.org") "Inbox")
	"** TODO [#%^{priority}] %^{Task Description}" :prepend t)
      ("c" "To-do Link" entry (file+headline ,(concat ak/custpath-org-notes "gtd.org") "Inbox")
	"** TODO [#%^{priority}] %A \n:PROPERTIES:\n:Created: %U\n:Source: %a\n:END:\n%?"
	:prepend t)
      ("m" "Meeting" entry (file+headline ,(concat ak/custpath-org-notes "meetings.org") "Meeting Notes")
       ,my/org-meeting-template)
))

(setq org-habit-show-all-today t)
(setq org-habit-show-habits-only-for-today t)
(setq org-agenda-show-future-repeats 'next)

(setq org-lowest-priority ?D)
(setq org-default-priority ?D)

(setq org-agenda-sorting-strategy
    '((agenda habit-up deadline-up scheduled-up priority-up) ;; show habits, then time, then by priority
      (tags todo-state-up deadline-up) ;; show todo-state then deadlines
      (search category-keep)))

(defun air-org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
      subtree-end
    nil)))

(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.
PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
      (pri-value (* 1000 (- org-lowest-priority priority)))
      (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
      subtree-end
    nil)))

(defvar current-date-format "%Y-%m-%d"
  "Format of date to insert with `insert-current-date-time' func
   See help of `format-time-string' for possible replacements")

(defun ak/org-skip-subtree-if-not-archived-today ()
  "Skip an agenda entry if it was not completed today"
  (concat ":ARCHIVE_TIME: " (format-time-string current-date-format (current-time))))

(setq org-agenda-custom-commands
      '(("d" "Daily agenda and all TODOs"
	 ((tags "PRIORITY=\"A\""
		((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
		 (org-agenda-overriding-header "High-priority unfinished tasks:")))
	  (agenda ""
		  ((org-agenda-span 'day)
		   (org-agenda-overriding-header "ALL normal priority tasks:")))
	  (tags (or "PRIORITY=\"B\"" "PRIORITY=\"C\"")
		((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
		 (org-agenda-overriding-header "Unfinished tasks:")))
	  (alltodo ""
		   ((org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
						   (air-org-skip-subtree-if-priority ?A)
						   (air-org-skip-subtree-if-priority ?B)
						   (org-agenda-skip-if nil '(scheduled deadline))))
		    (org-agenda-overriding-header "Eventually:"))))
	 ;; ((org-agenda-compact-blocks t)) ;; removes = breaks
	 )
	("r" "Day in review"
	   todo "DONE"
	   ;; agenda settings
	   ((org-agenda-files '("c:/emacs/notes/gtd.org_archive"))
	    (org-agenda-start-with-log-mode '(ARCHIVE_TIME))
	    (org-agenda-archives-mode t)
            (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp (ak/org-skip-subtree-if-not-archived-today)))
	    (org-agenda-overriding-header "Day in Review"))
	   ;; ("~/org/review/day.html") ;; for export
		)))

(defun air-pop-to-org-agenda (&optional split)
  "Visit the org agenda, in the current window or a SPLIT."
  (interactive "P")
  (org-agenda nil "d")
  (when (not split)
  (delete-other-windows)))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; https://github.com/yanivdll/.emacs.d/blob/master/config.org
(use-package org-pomodoro
  :ensure t
  :config (setq org-pomodoro-play-sounds 1
		org-pomodoro-finished-sound "c:/Users/akraemer/Dropbox/org/sounds/tone.wav"
		org-pomodoro-long-break-sound "c:/Users/akraemer/Dropbox/org/sounds/tone.wav"
		org-pomodoro-short-break-sound "c:/Users/akraemer/Dropbox/org/sounds/tone.wav"))

;;https://emacs.stackexchange.com/a/48352
;; required for org-sounds
(use-package sound-wav
  :ensure t)

;; ;; required for sound if on windows
(use-package powershell
  :if (memq window-system '(w32))
  :ensure t)

(use-package org-download
  :ensure t
  :config
  ;; add support to dired
  (add-hook 'dired-mode-hook 'org-download-enable))

(use-package org-journal
 :ensure t
 :defer t
 :custom
 (org-journal-dir ak/custpath-journal-path)
 (org-journal-date-format "%Y-%m-%d"))

(org-babel-do-load-languages
'org-babel-load-languages
'((R . t)
    (python . t)))
;; put viz inline by default
(setq org-startup-with-inline-images t)

(use-package ox-pandoc
  :ensure t
  :defer t)

 ;; pulled form my spacemacs for latex
 ;; (setenv "PATH" (concat (getenv "PATH") ":/sw/bin"))
 ;; (setq exec-path (append exec-path '("/sw/bin")))

(use-package yasnippet
  :ensure t
  :defer 2
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

(use-package company
  :hook
  (after-init . global-company-mode)
  :bind
  ;; make company completion work w/ vimkeys
  (:map company-active-map)
  ("C-n" . company-select-next-or-abort)
  ("C-p" . company-select-previous-or-abort))

(use-package magit
  :ensure t)

(use-package evil-magit
  :after magit)

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode 't)
  :diminish git-gutter-mode)

(use-package projectile
  :ensure t
  :init
  (setq projectile-require-project-root nil)
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-mode 1))

(use-package counsel-projectile
 :ensure t
 :config
 (counsel-projectile-mode))

(use-package lisp;; y
  :disabled
  :ensure t
  :defer t
  :init
    (general-add-hook '(hy-mode-hook lisp-mode-hook emacs-lisp-mode-hook) #'lispy-mode)
    ;; (add-hook 'hy-mode-hook #'lispy-mode)
    ;; (add-hook 'lisp-mode-hook #'lispy-mode)
    ;; (add-hook 'emacs-lisp-mode-hook #'lispy-mode)
)

(use-package lispyville
  :ensure t
  :defer t
  :init
    (general-add-hook '(emacs-lisp-mode-hook hy-mode-hook lisp-mode-hook) #'lispyville-mode))
  :config
    (lispyville-set-key-theme '(additional prettify text-objects atom-motions additional-motions commentary slurp/barf-cp additional-wrap))

;; Python
(use-package elpy
  :if (eq window-system 'w32)
  :ensure t
  :defer t
  :init
    (advice-add 'python-mode :before 'elpy-enable)

    ;; windows setup
    (when (eq window-system 'w32)
      (pyvenv-activate ak/custpath-py-default-env)
      (setq python-shell-interpreter "jupyter"
	python-shell-interpreter-args "console --simple-prompt"
	python-shell-prompt-detect-failure-warning nil))
    ;; couldn't get this to work on mac
    ;; (when (eq window-system 'ns)
    ;;   (pyvenv-activate "~/anaconda3/envs/py37")
    ;;   (setq python-shell-interpreter-args "console --simple-prompt"
    ;; 	python-shell-prompt-detect-failure-warning nil))
    :config
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)) ;; don't use use flymake
    (add-hook 'elpy-mode-hook 'flycheck-mode) ;; use use flycheck instead
    (when (eq window-system 'w32)
      (setq flycheck-python-flake8-executable ak/custpath-flake8))  ;; Need to install flake8 explicitly on windows
)

(use-package jedi
 :ensure t
 :config
 (use-package company-jedi
   :ensure t
   :init
   (add-hook 'python-mode-hook (lambda () (add-to-list 'company-backends 'company-jedi)))
   (setq company-jedi-python-bin "python"))
 (add-to-list 'company-backends 'company-jedi))

(use-package anaconda-mode
  :if (eq window-system 'ns)
  :ensure t
  :defer t
  :bind
   (:map company-active-map)
   ("e" . pythonic-activate)
  :init (add-hook 'python-mode-hook 'anaconda-mode)
	(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  :config
    ;; (pythonic-activate "~/anaconda3/envs/py37")
    (pythonic-activate "~/anaconda3/envs/ml4t") ;; for class
    (use-package company-anaconda
      :ensure t
      :init (add-hook 'python-mode-hook 'anaconda-mode)
      (eval-after-load "company"
	'(add-to-list 'company-backends '(company-anaconda :with company-capf)))))

(use-package hy-mode
  :ensure t
  :defer t
  :init (add-hook 'hy-mode-hook 'lispyville-mode))

(use-package racket-mode
  :ensure t
  :defer t
  :init
    (add-hook 'racket-mode-hook 'lispyville-mode)
    (if (eq window-system 'w32) (setq racket-program "c:/Program Files/Racket/Racket.exe")))

(use-package writeroom-mode
  :ensure t)

(use-package wc-mode
  :ensure t)

(setq-default ispell-program-name ak/custpath-aspell)

(use-package flycheck
  :init  (setq flymake-run-in-place nil) ;; don't save flymake locally
  :ensure t)

(defun new-eshell ()
  "Open eshell on bottom of screen."
  (interactive)
  (when (one-window-on-screen-p)
    (let* ((lines (window-body-height))
	   (new-window (split-window-vertically (floor (* 0.7 lines)))))
      (select-window new-window)
      (eshell "eshell"))))

(defun one-window-on-screen-p ()
  "Check if there is only one buffer on the screen."
  (= (length (window-list)) 1))

(use-package tramp
  :ensure t
  :defer t
  :init
   (when (eq window-system 'w32)
     (setq tramp-default-method "plink")
     (setenv "PATH" (concat "c:/Program Files/PuTTY/" ";" (getenv "PATH")))))

(use-package expand-region 
  :ensure t)

(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

(use-package general
  :ensure t
  :config (general-evil-setup) ;; let's me use general-*map keys
	  (general-nvmap
	    ;; replaces C-c with ,
	    "," (general-simulate-key "C-c"))
	  (general-define-key
	    :states '(normal visual)
	    ;; use visual line movement w/ j/k
	    "j" 'evil-next-visual-line
	    "k" 'evil-previous-visual-line
	    ;; globally define 
	    "C-=" 'er/expand-region
	    "<f5>" 'webjump)
	  (general-define-key
	    :states '(normal viusal)
	    :prefix "g"
	    ;; bind gj and gk
	    "j" 'evil-next-line
	    "k" 'evil-previous-line)
	  (general-define-key
	    :states '(normal visual insert emacs)
	    :prefix "SPC"
	    :non-normal-prefix "M-SPC"
	    "/"  '(swiper :which-key "swiper") ; You'll need counsel package for this ;; consider counsel-git-grep
	    "\\"  '(counsel-rg :which-key "ripgrep") ; You'll need counsel package for this ;; consider counsel-git-grep
	    "TAB" '(spacemacs/alternate-window :which-key "alternate buffer")
	    "SPC" '(counsel-M-x :which-key "M-x")
	    "f"   '(:ignore t :which-key "files")
	    "ff"  '(counsel-find-file :which-key "find files")
	    "fr"  '(counsel-recentf :which-key "recent files")
	    "fs"  '(save-buffer :which-key "save buffer")
	    "ft"  '(ak/ivy-tramp-find-file :which-key "find tramp files")
	    "fh"  '(ak/ivy-home-find-file :which-key "find home files")
	    ;;projects
	    "p"   '(:ignore t :which-key "project")
	    "pc"  '(:keymap projectile-command-map :which-key "commands")
	    "pp"  '(projectile-switch-project :which-key "switch project")
	    "pb"  '(counsel-projectile-switch-to-buffer :which-key "find project file")
	    "pf"  '(counsel-projectile-find-file :which-key "find project file")
	    "pg"  '(projectile-grep :which-key "grep project")
	    "pk"  '(projectile-kill-buffers :which-key "kill all buffers in project")
	    ;; eval
	    "e"   '(:ignore t :which-key "evaluate")
	    "ee"  '(eval-last-sexp :which-key "last expression")
	    "eE"  '(eval-expression :which-key "expression")
	    "eb"  '(eval-buffer :which-key "buffer")
	    "er"  '(eval-region :which-key "region")
	    ;; Buffers
	    "b"   '(:ignore t :which-key "buffers")
	    "bb"  '(ivy-switch-buffer :which-key "buffers list")
	    "bs"  '(ak/go-to-scratch :which-key "open scratch")
	    "bn"  '(switch-to-next-buffer :which-key "next buffer")
	    "bp"  '(switch-to-prev-buffer :which-key "prev buffer")
	    "bd"  '(kill-this-buffer :which-key "delete buffer")
	    "bk"  '(evil-delete-buffer :which-key "delete buffer and window")
	    "bq"  '(ak/save-exit-buffer-list :which-key "quit gtd-files")
	    ;; Window
	    "w"   '(:ignore t :which-key "window")
	    "wl"  '(windmove-right :which-key "move right")
	    "wh"  '(windmove-left :which-key "move left")
	    "wk"  '(windmove-up :which-key "move up")
	    "wj"  '(windmove-down :which-key "move bottom")
	    "wL"  '(evil-window-move-far-right :which-key "shift window right")
	    "wH"  '(evil-window-move-far-left :which-key "shift window left")
	    "wK"  '(evil-window-move-very-top :which-key "shift window up")
	    "wJ"  '(evil-window-move-very-bottom :which-key "shift window bottom")
	    "wv"  '(split-window-right :which-key "split right")
	    "ws"  '(split-window-below :which-key "split bottom")
	    "wo"  '(delete-other-windows :which-key "delete other windows")
	    "wd"  '(delete-window :which-key "delete window")
	    "wg"  '(golden-ratio :which-key "golden ratio")
	    "wc"  '(evil-window-delete :which-key "delete window")
	    "wu"  '(winner-undo :which-key "winner undo")
	    "wU"  '(winner-redo :which-key "winner redo")
	    "ww"  '(writeroom-mode :which-key "writeroom mode")
	    ;; v for view
	    "v"   '(:ignore t :which-key "view")
	    "vc"  '(ivy-push-view :which-key "create view")
	    "vv"  '(ivy-switch-view :which-key "switch view")
	    "vs"  '(ak/save-ivy-views :which-key "save views")
	    "vl"  '(ak/load-ivy-views :which-key "load views")
	    ;; Org
	    "o"   '(:ignore t :which-key "org")
	    "ob"  '(ak/insert-bable :Which-key "insert bable")
	    "oo"  '(air-pop-to-org-agenda :which-key "open standard agenda")
	    "or"  '(org-agenda :which-key "open review agenda")
	    "oc"  '(org-capture :which-key "org capture")
	    "oj"  '(org-journal-new-entry :which-key "org journal")
	    "op"  '(org-pomodoro :which-key "pomodoro start")
	    "oP"  '(org-clock-out :which-key "pomodoro stop")
	    ;; git
	    "g"   '(:ignore t :which-key "git")
	    "gs"  '(magit-status :which-key "magit status")
	    "ga"  '(magit-stage :which-key "magit add")
	    "gd"  '(magit-dispatch :which-key "magit dispatch")
	    "gi"  '(magit-gitignore :which-key "magit gitignore")
	    "gn"  '(git-gutter:next-hunk :which-key "next hunk")
	    "gp"  '(git-gutter:previous-hunk :which-key "prev hunk")
	    ;; Visual Toggles
	    "t"   '(:ignore t :which-key "ui toggle")
	    "tn"  '(display-line-numbers-mode :which-key "toggle line numbers")
	    "tL"  '(org-toggle-link-display :which-key "toggle how org links show")
	    "ti"  '(org-toggle-inline-images :which-key "toggle how org links show")
	    "tl"  '(visual-line-mode :which-key "toggle line wrap")
	    "tc"  '(flycheck-mode :which-key "toggle flycheck")
	    "ts"  '(flyspell-mode :which-key "toggle flyspell")
	    "tj"  '(json-pretty-print-buffer :which-key "toggle json pretty-print")
	    ;; Flycheck
	    "c"   '(:ignore t :which-key "code check")
	    "cn"  '(flycheck-next-error :which-key "next error")
	    "cp"  '(flycheck-previous-error :which-key "previous error")
	    ;; Snippets
	    "s"   '(:ignore t :which-key "code check")
	    "ss"  '(yas-insert-snippet :which-key "next error")
	    "sn"  '(yas-new-snippet :which-key "previous error")
	    ;; Others
	    "at"  '(new-eshell :which-key "eshell"))
	  (general-define-key
	    :states '(normal visual insert emacs)
	    :prefix "C-c"
	    ;; Quick open files
	    "c"  '((lambda () (interactive) (find-file (concat ak/custpath-emacsd "myinit.org"))) :which-key "open .emacs")
	    "o"  '((lambda () (interactive) (find-file (concat ak/custpath-org-notes "gtd.org"))) :which-key "open org")
	    "n"  '((lambda () (interactive) (find-file (concat ak/custpath-org-notes "worknotes.org"))) :which-key "open notes")
	    "N"  '((lambda () (interactive) (find-file (concat ak/custpath-dropbox-org "notes.org"))) :which-key "open notes")
	    ;; Vim  number increment
	    "C-="  '(evil-numbers/inc-at-pt :which-key "increment num")
	    "C--"  '(evil-numbers/dec-at-pt :which-key "decrement num"))
	  ;; org agenda (more options here: https://github.com/Somelauw/evil-org-mode/blob/master/evil-org-agenda.el)
	  (general-define-key
	     :keymaps 'org-agenda-mode-map
	     "j" 'org-agenda-next-line
	     "k" 'org-agenda-previous-line
	     "u" 'org-agenda-undo
	     "n" 'org-agenda-capture
	     "p" 'org-pomodoro
	     "C" 'org-agenda-clock-in)
	  ;; Org C-c links
	  (general-define-key
	     :states '(normal)
	     :prefix "C-c"
	     :keymaps 'org-mode-map
	     "l" 'org-store-link)
	  ;; C-w & C-d conflicted w/ the racket repl. This allows the standard evil bindings for function properly.
	  (general-unbind 'racket-repl-mode-map
	     "C-w"
	     "C-d")
	  (general-define-key
	     :keymaps 'elpy-mode-map
	     "C-c d" 'elpy-send-defun
	     "C-c C-a" 'elpy-goto-assignment)
	  (general-define-key
	     :keymaps 'org-journal-mode-map
	     "C-c C-c" 'ak/save-close-window)
	  (general-define-key
	     :keymaps 'org-mode-map
	      ;; Org-Promote
	     "M-l" 'org-do-demote
	     "M-h" 'org-do-promote
	     "M-L" 'org-demote-subtree
	     "M-H" 'org-promote-subtree
	     "M-k" 'org-move-subtree-up
	     "M-j" 'org-move-subtree-down
	     "C-c f" 'ak/org-focus-subtree
	     "C--" 'help/insert-em-dash
	     "M--" 'help/insert-en-dash
	     "C-M-y" 'insert-char))

(defhydra hydra-zoom (global-map "<f2>")
 "zoom"
 ("+" text-scale-increase "in")
 ("-" text-scale-decrease "out"))

(defhydra hydra-winner (global-map "<f2>")
 "Winner"
 ("u" winner-undo "in")
 ("U" winner-redo "out"))

(defhydra hydra-buffer-menu (:color pink
			     :hint nil)
  "
^Mark^             ^Unmark^           ^Actions^          ^Search
^^^^^^^^-----------------------------------------------------------------
_m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch
_s_: save          _U_: unmark up     _b_: bury          _I_: isearch
_d_: delete        ^ ^                _g_: refresh       _O_: multi-occur
_D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only
_~_: modified
"
  ("m" Buffer-menu-mark)
  ("u" Buffer-menu-unmark)
  ("U" Buffer-menu-backup-unmark)
  ("d" Buffer-menu-delete)
  ("D" Buffer-menu-delete-backwards)
  ("s" Buffer-menu-save)
  ("~" Buffer-menu-not-modified)
  ("x" Buffer-menu-execute)
  ("b" Buffer-menu-bury)
  ("g" revert-buffer)
  ("T" Buffer-menu-toggle-files-only)
  ("O" Buffer-menu-multi-occur :color blue)
  ("I" Buffer-menu-isearch-buffers :color blue)
  ("R" Buffer-menu-isearch-buffers-regexp :color blue)
  ("c" nil "cancel")
  ("v" Buffer-menu-select "select" :color blue)
  ("o" Buffer-menu-other-window "other-window" :color blue)
  ("q" quit-window "quit" :color blue))
;; (define-key Buffer-menu-mode-map "." 'hydra-buffer-menu/body)

(defun ak/go-to-buffer (buffer)
  "goes to buffer. If buffer does not exist, creates buffer"
  (if (not (get-buffer buffer))
      (generate-new-buffer buffer))
  (switch-to-buffer buffer))

(defun ak/go-to-scratch ()
  "runs ak/go-to-buffer for scratch file"
  (interactive)
  (ak/go-to-buffer "*buffer*"))

(defun spacemacs/alternate-window (&optional window)
  (interactive)
  (let ((current-buffer (window-buffer window)))
    ;; if no window is found in the windows history, `switch-to-buffer' will
    ;; default to calling `other-buffer'
    (switch-to-buffer
     (cl-find-if (lambda (buffer)
		   (not (eq buffer current-buffer)))
		 (mapcar #'car (window-prev-buffers window)))
     nil t)))

(defun ak/insert-bable ()
  "Insert src_sections for viz in orgmode."
  (interactive)
  (insert "#+begin_src "
	  (read-string "Enter Language (R, python, lisp): ")
	  (if (equal (read-string "Return Viz (y/n) ") "y")
	      " :results output graphics :file img.png"
	    ""))
  (insert "\n \n#+end_src"))

(defun ak/ivy-tramp-find-file ()
  "find-file with Tramp. Ex: '/plink:bort:~/'. See tramp note to setup"
  (interactive)
  (let ((tramp-path (concat "/" tramp-default-method ":")))
    (counsel-find-file tramp-path)))

(defun ak/ivy-home-find-file ()
  "open counsel-find-file in the home directory"
  (interactive)
  (counsel-find-file ak/custpath-home))

(defun ak/save-close-window ()
  "save, kill buffer, and kill window of focused file"
  (interactive)
  (save-buffer)
  (kill-buffer)
  (delete-window))

(defun ak/org-focus-subtree ()
  "Toggle org narrow subtreee / show everything"
  (interactive)
  (if (buffer-narrowed-p)
      (widen)
    (org-narrow-to-subtree)))

(setq gtd-buffer-list '("inbox.org" "gtd.org" "gtd.org_archive" "notes.org" "notes.org_archive" "phone_inbox.org")) ;; custpath

(defun ak/delete-buffer-list (buffer)
  "Delete buffer if exists"
  (when (get-buffer buffer)
    (kill-buffer buffer)))

(defun ak/save-exit-buffer-list ()
  "save all buffers in list then close them. Used for keeping gtd from conflicting on multiple machines"
  (interactive)
  (save-some-buffers gtd-buffer-list)
  (mapcar #'ak/delete-buffer-list gtd-buffer-list))

(defun help/insert-em-dash ()
  "Inserts an EM-DASH (not a HYPEN, not an N-DASH)"
  (interactive)
  (insert "¡X"))
(defun help/insert-en-dash ()
  "Inserts an EN-DASH (not a HYPEN, not an EM-DASH)"
  (interactive)
  (insert "¡V"))

(setq gc-cons-threshold (* 2 1000 1000))
)
