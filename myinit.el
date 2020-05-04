;;; Code:
(setq workstation 'catapult)
(setq user-full-name "Andrew Kraemer")
(setq user-mail-address "andrew.h.kraemer@gmail.com")

;; Minimal UI
(scroll-bar-mode  -1)
(tool-bar-mode    -1)
(tooltip-mode     -1)
(blink-cursor-mode 0)
(setq initial-scratch-message nil)
(setq ring-bell-function 'ignore)
(setq redisplay-dont-pause t ;; Scrolling smoothness
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 10000
  scroll-preserve-screen-position 1)
(setq temporary-file-directory "c:/emacs/.emacs.d/tmp/") ;; custpath Don't save flycheck locally
;; set meta & super keys ;;cust
(setq w32-pass-lwindow-to-system nil)
(setq w32-lwindow-modifier 'super) ; Left Windows key

(fset 'yes-or-no-p 'y-or-n-p)

;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode 1)

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

;; files
(use-package files
  :init
  ;; Backup ~ files in seperate directory
  (setq backup-directory-alist '(("." . "c:/emacs/.emacs.d/backups"))) ;; custpath
  ;; No confirmation when creating new buffer
  (setq confirm-nonexistent-file-or-buffer nil))

;; Vim mode
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

(set-face-attribute 'default nil :family "Consolas" :height 110)

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  (load-theme 'sanityinc-tomorrow-eighties t))

(use-package golden-ratio
  :ensure t)

(use-package ivy
  :ensure t
  :init
  (setq ivy-use-virtual-buffers t
	enable-recursive-minibuffers t
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

(use-package magit
  :ensure t)

(use-package perspective
  :ensure t
  :config 
  (persp-mode))

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

(use-package persp-projectile
  :ensure t
  :after projectile
  :config
  ;; from amolgawai
  ;; (setq wg-morph-on nil ;; switch off animation
  ;;       persp-autokill-buffer-on-remove 'kill-weak
  ;;       persp-auto-save-opt 0
  ;;       persp-auto-resume-time -1
  ;;       persp-nil-hidden t
  ;;       persp-add-buffer-on-find-file t
  ;;       persp-add-buffer-on-after-change-major-mode t
  ;;       persp-hook-up-emacs-buffer-completion t
  ;;       ;; persp-state-default-file (locate-user-emacs-file "perspectives/default.persp"))
  ;;       persp-state-default-file (expand-file-name "perspectives/default.persp" user-emacs-directory))
  ;; (add-hook 'kill-emacs-hook #'persp-state-save)
  (persp-mode t))

;; (define-key org-agenda-mode-map "J" 'air-org-agenda-next-header)
;; (define-key org-agenda-mode-map "K" 'air-org-agenda-previous-header)
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(with-eval-after-load 'org (setq org-agenda-files
				'("c:/emacs/notes/")))

;; Org-Todos
(setq evil-org-key-theme '(textobjects navigation additional insert todo))
(setq org-todo-keywords
      (quote ((sequence "NEXT(n)" "TODO(t)" "WAITING(w@/)" "IN_PROGRESS(i)" "DONE(d)"))))

(setq org-refile-targets '(
                           (nil :maxlevel . 4)             ; refile to headings in the current buffer
                           (org-agenda-files :maxlevel . 4) ; refile to any of these files
                           ))

;; Org-Habits
(with-eval-after-load 'org
  (add-to-list 'org-modules 'org-habit t))
(setq org-habit-show-all-today t)

;; Org-Capture
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

      ("t" "To-do" entry (file+headline "c:/emacs/notes/gtd.org" "Inbox")
        "** TODO [#%^{priority}] %^{Task Description}" :prepend t)
      ("c" "To-do Link" entry (file+headline "c:/emacs/notes/gtd.org" "Inbox")
        "** TODO [#%^{priority}] %A \n:PROPERTIES:\n:Created: %U\n:Source: %a\n:END:\n%?"
	:prepend t)
      ("m" "Meeting" entry (file+headline "c:/emacs/notes/meetings.org" "Meeting Notes")
       ,my/org-meeting-template)
))

;; Org-Priority
(setq org-lowest-priority ?D)
(setq org-default-priority ?D)
(setq org-agenda-sorting-strategy
      '((agenda time-up priority-down tag-up category-keep effort-up)
        ;; (todo user-defined-up todo-state-up priority-down effort-up)
        (todo todo-state-up priority-down effort-up)
        (tags user-defined-up)
        (search category-keep)))

;; Org-Agenda custom view
;; https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
(defun air-org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.
IORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))
(setq org-agenda-custom-commands
      '(("d" "Daily agenda and all TODOs"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (agenda "test" ((org-agenda-ndays 1)
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
         )))

(defun air-org-agenda-next-header ()
"Jump to the next header in an agenda series."
  (interactive)
  (air--org-agenda-goto-header))

(defun air-org-agenda-previous-header ()
  "Jump to the previous header in an agenda series."
  (interactive)
  (air--org-agenda-goto-header t))

(defun air--org-agenda-goto-header (&optional backwards)
  "Find the next agenda series header forwards or BACKWARDS."
  (let ((pos (save-excursion
	       (goto-char (if backwards
                              (line-beginning-position)
                            (line-end-position)))
	       (let* ((find-func (if backwards
                                     'previous-single-property-change
                                   'next-single-property-change))
                      (end-func (if backwards
                                    'max
                                  'min))
                      (all-pos-raw (list (funcall find-func (point) 'org-agenda-structural-header)
                                         (funcall find-func (point) 'org-agenda-date-header)))
                      (all-pos (cl-remove-if-not 'numberp all-pos-raw))
                      (prop-pos (if all-pos (apply end-func all-pos) nil)))
                 prop-pos))))
    (if pos (goto-char pos))
    (if backwards (goto-char (line-beginning-position)))))

(defun air-pop-to-org-agenda (&optional split)
  "Visit the org agenda, in the current window or a SPLIT."
  (interactive "P")
  (org-agenda nil "d")
  (when (not split)
    (delete-other-windows)))

;; Org-Pomodoro ;; https://github.com/yanivdll/.emacs.d/blob/master/config.org
(use-package org-pomodoro
  :ensure t
  :commands (org-pomodoro)
  :config
  ;; (setq alert-user-configuration (quote ((((:category . "org-pomodoro")) libnotify nil))))
  )

(use-package org-download
  :ensure t
  :config
  ;; add support to dired
  (add-hook 'dired-mode-hook 'org-download-enable))

(org-babel-do-load-languages
'org-babel-load-languages
'((R . t)
    (python . t)))
;; put viz inline by default
(setq org-startup-with-inline-images t)


(defun insert-bable ()
  "Insert src_sections for viz in orgmode."
  (interactive)
  (insert "#+begin_src "
          (read-string "Enter Language (R, python, lisp): ")
          (if (equal (read-string "Return Viz (y/n) ") "y")
              " :results output graphics :file img.png"
            ""))
  (insert "\n \n#+end_src"))

(use-package ox-pandoc
  :ensure t
  :defer t
  )
;;End Orgmode;;

(use-package yasnippet
  :ensure t
  :defer 2
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

;; Python
(use-package elpy
  :ensure t
  :defer t
  :init
    (advice-add 'python-mode :before 'elpy-enable)
    (setq python-shell-interpreter "jupyter"
       python-shell-interpreter-args "console --simple-prompt"
       python-shell-prompt-detect-failure-warning nil)
    (pyvenv-activate "C:/Users/akraemer/Anaconda3/envs/py37")
  :config
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)) ;; don't use use flymake
    (add-hook 'elpy-mode-hook 'flycheck-mode) ;; use use flycheck instead
    (setq flycheck-python-flake8-executable "c:/Users/akraemer/Anaconda3/Scripts/flake8.exe") ;;custpath ;; note that flake8 config is in c:/Users/akraemer/.flake8
)

(use-package hy-mode
  :defer t
  :init (add-hook 'hy-mode-hook 'lispyville-mode))

;; Lispy
;;(use-package lisp;; y
;;   :ensure t
;;   :defer t
;;   :init
;;     (general-add-hook '(hy-mode-hook lisp-mode-hook emacs-lisp-mode-hook) #'lispy-mode)
;;     ;; (add-hook 'hy-mode-hook #'lispy-mode)
;;     ;; (add-hook 'lisp-mode-hook #'lispy-mode)
;;     ;; (add-hook 'emacs-lisp-mode-hook #'lispy-mode)
;;)

(use-package lispyville
  :ensure t
  :defer t
  :init
    (general-add-hook '(emacs-lisp-mode-hook hy-mode-hook lisp-mode-hook) #'lispyville-mode))
  :config
    (lispyville-set-key-theme '(additional prettify text-objects atom-motions additional-motions commentary slurp/barf-cp wrap additional))

(use-package tramp
  :ensure t
  :defer t
  :init
   (when (eq window-system 'w32)
     (setq tramp-default-method "plink")
     (setenv "PATH" (concat "c:/Program Files/PuTTY/" ";" (getenv "PATH")))))

;; eshell config
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

;; (use-package spell-fu
;;   :ensure t)
(setq-default ispell-program-name "C:/msys64/mingw64/bin/aspell.exe")

(use-package flycheck
  :ensure t)
(setq flymake-run-in-place nil) ;; don't save flymake locally

(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

;; Custom keybinding
(use-package general
  :ensure t
  :config (general-evil-setup) ;; let's me use general-*map keys
	  (general-nvmap
	    ;; replaces C-c with ,
	    "," (general-simulate-key "C-c"))
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
	    ;;projects
	    "p"   '(:ignore t :which-key "project")
	    "pc"  '(:keymap projectile-command-map :which-key "commands")
	    "pp"  '(projectile-persp-switch-project :which-key "switch project")
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
	    "bs"  '(ak-go-to-scratch :which-key "open scratch")
	    "bn"  '(switch-to-next-buffer :which-key "next buffer")
	    "bp"  '(switch-to-prev-buffer :which-key "prev buffer")
	    ;; "bd"  '(kill-buffer :which-key "delete buffer")
	    "bd"  '(kill-this-buffer :which-key "delete buffer")
	    "bk"  '(evil-delete-buffer :which-key "delete buffer and window")
	    ;; Window
	    "w"   '(:ignore t :which-key "window")
	    "wl"  '(windmove-right :which-key "move right")
	    "wh"  '(windmove-left :which-key "move left")
	    "wk"  '(windmove-up :which-key "move up")
	    "wj"  '(windmove-down :which-key "move bottom")
	    "w/"  '(split-window-right :which-key "split right")
	    "w-"  '(split-window-below :which-key "split bottom")
	    "wx"  '(delete-window :which-key "delete window")
	    "wg"  '(golden-ratio :which-key "golden ratio")
	    ;; Perspective (v for view)
	    "vn"  '(persp-next :which-key "previous next")
	    "vp"  '(persp-prev :which-key "previous perspective")
	    "vc"  '(:keymap perspective-map :which-key "commands")
	    ;; Org
	    "o"   '(:ignore t :which-key "org")
	    "ob"  '(insert-bable :Which-key "insert bable")
	    "oo"  '(air-pop-to-org-agenda :which-key "Open Agenda")
	    "oc"  '(org-capture :which-key "Org Capture")
	    ;; org-pomodoro
	    "op"  '(org-clock-in :which-key "Pomodoro Start")
	    "oP"  '(org-clock-out :which-key "Pomodoro Stop")
	    ;; Magit
	    "g"   '(:ignore t :which-key "magit")
	    "gs"  '(magit-status :which-key "magit status")
	    "ga"  '(magit-stage :which-key "magit add")
	    "gd"  '(magit-dispatch :which-key "magit dispatch")
	    "gi"  '(magit-gitignore :which-key "magit gitignore")
	    ;; Visual Toggles
	    "t"   '(:ignore t :which-key "ui toggle")
	    "tn"  '(display-line-numbers-mode :which-key "toggle line numbers")
	    "tl"  '(org-toggle-link-display :which-key "toggle how org links show")
	    "tL"  '(visual-line-mode :which-key "toggle line wrap")
	    "tc"  '(flycheck-mode :which-key "toggle flycheck")
	    "ts"  '(flyspell-mode :which-key "toggle flyspell")
	    "tj"  '(json-pretty-print-buffer :which-key "toggle json pretty-print")
	    ;; Flycheck
	    "c"   '(:ignore t :which-key "code check")
	    "cn"  '(flycheck-next-error :which-key "next error")
	    "cN"  '(flycheck-previous-error :which-key "previous error")
	    ;; Others
	    "at"  '(new-eshell :which-key "eshell"))
	  (general-define-key
	    :states '(normal visual insert emacs)
	    :prefix "C-c"
	    ;; Quick open files
	    "c"  '((lambda () (interactive) (find-file "c:/emacs/.emacs.d/myinit.org")) :which-key "open .emacs")
	    "o"  '((lambda () (interactive) (find-file "c:/emacs/notes/gtd.org")) :which-key "open org")
	    "n"  '((lambda () (interactive) (find-file "c:/emacs/notes/notes.org")) :which-key "open notes")
	    ;; winner undo / redo
	    "H"  '(winner-undo :which-key "winner undo")
	    "L"  '(winner-redo :which-key "winner redo")
	    ;; Vim  number increment
	    "C-="  '(evil-numbers/inc-at-pt :which-key "increment num")
	    "C--"  '(evil-numbers/dec-at-pt :which-key "decrement num"))
	  ;; org agenda (more options here: https://github.com/Somelauw/evil-org-mode/blob/master/evil-org-agenda.el)
	  (general-define-key
	     :keymaps 'org-agenda-mode-map
	     "j" 'org-agenda-next-line
	     "k" 'org-agenda-previous-line
	     "u" 'org-agenda-undo
	     "C" 'org-agenda-clock-in)
	  ;; Org C-c links
	  (general-define-key
	     :states '(normal)
	     :prefix "C-c"
	     :keymaps 'org-mode-map
	     "l" 'org-store-link)
	  (general-define-key
	     :keymaps 'elpy-mode-map
	     "C-c d" 'elpy-send-defun
	     "C-c C-a" 'elpy-goto-assignment)
	  ;; Org-Promote
	  (general-define-key
	     :keymaps 'org-mode-map
	     "M-l" 'org-do-demote
	     "M-h" 'org-do-promote
	     "M-L" 'org-demote-subtree
	     "M-H" 'org-promote-subtree
	     "M-k" 'org-move-subtree-up
	     "M-j" 'org-move-subtree-down))

(defun ak-go-to-buffer (buffer)
  "goes to buffer. If buffer does not exist, creates buffer"
  (if (not (get-buffer buffer))
      (generate-new-buffer buffer))
  (switch-to-buffer buffer))

(defun ak-go-to-scratch ()
  "runs ak-go-to-buffer for scratch file"
  (interactive)
  (ak-go-to-buffer "*buffer*"))

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

