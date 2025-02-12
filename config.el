;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;____________________________________________________________
;;

;; doom theme (doom-one, doom-dracula)
(setq doom-theme 'doom-dracula
      doom-font (font-spec :family "JetBrains Mono" :size 14 :weight 'light)
      doom-variable-pitch-font (font-spec :family "JetBrains Mono" :size 14))

;; Password cache
(setq password-cache t)
(setq password-cache-expiry nil)

;; Start fullscreen
(add-hook 'window-setup-hook #'toggle-frame-fullscreen)

;; Remove duplicates in commands history
(setq history-delete-duplicates t)

;; For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; tell projectile not to register projects automatically
(setq projectile-track-known-projects-automatically nil)

;; Minimal UI
(setq menu-bar-mode nil)
(setq tool-bar-mode nil)
(setq scroll-bar-mode nil)

;; don't yank on paste
(setq evil-kill-on-visual-paste nil)

;; set auth-sources
(setq auth-sources
      '((:source "~/.authinfo.gpg")))

;; the default is too high and makes vterm to slow
(setq vterm-timer-delay 0.01)

;; Prevents some cases of Emacs flickering.
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; Manual auto complete
;; (after! corfu
;;   (setq corfu-auto nil))

;; An evil mode indicator is redundant with cursor shape
(setq doom-modeline-modal nil)

;; Focus new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; add clipboard to the kill-ring before next kill
(setq save-interprogram-paste-before-kill t)

;;____________________________________________________________
;; lsp configs
(after! lsp
  :config
  ;; disable code underlines/flychecks
  ;; (setq lsp-diagnostics-provider :none)
  (setq lsp-headerline-breadcrumb-enable t))

;;____________________________________________________________
;;lsp-headerline remove underlines
;; generated automatically by running the following commands
;; M-x -> list-faces-display
;; search for "lsp-headerline"
;; click on the face hyperlink
;; turn off underline
;; the output is saved here to make it permanent.
(custom-set-faces
 '(lsp-headerline-breadcrumb-path-error-face ((t (:inherit lsp-headerline-breadcrumb-path-face :underline nil))))
 '(lsp-headerline-breadcrumb-path-hint-face ((t (:inherit lsp-headerline-breadcrumb-path-face :underline nil))))
 '(lsp-headerline-breadcrumb-path-info-face ((t (:inherit lsp-headerline-breadcrumb-path-face :underline nil))))
 '(lsp-headerline-breadcrumb-path-warning-face ((t (:inherit lsp-headerline-breadcrumb-path-face :underline nil))))
 '(lsp-headerline-breadcrumb-path-error-face ((t (:inherit lsp-headerline-breadcrumb-path-face :underline nil))))
 '(lsp-headerline-breadcrumb-path-hint-face ((t (:inherit lsp-headerline-breadcrumb-path-face :underline nil))))
 '(lsp-headerline-breadcrumb-path-info-face ((t (:inherit lsp-headerline-breadcrumb-path-face :underline nil))))
 '(lsp-headerline-breadcrumb-path-warning-face ((t (:inherit lsp-headerline-breadcrumb-path-face :underline nil))))
 '(lsp-headerline-breadcrumb-symbols-error-face ((t (:inherit lsp-headerline-breadcrumb-symbols-face :underline nil))))
 '(lsp-headerline-breadcrumb-symbols-hint-face ((t (:inherit lsp-headerline-breadcrumb-symbols-face :underline nil))))
 '(lsp-headerline-breadcrumb-symbols-info-face ((t (:inherit lsp-headerline-breadcrumb-symbols-face :underline nil))))
 '(lsp-headerline-breadcrumb-symbols-warning-face ((t (:inherit lsp-headerline-breadcrumb-symbols-face :underline nil)))))

;;____________________________________________________________
;; modeline configs
(after! doom-modeline
  (remove-hook 'doom-modeline-mode-hook #'size-indication-mode) ; filesize in modeline
  (remove-hook 'doom-modeline-mode-hook #'column-number-mode)   ; cursor column in modeline
  (add-hook 'doom-modeline-mode-hook #'display-time-mode)
  (setq line-number-mode nil)
  (setq doom-modeline-persp-name t)
  (setq doom-modeline-buffer-encoding nil))

;;____________________________________________________________
;; ibuffer configs
(setq ibuffer-expert t)  ;; no prompts
(add-hook 'ibuffer-hook
          (lambda ()
            (ibuffer-projectile-set-filter-groups)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))

;;___(setq +format-with-lsp nil)
(setq-default indent-tabs-mode nil)

;; add spacing between windows
(modify-all-frames-parameters
 '((right-divider-width . 5)
   (bottom-divider-width . 5)))

;;____________________________________________________________
;; org configs
(setq org-directory "~/org/"
      org-roam-directory (file-name-concat org-directory "org-roam/")
      org-roam-db-location (file-name-concat org-directory ".org-roam.db")
      org-roam-dailies-directory (file-name-concat org-directory "journal/")
      org-archive-location (file-name-concat org-directory ".archive/%s::")
      org-agenda-files (list org-directory))

(after! org
  (setq org-startup-folded 'show2levels
        org-hide-emphasis-markers t
        org-startup-folded t
        org-agenda-timegrid-use-ampm 1 ;; Use 12-hour clock instead of 24-hour in agenda view
        org-ellipsis " [...] "
        org-capture-templates
        '(("t" "todo" entry (file+headline "todo.org" "Inbox")
           "* [ ] %?\n%i\n%a"
           :prepend t)
          ("d" "deadline" entry (file+headline "todo.org" "Inbox")
           "* [ ] %?\nDEADLINE: <%(org-read-date)>\n\n%i\n%a"
           :prepend t)
          ("s" "schedule" entry (file+headline "todo.org" "Inbox")
           "* [ ] %?\nSCHEDULED: <%(org-read-date)>\n\n%i\n%a"
           :prepend t)
          ("c" "check out later" entry (file+headline "todo.org" "Check out later")
           "* [ ] %?\n%i\n%a"
           :prepend t)
          )))

;; enable auto fill for org
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'markdown-mode-hook 'turn-on-auto-fill)

;;____________________________________________________________
;; org-roam configs
(after! org-roam
  (org-roam-db-sync)
  (setq org-roam-db-autosync-mode t
        org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:10}" 'face 'org-tag))))

;;____________________________________________________________
;; org-modern
(use-package! org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-fold-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-ellipsis "…"

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   ))

;;____________________________________________________________
;; terraform lsp
(add-hook! 'terraform-mode-hook #'lsp)
(setq lsp-disabled-clients '(tfls)) ; go with the official Hashicorp's language server
(setq lsp-terraform-ls-enable-show-reference t)
(setq lsp-semantic-tokens-enable t)
(setq lsp-semantic-tokens-honor-refresh-requests t)
(setq lsp-enable-links t)
(setq lsp-terraform-ls-prefill-required-fields t)

;;____________________________________________________________
;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :config
  (setq copilot-indent-offset-warning-disable t)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

;;____________________________________________________________
;; eglot-java
(use-package! eglot-java
  :hook (java-mode . eglot-java-mode))

;;____________________________________________________________
;; load key bindings
(load-file "~/.doom.d/keybinding.el")

;;_________________________________________________________
;; some helper functions

;; insert a new line right after the cursor's position
(defun my/open-line ()
  (interactive)
  (save-excursion
    (move-end-of-line 1)
    (open-line 1)))

;; C-Backspace kill one word, a space character or a new line character
(defun my/kill-backward-smart ()
  (interactive)
  (cond
   ((looking-back (rx (char word)) 1)
    (backward-kill-word 1))
   ((looking-back (rx (char blank)) 1)
    (delete-horizontal-space t))
   (t
    (backward-delete-char 1))))

;; Browse repo in browser
(defun my/browse-repo()
  (interactive)
  (browse-url
   (let
       ((rev (magit-rev-abbrev "HEAD"))
        (repo (forge-get-repository 'stub))
        (file (magit-file-relative-name buffer-file-name))
        (highlight
         (if
             (use-region-p)
             (let ((l1 (line-number-at-pos (region-beginning)))
                   (l2 (line-number-at-pos (- (region-end) 1))))
               (format "#L%d-L%d" l1 l2))
           ""
           )))
     (forge--format repo (if file
                             "https://%h/%o/%n/blob/%r/%f%L"
                           "https://%h/%o/%n/blob/%r")
                    `((?r . ,rev) (?f . ,file) (?L . ,highlight))))))

(setq lsp-clients-clangd-args
      '("-j=3"
        "--background-index"
        "--clang-tidy"
        "--completion-style=detailed"
        "--header-insertion=never"
        "--header-insertion-decorators=0"))
(after! lsp-clangd (set-lsp-priority! 'clangd 2))

;;____________________________________________________________
;; set-mark symbol at point
(defun my/mark-symbol-at-point ()
  (interactive)
  (unless (region-active-p)
    (let (bounds pos1 pos2)
      (setq bounds (bounds-of-thing-at-point 'symbol))
      (setq pos1 (car bounds))
      (setq pos2 (cdr bounds))
      (goto-char pos1)
      (push-mark pos1 t t)
      (goto-char pos2)
      (setq transient-mark-mode  (cons 'only transient-mark-mode)))))
