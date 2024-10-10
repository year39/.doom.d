;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;____________________________________________________________
;;
;; Replace highlight when typing
(delete-selection-mode 1)

;; doom theme (doom-one, doom-dracula)
(setq doom-theme 'doom-dracula
      doom-font (font-spec :family "JetBrains Mono" :size 14 :weight 'light)
      doom-variable-pitch-font (font-spec :family "JetBrains Mono" :size 14))

;; Password cache
(setq password-cache t)
(setq password-cache-expiry nil)

;; Start fullscreen
(add-hook 'window-setup-hook #'toggle-frame-fullscreen)

;; don't yank on paste
(setq-default evil-kill-on-visual-paste nil)

;; Remove duplicates in commands history
(setq history-delete-duplicates t)

;; Replace highlight when typing
(delete-selection-mode 1)

;; For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; tell projectile not to register projects automatically
(setq projectile-track-known-projects-automatically nil)

;; Minimal UI
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; don't yank on paste
(setq evil-kill-on-visual-paste nil)

;; set auth-sources
(setq auth-sources
      '((:source "~/.authinfo.gpg")))

;; disable lsp formatting (use prettier)
;; https://github.com/doomemacs/doomemacs/issues/4158
(setq-hook! 'typescript-mode-hook +format-with-lsp nil)

;; the default is too high and makes vterm to slow
(setq vterm-timer-delay 0.01)

;;____________________________________________________________
;; org configs
(use-package! org
  :init
  (setq-default org-agenda-files '("~/Dropbox/org-agenda"))
  :config
  (setq org-startup-folded t)
  (setq org-hide-emphasis-markers t))

;; Use 12-hour clock instead of 24-hour in agenda view
(setq org-agenda-timegrid-use-ampm 1)

;; enable auto fill for org
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'markdown-mode-hook 'turn-on-auto-fill)

;;____________________________________________________________

;;____________________________________________________________
;; lsp configs
(use-package! lsp-mode
  :config
  ;; disable code underlines/flychecks
  ;; (setq lsp-diagnostics-provider :none)
  :custom
  (lsp-headerline-breadcrumb-enable t))

;;____________________________________________________________
;; modeline configs
(after! doom-modeline
  (remove-hook 'doom-modeline-mode-hook #'size-indication-mode) ; filesize in modeline
  (remove-hook 'doom-modeline-mode-hook #'column-number-mode)   ; cursor column in modeline
  (add-hook 'doom-modeline-mode-hook #'display-time-mode)
  (line-number-mode nil)
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

;;____________________________________________________________
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

(setq +format-with-lsp nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(after! dtrt-indent
  (add-to-list 'dtrt-indent-hook-mapping-list '(typescript-mode javascript typescript-indent-level)))

;; add spacing between windows
(modify-all-frames-parameters
 '((right-divider-width . 5)
   (bottom-divider-width . 5)))

;;____________________________________________________________
;; org-roam configs
(use-package! org-roam
  :init
  (setq org-roam-directory "~/Dropbox/org-roam"
        org-roam-db-location "~/Dropbox/org-roam/org-roam.db")
  (org-roam-db-autosync-mode t)
  :config
  (org-roam-db-sync)
  (setq org-roam-node-display-template
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

;;____________________________________________________________
;;
(use-package! kubernetes
  :commands (kubernetes-overview)
  :config
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600))

;;____________________________________________________________
;; lsp ignore
;; (add-to-list 'lsp-file-watch-ignored-directories
;;              '(".idea" "/opt/homebrew" "node_modules"
;;                ".git" "build" "_build" "postgres-data")
;;              )

;; disable copilot warning
(after! copilot
  (setq copilot-indent-offset-warning-disable t))

;; add clipboard to the kill-ring before next kill
(setq save-interprogram-paste-before-kill t)
