;;; init.el -*- lexical-binding: t; -*-

(doom! :input

       :completion
       (company +childframe)
       (vertico +icons)

       :ui
       doom                ; what makes DOOM look the way it does
       doom-dashboard      ; a nifty splash screen for Emacs
       doom-quit           ; DOOM quit-message prompts when you quit Emacs
       hl-todo             ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       indent-guides       ; highlighted indent columns
       modeline            ; snazzy, Atom-inspired modeline, plus API
       ophints             ; highlight the region an operation acts on
       (popup +defaults)   ; tame sudden yet inevitable temporary windows
       (vc-gutter +pretty) ; vcs diff in the fringe
       vi-tilde-fringe     ; fringe tildes to mark beyond EOB
       (window-select      ; visually switch windows
        +switch-window)
       workspaces          ; tab emulation, persistence & separate workspaces

       :editor
       (evil +everywhere)  ; come to the dark side, we have cookies
       file-templates      ; auto-snippets for empty files
       fold                ; (nigh) universal code folding
       (format +onsave)  ; automated prettiness
       multiple-cursors  ; editing in many places at once
       snippets            ; my elves. They type so I don't have to
       word-wrap           ; soft wrapping with language-aware indent

       :emacs
       dired               ; making dired pretty [functional]
       electric            ; smarter, keyword-based electric-indent
       (ibuffer            ; interactive buffer management
        +icons)
       (undo               ; persistent, smarter undo for your inevitable mistakes
        +tree)
       vc                  ; version-control and Emacs, sitting in a tree

       :term
       vterm               ; the best terminal emulation in Emacs

       :checkers
       syntax              ; tasing you for every semicolon you forget
       (spell +flyspell)   ; tasing you for misspelling mispelling
       grammar             ; tasing grammar mistake every you make

       :tools
       (lsp +eglot)
       (lsp +peek)
       lookup
       (debugger +lsp)     
       tree-sitter
       direnv
       editorconfig        ; let someone else argue about tabs vs spaces
       (eval +overlay)     ; run code, run (also, repls)
       (magit +forge)      ; a git porcelain for Emacs
       (pass +auth)        ; password manager for nerds
       upload              ; map local to remote projects via ssh/ftp
       terraform

       :os
       (:if IS-MAC macos)  ; improve compatibility with macOS
       tty                 ; improve the terminal Emacs experience

       :lang
       (cc +lsp)           ; C > C++ == 1
       common-lisp         ; if you've seen one lisp, you've seen them all
       data                ; config/data formats
       emacs-lisp          ; drown in parentheses
       (go +lsp)           ; the hipster dialect
       (graphql +lsp)      ; Give queries a REST
       (json +lsp)         ; At least it ain't XML
       ;;(java +lsp)       ; the poster child for carpal tunnel syndrome
       (javascript +lsp)
       ;;kotlin            ; a better, slicker Java(Script)
       ;;latex             ; writing papers in Emacs has never been so fun
       markdown            ; writing docs for people to ignore
       (org                ; organize your plain life in plain text
        +present
        +roam2
        +journal
        +pretty)
       ;;php               ; perl's insecure younger brother
       (purescript +lsp)          ; javascript, but functional
       (python             ; beautiful is better than ugly
        +lsp
        +pyright
        +conda)
       rest                ; Emacs as a REST client
       rst                 ; ReST in peace
       ;;rust              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;;scala             ; java, but good
       (sh +lsp)           ; she sells {ba,z,fi}sh shells on the C xor
       (web +lsp)          ; the tubes
       (yaml +lsp)         ; JSON, but readable
       ;;zig               ; C, but simpler

       :email
       ;;(mu4e +org +gmail)

       :app
       calendar

       :config
       (default +bindings +smartparens))
