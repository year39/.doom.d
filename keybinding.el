;;; keybinding.el -*- lexical-binding: t; -*-

;;;____________________________________________________________
;; access the map! documentation with (SPC h f map!)
;; https://discourse.doomemacs.org/t/how-to-re-bind-keys/56

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Note: Need to add the (interactive) to the lambda functions when using them in map!. ;;
;; Kebindings are considered interactive.                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map! :map 'override
      :desc "kill backward smart"
      :nvig "C-<backspace>" #'my/kill-backward-smart)

(map! :leader
      "m" nil
      :prefix ("m" . "er/mark")
      :desc "Mark buffer"         :nv "a" #'mark-whole-buffer
      :desc "Mark paragraph"            :nv "p" #'er/mark-paragraph
      :desc "Mark word"                 :nv "w" #'er/mark-word
      :desc "Mark sentence"             :nv "s" #'er/mark-sentence
      :desc "Mark function"             :nv "f" #'er/mark-defun
      :desc "Mark comment"              :nv "c" #'er/mark-comment
      :desc "Mark url"                  :nv "u" #'er/mark-url
      :desc "Mark email"                :nv "e" #'er/mark-email
      :desc "Mark symbol"               :nv "s" #'er/mark-symbol
      :desc "Mark quotes"               :nv "q" #'er/mark-outside-quotes
      :desc "Mark inside quotes"        :nv "i q" #'er/mark-inside-quotes
      :desc "Mark method call"          :nv "m" #'er/mark-method-call
      :desc "Mark inside parens"        :nv "i p" #'er/mark-inside-pairs
      )

(map! :leader
      :desc "Other window"              :nv     "RET"   #'other-window
      :desc "Open line"                 :nv     "i n"   #'my/open-line
      :desc "Yank kill-ring"       :nvig   "i k"   #'(lambda () (interactive) (yank-pop))
      :desc "Increase-height 10"        :nv     "w +"   #'(lambda () (interactive) (evil-window-increase-height 10))
      :desc "Decrease-height 10"        :nv     "w -"   #'(lambda () (interactive) (evil-window-decrease-height 10))
      :desc "Jump backward"             :nv     "c h"   #'evil-jump-backward
      :desc "Jump forward"              :nv     "c l"   #'evil-jump-forward
      :desc "Format buffer"             :nv     "c b"   #'+format/buffer
      :desc "Format region"             :nv     "c r"   #'+format/region
      )
