(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(blink-cursor-mode 0)
(setq-default cursor-type 'bar)
(show-paren-mode 1)
(setq make-backup-files nil)

;;modeline
(set-face-attribute 'mode-line nil
                    :background "#676767"
                    :foreground "#FFFAFA"
                    :box '(:line-width 2 :color "#FFFAFA")
                    :overline nil
                    :underline nil)

;;(set-face-attribute 'mode-line-inactive nil
;;                    :background "#565063"
;;                    :foreground "white"
;;                    :box '(:line-width 8 :color "#565063")
;;                    :overline nil
;;                    :underline nil)

(setq default-frame-alist
  '((cursor-color . "#676767")))
(set-frame-font "Roboto Mono 10" nil t)
(set-background-color "#FFFAFA")
(set-foreground-color "#676767")
(set-face-foreground 'font-lock-comment-face "#d98695")
(set-face-foreground 'font-lock-string-face "#676767")
(set-face-foreground 'font-lock-type-face "#676767") 
(set-face-foreground 'font-lock-variable-name-face "#676767") 
(set-face-foreground 'font-lock-function-name-face "#676767") 
(set-face-foreground 'font-lock-keyword-face "#431C53")

(define-minor-mode legerity-mode
  "Modal editing mode."
  ;; The initial value - Set to 1 to enable by default
  nil
  ;; The indicator for the mode line.
  " Legerity"
  ;; The minor mode keymap
 `(
    (,(kbd "C-c C-a") . some-command)
    (,(kbd "C-c C-b") . other-command)
    (,(kbd "j") . forward-line)
    (,(kbd "i") . previous-line)
    (,(kbd "o") . forward-char)
    (,(kbd "u") . backward-char)
    (,(kbd "f") . legerity-toggle)
    (,(kbd "s") . save-buffer)
    (,(kbd "k") . legerity-kill)
    (,(kbd "c") . legerity-copy)
    (,(kbd "v") . legerity-paste)
    (,(kbd "O") . end-of-line)
    (,(kbd "U") . beginning-of-line)
    (,(kbd "J") . newline)
    )
 ;; Make mode global rather than buffer local
   :global 1
  )

(global-set-key (kbd "S-SPC") 'legerity-toggle)

(defun legerity-kill ()
  (interactive)
  (cond ((or (eq (point) (line-end-position)) (eq (point) (line-beginning-position)))
	 (kill-whole-line))
	((eq ?\( (char-before))
	     (kill-sexp))
	(t (progn (forward-word) (backward-kill-word 1))))
)

(defun legerity-copy ()
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))))

(defun legerity-paste ()
  (interactive)
  (yank))

(defun legerity-toggle ()
  (interactive)
  (if (bound-and-true-p legerity-mode)
      (progn (call-interactively 'legerity-mode)
	     (setq-default cursor-type 'bar))
      (progn (call-interactively 'legerity-mode)
	     (setq-default cursor-type 'box))
      )
)

