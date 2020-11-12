(require 'package)
(setq package-enable-at-statup nil)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;packages
(use-package ivy
  :ensure t
  :init (ivy-mode 1))
;;----------------------------------


(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(blink-cursor-mode 0)
(setq-default cursor-type 'bar)
(show-paren-mode 1)
(setq make-backup-files nil)

;; modeline
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

;; theme
 (setq default-frame-alist
  '((cursor-color . "#676767")))
 (set-frame-font "Roboto Mono 12" nil t)
 (set-background-color "#F5F3EF") ;; #FFFAFA
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
    (,(kbd "j") . next-line)
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
    (,(kbd "e") . legerity-forward)
    (,(kbd "r") . legerity-backward)
    (,(kbd "w") . undo)
    (,(kbd "b") . ivy-switch-buffer)
    (,(kbd "a") . execute-extended-command)
    )
 ;; Make mode global rather than buffer local
   :global 1
   )

(define-key ivy-minibuffer-map (kbd "S-SPC") #'legerity-toggle)
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
	(cond
	((eq (use-region-p) t)
		(kill-ring-save (region-beginning) (region-end)))
	((eq t t)
		(copy-line 1))))

  (defun copy-line (arg)
    "Copy lines (as many as prefix argument) in the kill ring.
      Ease of use features:
      - Move to start of next line.
      - Appends the copy on sequential calls.
      - Use newline as last char even on the last line of the buffer.
      - If region is active, copy its lines."
    (interactive "p")
    (let ((beg (line-beginning-position))
          (end (line-end-position arg)))
      (when mark-active
        (if (> (point) (mark))
            (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
          (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
      (if (eq last-command 'copy-line)
          (kill-append (buffer-substring beg end) (< end beg))
        (kill-ring-save beg end)))
    (kill-append "\n" nil)
    (beginning-of-line (or (and arg (1+ arg)) 2))
    (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

(defun legerity-paste ()
  (interactive)
  (yank))

(defun is-left-pair ()
	(interactive)
	(if (or (eq ?\( (char-before))
		(eq ?\{ (char-before))
		(eq ?\[ (char-before))
		(eq ?\< (char-before)))
		(char-before)
		))

(defun is-right-pair ()
	(interactive)
	(if (or (eq ?\) (char-after))
		(eq ?\} (char-after))
		(eq ?\] (char-after))
		(eq ?\> (char-after)))
		(char-after)
		))

(defun legerity-forward ()
	(interactive)
	(if (is-right-pair)
		(forward-char)
		(while (eq (is-right-pair) nil)
			(forward-char))
		)
        (forward-char)
)

(defun legerity-backward ()
	(interactive)
	(if (is-left-pair)
		(backward-char)
		(while (eq (is-left-pair) nil)
			(backward-char))
		)
		(backward-char)
)

(defun legerity-toggle ()
  (interactive)
  (let ((inhibit-message t))
  (if (bound-and-true-p legerity-mode)
      (progn (call-interactively 'legerity-mode)
	     (setq-default cursor-type 'bar))
      (progn (call-interactively 'legerity-mode)
	     (setq-default cursor-type 'box))
      ))
)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (ivy use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

