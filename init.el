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
(fringe-mode 0)
(setq-default cursor-type 'bar)
(set-default 'truncate-lines t)
(setq inhibit-startup-screen t)

(electric-indent-mode 0)
(blink-cursor-mode 0)
(show-paren-mode 1)
(setq show-paren-delay 0)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "TAB") 'self-insert-command)
(global-set-key (kbd "<SPC>") 'legerity-space)
;; # (global-set-key (kbd "<backspace>") 'legerity-backspace)
(global-set-key (kbd "S-<backspace>") 'legerity-del-spaces)
(add-hook 'python-mode-hook
          (lambda () (local-set-key (kbd "<backspace>") 'legerity-backspace)
		  			 (local-set-key (kbd "<tab>") 'legerity-placeholder)))

(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq auto-save-default nil)

;; modeline
;; (set-face-attribute 'mode-line nil
                    ;; :background "#676767"
                    ;; :foreground "#FFFAFA"
                    ;; :box '(:line-width 2 :color "#FFFAFA")
                    ;; :overline nil
                    ;; :underline nil)

(set-face-attribute 'mode-line nil
                    :background "#181B1B"
                    :foreground "#D3D7CF"
		    :box '(:line-width 3 :color "#181B1B")
                    :overline nil
                    :underline nil)

(set-face-attribute 'mode-line-inactive nil
                    :background "#2E3436"
                    :foreground "#D3D7CF"
                    :box '(:line-width 3 :color "#2E3436")
                    :overline nil
                    :underline nil)

;; light theme
 ;; (setq default-frame-alist
 ;; '((cursor-color . "#676767")))
 ;; (set-frame-font "Roboto Mono 12" nil t)
 ;; (set-background-color "#F5F3EF") ;; #FFFAFA
 ;; (set-foreground-color "#676767")
 ;; (set-face-foreground 'font-lock-comment-face "#d98695")
 ;; (set-face-foreground 'font-lock-string-face "#676767")
 ;; (set-face-foreground 'font-lock-type-face "#676767")
 ;; (set-face-foreground 'font-lock-variable-name-face "#676767")
 ;; (set-face-foreground 'font-lock-function-name-face "#676767")
;; (set-face-foreground 'font-lock-keyword-face "#431C53")


;; my old Eclipse Java theme
   (setq default-frame-alist
  '((cursor-color . "#D3D7CF")))
 (set-frame-font "Roboto Mono 12" nil t)
 (set-background-color "#2E3436") ;; #FFFAFA
 (set-foreground-color "#D3D7CF")
 (set-face-foreground 'font-lock-comment-face "#818FA6")
 (set-face-foreground 'font-lock-string-face "#FFE5B4")
 (set-face-foreground 'font-lock-type-face "#676767")
 (set-face-foreground 'font-lock-variable-name-face "#D3D7CF")
 (set-face-foreground 'font-lock-function-name-face "#D3D7CF")
 (set-face-foreground 'font-lock-keyword-face "#D197D9")
(set-face-background 'fringe "black")

(set-face-background 'show-paren-match "#181B1B")
    (set-face-foreground 'show-paren-match "#87CEEB")
    (set-face-attribute 'show-paren-match nil :weight 'extra-bold)

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
    (,(kbd "r") . legerity-forward)
    (,(kbd "e") . legerity-backward)
    (,(kbd "w") . undo)
    (,(kbd "b") . ivy-switch-buffer)
    (,(kbd "a") . execute-extended-command)
    (,(kbd "z") . comment-line)
	(,(kbd "g") . legerity-quit)
	(,(kbd "S") . placeholder)
	(,(kbd "8") . python-buffer-run)
	(,(kbd "d") . ido-switch-buffer)
	(,(kbd "T") . beginning-of-buffer)
	(,(kbd "Y") . end-of-buffer)
	(,(kbd "/") . legerity-test)
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

(defun legerity-space ()
(interactive)
	(if (or (eq (point) (line-beginning-position)) (eq ?\s (char-before)) (eq ?\t (char-before)))
	   (insert "\s\s\s\s")
	   (insert "\s")
))

(defun legerity-backspace ()
(interactive)
	(if (and (eq (char-before) ?\s) (eq (char-before (- (point) 1)) ?\s)
	(eq (char-before (- (point) 2)) ?\s) (eq (char-before (- (point) 3)) ?\s))
	(delete-backward-char 4)
	(delete-backward-char 1)))

(defun legerity-del-spaces ()
(interactive)
(delete-char 4))

;; move to first non-whitespace character on current line
(defun legerity-python-tab ()
(interactive)
)
	    
(defun legerity-quit ()
       (interactive)
	(if (active-minibuffer-window)
        (minibuffer-keyboard-quit)
        (keyboard-quit)))

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

;; placeholder so key-input isn't inserted
(defun placeholder ()
(interactive)
(message "not implemented yet!")
)

;; used for testing emacs lisp code
(defun legerity-test ()
(interactive)
(backward-delete-char 4))

;; for quick/small python tests/scripts
(defun python-buffer-run ()
	(interactive)
	(if (string= (buffer-name) "*shell*")
		(kill-buffer-and-window)
	(let (buffer-name)
	(setq buffer-name (file-name-base))
    (save-buffer)
	(shell)
	(insert (concat "python " buffer-name ".py"))
	(execute-kbd-macro (read-kbd-macro "<RET>")))))


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

;; For python literate programming
(org-babel-do-load-languages
 'org-babel-load-languages '((python . t)))
(setq org-confirm-babel-evaluate nil)
