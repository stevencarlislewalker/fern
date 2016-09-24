(setq initial-frame-alist '((foreground-color . "grey90")
			    (background-color . "grey20")
			    (left . 50)  ))
(setq default-frame-alist '((foreground-color . "grey90")
			    (background-color . "grey20")
			    (cursor-color . "green")
			    (left . 0)
			    (width . 141)
			    (height . 44)))
(set-default 'cursor-type 'box)

(R-mode 1)

(setq ess-history-directory "~")
(ess-toggle-underscore nil)



(global-set-key (kbd "M-[") 'insert-pair)
(global-set-key (kbd "M-\"") 'insert-pair)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-command-list
   (quote
    (("LaTeXnonint" "%`%l -interaction=nonstopmode %(mode)%' %t" TeX-run-command nil t)
     ("TeX" "%(PDF)%(tex) %(extraopts) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (plain-tex-mode texinfo-mode ams-tex-mode)
      :help "Run plain TeX")
     ("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil
      (latex-mode doctex-mode)
      :help "Run LaTeX")
     ("Makeinfo" "makeinfo %(extraopts) %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with Info output")
     ("Makeinfo HTML" "makeinfo %(extraopts) --html %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with HTML output")
     ("AmSTeX" "%(PDF)amstex %(extraopts) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (ams-tex-mode)
      :help "Run AMSTeX")
     ("ConTeXt" "texexec --once --texutil %(extraopts) %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt once")
     ("ConTeXt Full" "texexec %(extraopts) %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt until completion")
     ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX")
     ("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber")
     ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer")
     ("Print" "%p" TeX-run-command t t :help "Print the file")
     ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command)
     ("File" "%(o?)dvips %d -o %f " TeX-run-command t t :help "Generate PostScript file")
     ("Index" "makeindex %s" TeX-run-command nil t :help "Create index file")
     ("Xindy" "texindy %s" TeX-run-command nil t :help "Run xindy to create index file")
     ("Check" "lacheck %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for correctness")
     ("ChkTeX" "chktex -v6 %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for common mistakes")
     ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document")
     ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
     ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
     ("Other" "" TeX-run-command t t :help "Run an arbitrary command"))))
 '(inhibit-startup-screen t)
 '(send-mail-function nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; (defun then_R_operator ()
;;   "R - %>% operator or 'then' pipe operator"
;;   (interactive)
;;   (just-one-space 1)
;;   (insert "%>%")
;;   (reindent-then-newline-and-indent))
;; (define-key ess-mode-map (kbd "C-%") 'then_R_operator)
;; (define-key inferior-ess-mode-map (kbd "C-%") 'then_R_operator)


;; ----------------------------------------------------------------------
;; Brilliant:
;; https://github.com/emacs-ess/ESS/issues/96#issuecomment-71984227
(add-to-list 'ess-style-alist
             '(my-style
               (ess-indent-level . 4)
               (ess-first-continued-statement-offset . 2) ;; should it be 2? yes
               (ess-continued-statement-offset . 0)
               (ess-brace-offset . -4)
               (ess-expression-offset . 4)
               (ess-else-offset . 0)
               (ess-close-brace-offset . 0)
               (ess-brace-imaginary-offset . 0)
               (ess-continued-brace-offset . 0)
               (ess-arg-function-offset . 4)
           (ess-arg-function-offset-new-line . '(4))
               ))
(setq ess-default-style 'my-style)
;; ----------------------------------------------------------------------

;; ----------------------------------------------------------------------
;; mydir-
;;
;; light-weight
;; ----------------------------------------------------------------------

(defun mydir-ppg ()
  (interactive)
  (find-file (concat "//ca3answcmgr02/ClientData/PPG/Work/Services/")))

(defun mydir-content-manager ()
  (interactive)
  (find-file (concat "//ca3answcmgr02/ClientData/")))

(defun mydir-notes ()
  (interactive)
  (find-file (concat "~/Documents/notes/")))




;; (setq paragraph-start
;;       '("\f\\|[ \t]*$\\|[ \t]*[*+-] \\|[ \t]*[0-9]+\\.\\|[ \t]*: "))

;; (setq paragraph-separate
;;       '("\\(?:[ \t\f]\\|.*  \\)*$"))


;; Paragraph filling
(set (make-local-variable 'paragraph-start)
       "\f\\|[ \t]*$\\|[ \t]*[*+-] \\|[ \t]*[0-9]+\\.\\|[ \t]*: ")
(set (make-local-variable 'paragraph-separate)
     "\\(?:[ \t\f]\\|.*  \\)*$")
(set (make-local-variable 'adaptive-fill-first-line-regexp)
     "\\`[ \t]*>[ \t]*?\\'")
(set (make-local-variable 'adaptive-fill-function)
     'markdown-adaptive-fill-function)
(put 'upcase-region 'disabled nil)



;; use linux line endings
(setq default-buffer-file-coding-system 'utf-8-unix)



(defun renumber (&optional num)
  "Renumber the list items in the current paragraph,
    starting at point."
  (interactive "p")
  (setq num (or num 1))
  (let ((end (save-excursion
	       (forward-paragraph)
	       (point))))
    (while (re-search-forward "^[0-9]+" end t)
      (replace-match (number-to-string num))
      (setq num (1+ num)))))

(defun renumber-list (start end &optional num)
  "Renumber the list items in the current START..END region.
    If optional prefix arg NUM is given, start numbering from that number
    instead of 1."
  (interactive "*r\np")
  (save-excursion
    (goto-char start)
    (setq num (or num 1))
    (save-match-data
      (while (re-search-forward "^[0-9]+" end t)
	(replace-match (number-to-string num))
	(setq num (1+ num))))))


;; ----------------------------------------------------------------------
;; https://kb.iu.edu/d/abuf
;; display column numbers below buffer
(setq column-number-mode t)
;; ----------------------------------------------------------------------

(defun bigcomment ()
  (interactive)
  (insert "#### ----------------------------------------------------------------------\n")
  (insert "#### \n")
  (insert "#### ----------------------------------------------------------------------\n")
  (backward-char 77))

(defun ligcomment ()
  (interactive)
  (insert "## ----------------------------------------------------------------------\n")
  (insert "## \n")
  (insert "## ----------------------------------------------------------------------\n")
  (backward-char 75))



(defun rpipe ()
  (interactive)
  (just-one-space 1)
  (insert "%>%")
  (just-one-space 1))

(define-key ess-mode-map (kbd "C-%") 'rpipe)
(define-key inferior-ess-mode-map (kbd "C-%") 'rpipe)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; oh ya!
;; http://stackoverflow.com/questions/6286579/emacs-shell-mode-how-to-send-region-to-shell#7053298
(defun sh-send-line-or-region (&optional step)
  (interactive ())
  (let ((proc (get-process "shell"))
        pbuf min max command)
    (unless proc
      (let ((currbuff (current-buffer)))
        (shell)
        (switch-to-buffer currbuff)
        (setq proc (get-process "shell"))
        ))
    (setq pbuff (process-buffer proc))
    (if (use-region-p)
        (setq min (region-beginning)
              max (region-end))
      (setq min (point-at-bol)
            max (point-at-eol)))
    (setq command (concat (buffer-substring min max) "\n"))
    (with-current-buffer pbuff
      (goto-char (process-mark proc))
      (insert command)
      (move-marker (process-mark proc) (point))
      ) ;;pop-to-buffer does not work with save-current-buffer -- bug?
    (process-send-string  proc command)
    (display-buffer (process-buffer proc) t)
    (when step
      (goto-char max)
      (next-line))
    ))

(defun sh-send-line-or-region-and-step ()
  (interactive)
  (sh-send-line-or-region t))
(defun sh-switch-to-process-buffer ()
  (interactive)
  (pop-to-buffer (process-buffer (get-process "shell")) t))

;;(define-key sh-mode-map [(control ?j)] 'sh-send-line-or-region-and-step)
;;(define-key sh-mode-map [(control ?c) (control ?z)] 'sh-switch-to-process-buffer)

