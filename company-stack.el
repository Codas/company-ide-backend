;;; company-stack.el --- company-mode stack-mode backend -*- lexical-binding: t -*-

;; Copyright (C) 2015 by Arne Link

;; Author:    Arne Link <link.arne@gmail.com>
;; URL:       https://github.com/Codas/company-stack
;; Version:   0.0.1
;; Package-Requires: ((cl-lib "0.5") (company "0.8.0") (haskell-mode "13.8") (emacs "24"))
;; Keywords:  haskell, completion
;; Stability: experimental

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `company-mode' back-end for `haskell-mode' via `stack-mode'.
;;
;; Provide context sensitive completion by using information from `stack-mode'.
;; Add `company-stack' to `company-mode' back-ends list.
;;
;;     (add-to-list 'company-backends 'company-stack)
;;
;; or grouped with other back-ends.
;;
;;     (add-to-list 'company-backends '(company-stack :with company-dabbrev-code))

;;; Code:

(require 'cl-lib)
(require 'company)
(require 'stack-mode)
(require 'haskell-mode)

(defgroup company-stack nil
  "company-mode back-end for haskell-mode."
  :group 'company)

(defcustom company-stack-show-module t
  "Non-nil to show module name as annotation."
  :type 'boolean)

(defconst company-stack-pragma-regexp "{-#[[:space:]]*\\([[:upper:]]+\\>\\|\\)")

(defconst company-stack-langopt-regexp
  (concat "{-#[[:space:]\n]*\\(LANGUAGE\\|OPTIONS_GHC\\)[[:space:]\n]+"
          "\\(?:[^[:space:]]+,[[:space:]\n]*\\)*"
          "\\([^[:space:]]+\\|\\)"))

(defconst company-stack-import-regexp
  (concat "import[[:space:]\n]+"
          "\\(?:safe[[:space:]\n]+\\)?"
          "\\(?:qualified[[:space:]\n]+\\)?"
          "\\(?:\"[^\"]+\"[[:space:]\n]+\\)?"
          "\\([[:word:].]+\\|\\)"))

(defconst company-stack-impdecl-regexp
  (concat company-stack-import-regexp
          "\\(?:[[:space:]\n]+as[[:space:]\n]+\\w+\\)?"
          "[[:space:]\n]*\\(?:hiding[[:space:]\n]\\)*("
          "\\(?:[[:space:]\n]*[[:word:]]+[[:space:]\n]*,\\)*"
          "[[:space:]\n]*\\([[:word:]]+\\_>\\|\\)"))

(defconst company-stack-module-regexp
  "module[[:space:]]*\\([[:word:].]+\\_>\\|\\)")

(defconst company-ide-pragma-names '("LANGUAGE" "OPTIONS_GHC" "INCLUDE" "WARNING" "DEPRECATED" "INLINE" "NOINLINE" "ANN" "LINE" "RULES" "SPECIALIZE" "UNPACK" "SOURCE"))


(defvar company-stack--propertized-modules '())
(defvar company-stack--imported-modules '())
(make-variable-buffer-local 'company-stack--imported-modules)

(defun company-stack--find-context ()
  "Find completion context at the current point."
  (cond
   ((company-stack--in-comment-p)
    (cond
     ((company-grab company-stack-pragma-regexp)
      '(pragma))
     ((company-grab company-stack-langopt-regexp)
      (and (looking-at-p "\\([#, [:space:]]\\|$\\)")
           (cons 'langopt (match-string-no-properties 1))))))

   ((company-grab company-stack-impdecl-regexp)
    (cons 'impspec (match-string-no-properties 1)))

   ((company-grab company-stack-import-regexp) '(module))

   ((company-grab company-stack-module-regexp) '(module))

   (t (let ((qcons (company-stack--grab-qualified)))
        (if qcons
            (cons 'qualifier (car qcons))
          '(keyword))))))

(defun company-stack-prefix ()
  "Provide completion prefix at the current point."
  (let ((ppss (syntax-ppss)) match)
    (cond
     ((nth 3 ppss) 'stop)
     ((nth 4 ppss)
      (if (looking-back company-stack-pragma-regexp)
          (match-string-no-properties 1)
        (company-grab "[[:space:],]\\([^[:space:]]*\\)" 1)))
     ((looking-back "^[^[:space:]]*") nil)
     ((let ((case-fold-search nil))
        (and (save-excursion
               (forward-line 0)
               (not (looking-at-p "^import\\>")))
             (setq match (company-stack--grab-qualified))))
      (cons (cdr match) t))
     ((looking-back "[[:word:].]+" nil t)
      (match-string-no-properties 0))
     (t (company-stack--grab-name)))))

(defun company-stack-completion-prefix ()
  "Provide completion prefix at the current point."
  (let ((ppss (syntax-ppss)) match)
    (cond
     ((nth 3 ppss) 'stop)
     ((nth 4 ppss)
      (if (looking-back company-stack-pragma-regexp)
          (match-string-no-properties 1)
        (company-grab "[[:space:],]\\([^[:space:]]*\\)" 1)))
     ((looking-back "^[^[:space:]]*") nil)
     ((let ((case-fold-search nil))
        (and (save-excursion
               (forward-line 0)
               (not (looking-at-p "^import\\>")))
             (setq match (company-stack--grab-qualified))))
      (concat (car match) "." (cdr match)))
     ((looking-back "[[:word:].]+" nil t)
      (match-string-no-properties 0))
     (t (company-stack--grab-name)))))

(defun company-stack-candidates (prefix)
  "Provide completion candidates for the given PREFIX."
  (let ((ctx (company-stack--find-context)))
    (pcase ctx
      (`(pragma) (all-completions prefix company-ide-pragma-names))
      (_ (company-stack--gather-candidates (company-stack-completion-prefix))))))

(defun company-stack-meta (candidate)
  "Show type info for the given CANDIDATE."
  (let* ((metadata (company-stack--pget candidate :type))
         (type (if (string= "" metadata) "" (concat candidate " :: " metadata))))
    (haskell-fontify-as-mode type 'haskell-mode)))

(defun company-stack-annotation (candidate)
  "Show module name as annotation where the given CANDIDATE is defined."
  (when company-stack-show-module
    (concat " " (company-stack--pget candidate :module))))

(defun company-stack--gather-candidates (prefix)
  "Gather candidates for PREFIX from keywords and return them sorted."
  (when (and (stack-mode-process) (process-live-p (stack-mode-process)))
    (let* ((complassoc (company-stack-completion-candidates prefix))
           (completions (mapcar (lambda (c)
                                  (let ((name (cdr (assoc 'autocompletionInfoName c)))
                                        (module (or (cdr (assoc 'autocompletionInfoDefinedIn c)) ""))
                                        (type (or (cdr (assoc 'autocompletionType c)) "")))
                                    (put-text-property 0 1 :module module name)
                                    (put-text-property 0 1 :type type name)
                                    name)) complassoc)))
      (sort completions (lambda (c1 c2)
                          (string< c1 c2))))))
;;
;; stack-mode autocompletion support
;; Only use this as long as stack-mode does not provide a means
;; to get completion candidates
;;
(defun company-stack-completion-candidates (prefix)
  "Display type info of thing at point."
  (let* ((filename (buffer-file-name))
         (module-name (haskell-guess-module-name))
         (points (stack-mode-points)))
    (let* ((info (company-stack-get-completion-candidates
                  module-name
                  (with-current-buffer (stack-mode-buffer)
                    (file-relative-name filename default-directory))
                  prefix))
           (suggestions (mapcar #'identity (cdr (assoc 'contents info)))))
      suggestions)))

(defun company-stack-get-completion-candidates (module file prefix)
  "Get autocomplete info of a given prefix."
  (with-current-buffer (stack-mode-buffer)
    (stack-mode-call
     `((tag . "RequestGetAutocompletion")
       (contents . ((autocompletionFilePath . ,file)
                    (autocompletionPrefix . ,prefix)))))))

;;
;; Unitilities
;;
(defun company-stack--pget (s prop)
  "Get property value of PROP from the keyword S."
  (get-text-property 0 prop s))

(defun company-stack--in-comment-p ()
  "Return whether the point is in comment or not."
  (let ((ppss (syntax-ppss))) (nth 4 ppss)))


(defun company-stack--grab-name ()
  "Grap identifier or operator name backward from the current point."
  (save-excursion
   (buffer-substring-no-properties
    (point)
    (progn
      (let* ((c (char-before))
             (syn (and c (char-syntax c))))
        (when (member syn '(?w ?.))
          (skip-syntax-backward (string syn)))
        (point))))))

(defun company-stack--grab-qualified ()
  "Grab cons of qualified specifier and keyword backward from the current point.
Return nil if none found."
  (save-excursion
    (let ((prefix
           (buffer-substring-no-properties
            (point)
            (progn
              (skip-chars-backward "[:word:]")
              (point))))
          (case-fold-search nil)
          end)
      (setq end (- (point) 1))
      (when (and (equal (char-before) ?.)
                 (< (skip-chars-backward ".[:word:]") 0)
                 (looking-at-p "[[:upper:]]"))
        (cons
         (buffer-substring-no-properties (point) end)
         prefix)))))

;;;###autoload
(defun company-stack (command &optional arg &rest ignored)
  "`company-mode' completion back-end for `haskell-mode' via stack-mode.
Provide completion info according to COMMAND and ARG.  IGNORED, not used."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-stack))
    (prefix (and (derived-mode-p 'haskell-mode)
                 (company-stack-prefix)))
    (candidates (company-stack-candidates arg))
    (meta (company-stack-meta arg))
    (annotation (company-stack-annotation arg))
    (sorted t)))


(provide 'company-stack)
;;; company-stack.el ends here
