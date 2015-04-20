;;; company-ide-backend.el --- company-mode ide-backend-mode backend -*- lexical-binding: t -*-

;; Copyright (C) 2015 by Arne Link

;; Author:    Arne Link <link.arne@gmail.com>
;; URL:       https://github.com/Codas/company-ide-backend
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

;; `company-mode' back-end for `haskell-mode' via `ide-backend-mode'.
;;
;; Provide context sensitive completion by using information from `ide-backend-mode'.
;; Add `company-ide-backend' to `company-mode' back-ends list.
;;
;;     (add-to-list 'company-backends 'company-ide-backend)
;;
;; or grouped with other back-ends.
;;
;;     (add-to-list 'company-backends '(company-ide-backend :with company-dabbrev-code))

;;; Code:

(require 'cl-lib)
(require 'company)
(require 'ide-backend-mode)
(require 'haskell-mode)

(defgroup company-ide-backend nil
  "company-mode back-end for haskell-mode."
  :group 'company)

(defcustom company-ide-backend-show-module t
  "Non-nil to show module name as annotation."
  :type 'boolean)

(defconst company-ide-backend-pragma-regexp "{-#[[:space:]]*\\([[:upper:]]+\\>\\|\\)")

(defconst company-ide-backend-langopt-regexp
  (concat "{-#[[:space:]\n]*\\(LANGUAGE\\|OPTIONS_GHC\\)[[:space:]\n]+"
          "\\(?:[^[:space:]]+,[[:space:]\n]*\\)*"
          "\\([^[:space:]]+\\|\\)"))

(defconst company-ide-backend-import-regexp
  (concat "import[[:space:]\n]+"
          "\\(?:safe[[:space:]\n]+\\)?"
          "\\(?:qualified[[:space:]\n]+\\)?"
          "\\(?:\"[^\"]+\"[[:space:]\n]+\\)?"
          "\\([[:word:].]+\\|\\)"))

(defconst company-ide-backend-impdecl-regexp
  (concat company-ide-backend-import-regexp
          "\\(?:[[:space:]\n]+as[[:space:]\n]+\\w+\\)?"
          "[[:space:]\n]*\\(?:hiding[[:space:]\n]\\)*("
          "\\(?:[[:space:]\n]*[[:word:]]+[[:space:]\n]*,\\)*"
          "[[:space:]\n]*\\([[:word:]]+\\_>\\|\\)"))

(defconst company-ide-backend-module-regexp
  "module[[:space:]]*\\([[:word:].]+\\_>\\|\\)")

(defconst company-ide-pragma-names '("LANGUAGE" "OPTIONS_GHC" "INCLUDE" "WARNING" "DEPRECATED" "INLINE" "NOINLINE" "ANN" "LINE" "RULES" "SPECIALIZE" "UNPACK" "SOURCE"))


(defvar company-ide-backend--propertized-modules '())
(defvar company-ide-backend--imported-modules '())
(make-variable-buffer-local 'company-ide-backend--imported-modules)

(defun company-ide-backend--find-context ()
  "Find completion context at the current point."
  (cond
   ((company-ide-backend--in-comment-p)
    (cond
     ((company-grab company-ide-backend-pragma-regexp)
      '(pragma))
     ((company-grab company-ide-backend-langopt-regexp)
      (and (looking-at-p "\\([#, [:space:]]\\|$\\)")
           (cons 'langopt (match-string-no-properties 1))))))

   ((company-grab company-ide-backend-impdecl-regexp)
    (cons 'impspec (match-string-no-properties 1)))

   ((company-grab company-ide-backend-import-regexp) '(module))

   ((company-grab company-ide-backend-module-regexp) '(module))

   (t (let ((qcons (company-ide-backend--grab-qualified)))
        (if qcons
            (cons 'qualifier (car qcons))
          '(keyword))))))

(defun company-ide-backend-prefix ()
  "Provide completion prefix at the current point."
  (let ((ppss (syntax-ppss)) match)
    (cond
     ((nth 3 ppss) 'stop)
     ((nth 4 ppss)
      (if (looking-back company-ide-backend-pragma-regexp)
          (match-string-no-properties 1)
        (company-grab "[[:space:],]\\([^[:space:]]*\\)" 1)))
     ((looking-back "^[^[:space:]]*") nil)
     ((let ((case-fold-search nil))
        (and (save-excursion
               (forward-line 0)
               (not (looking-at-p "^import\\>")))
             (setq match (company-ide-backend--grab-qualified))))
      (cons (cdr match) t))
     ((looking-back "[[:word:].]+" nil t)
      (match-string-no-properties 0))
     (t (company-ide-backend--grab-name)))))

(defun company-ide-backend-completion-prefix ()
  "Provide completion prefix at the current point."
  (let ((ppss (syntax-ppss)) match)
    (cond
     ((nth 3 ppss) 'stop)
     ((nth 4 ppss)
      (if (looking-back company-ide-backend-pragma-regexp)
          (match-string-no-properties 1)
        (company-grab "[[:space:],]\\([^[:space:]]*\\)" 1)))
     ((looking-back "^[^[:space:]]*") nil)
     ((let ((case-fold-search nil))
        (and (save-excursion
               (forward-line 0)
               (not (looking-at-p "^import\\>")))
             (setq match (company-ide-backend--grab-qualified))))
      (concat (car match) "." (cdr match)))
     ((looking-back "[[:word:].]+" nil t)
      (match-string-no-properties 0))
     (t (company-ide-backend--grab-name)))))

(defun company-ide-backend-candidates (prefix)
  "Provide completion candidates for the given PREFIX."
  (let ((ctx (company-ide-backend--find-context)))
    (pcase ctx
      (`(pragma) (all-completions prefix company-ide-pragma-names))
      (_ (company-ide-backend--gather-candidates (company-ide-backend-completion-prefix))))))

(defun company-ide-backend-meta (candidate)
  "Show type info for the given CANDIDATE."
  (let* ((metadata (company-ide-backend--pget candidate :type))
         (type (if (string= "" metadata) "" (concat candidate " :: " metadata))))
    (haskell-fontify-as-mode type 'haskell-mode)))

(defun company-ide-backend-annotation (candidate)
  "Show module name as annotation where the given CANDIDATE is defined."
  (when company-ide-backend-show-module
    (concat " " (company-ide-backend--pget candidate :module))))

(defun company-ide-backend--gather-candidates (prefix)
  "Gather candidates for PREFIX from keywords and return them sorted."
  (when (and (ide-backend-mode-process) (process-live-p (ide-backend-mode-process)))
    (let* ((complassoc (company-ide-backend-completion-candidates prefix))
           (completions (mapcar (lambda (c)
                                  (let ((name (cdr (assoc 'name c)))
                                        (module (or (cdr (assoc 'definedIn c)) ""))
                                        (type (or (cdr (assoc 'type c)) "")))
                                    (put-text-property 0 1 :module module name)
                                    (put-text-property 0 1 :type type name)
                                    name)) complassoc)))
      (sort completions (lambda (c1 c2)
                          (string< c1 c2))))))
;;
;; ide-backend-mode autocompletion support
;; Only use this as long as ide-backend-mode does not provide a means
;; to get completion candidates
;;
(defun company-ide-backend-completion-candidates (prefix)
  "Display type info of thing at point."
  (let* ((filename (buffer-file-name))
         (module-name (haskell-guess-module-name))
         (points (ide-backend-mode-points)))
    (let* ((info (company-ide-backend-get-completion-candidates
                  module-name
                  (with-current-buffer (ide-backend-mode-buffer)
                    (file-relative-name filename default-directory))
                  prefix))
           (suggestions (mapcar #'identity (cdr (assoc 'completions info)))))
      suggestions)))

(defun company-ide-backend-get-completion-candidates (module file prefix)
  "Get autocomplete info of a given prefix."
  (with-current-buffer (ide-backend-mode-buffer)
    (ide-backend-mode-call
     `((request . "getAutocompletion")
       (module . ,module)
       (autocomplete . ((filePath . ,file)
                (prefix . ,prefix)))))))

;;
;; Unitilities
;;
(defun company-ide-backend--pget (s prop)
  "Get property value of PROP from the keyword S."
  (get-text-property 0 prop s))

(defun company-ide-backend--in-comment-p ()
  "Return whether the point is in comment or not."
  (let ((ppss (syntax-ppss))) (nth 4 ppss)))


(defun company-ide-backend--grab-name ()
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

(defun company-ide-backend--grab-qualified ()
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
(defun company-ide-backend (command &optional arg &rest ignored)
  "`company-mode' completion back-end for `haskell-mode' via ide-backend-mode.
Provide completion info according to COMMAND and ARG.  IGNORED, not used."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-ide-backend))
    (prefix (and (derived-mode-p 'haskell-mode)
                 (company-ide-backend-prefix)))
    (candidates (company-ide-backend-candidates arg))
    (meta (company-ide-backend-meta arg))
    (annotation (company-ide-backend-annotation arg))
    (sorted t)))


(provide 'company-ide-backend)
;;; company-ide-backend.el ends here
