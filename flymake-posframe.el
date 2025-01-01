;;; flymake-posframe.el --- Display flymake diagnostics at point  -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2025 Aya Igarashi

;; Author: Aya Igarashi <ladiclexxx@gmail.com>
;; Maintainer: Aya Igarashi <ladiclexxx@gmail.com>
;; URL: https://github.com/Ladicle/flymake-posframe
;; Keywords: convenience, languages, tools
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1") (posframe "1.4.4"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Display flymake message at point using a posframe.
;; Check out the README for more information.

;;;; Setup

;; (with-eval-after-load 'flymake
;;    (require 'flymake-posframe)
;;    (add-hook 'flymake-mode-hook #'flymake-posframe-mode))

;;; Code:

(require 'flymake)
(require 'posframe)
(require 'subr-x)

(defcustom flymake-posframe-error-prefix "!! "
  "String to be displayed before every error line."
  :group 'flymake-posframe
  :type '(choice (const :tag "No prefix" nil)
                 string))

(defcustom flymake-posframe-warning-prefix "! "
  "String to be displayed before every warning line."
  :group 'flymake-posframe
  :type '(choice (const :tag "No prefix" nil)
                 string))

(defcustom flymake-posframe-note-prefix "? "
  "String to be displayed before every note line."
  :group 'flymake-posframe
  :type '(choice (const :tag "No prefix" nil)
                 string))

(defcustom flymake-posframe-unknown-prefix "* "
  "String to be displayed before every unknown line."
  :group 'flymake-posframe
  :type '(choice (const :tag "No prefix" nil)
                 string))

(defcustom flymake-posframe-buffer " *flymake-posframe-buffer*"
  "Name of the flymake posframe buffer."
  :group 'flymake-posframe
  :type 'string)

(defcustom flymake-posframe-timeout 0
  "Number of seconds after which the posframe will auto-hide."
  :group 'flymake-posframe
  :type 'integer)

(defcustom flymake-posframe-internal-border-width 6
  "Number of the internal border width for the flymake posframe."
  :group 'flymake-posframe
  :type 'integer)

(defcustom flymake-posframe-max-width 75
  "Maximum number of the flymake posframe width."
  :group 'flymake-posframe
  :type 'integer)

(defcustom flymake-posframe-max-hight nil
  "Maximum number of the flymake posframe hight."
  :group 'flymake-posframe
  :type 'integer)

(defcustom flymake-posframe-parameters nil
  "The frame parameters used by flymake-posframe."
  :group 'flymake-posframe
  :type 'alist)

(defvar-local flymake-posframe-last-diag nil
  "Show the error at point.")

(defvar flymake-posframe-hide-posframe-hooks
  '(pre-command-hook post-command-hook focus-out-hook)
  "The hooks which should trigger automatic removal of the posframe.")

(defface flymake-posframe-background-face
  '((t :inherit highlight))
  "The background color of the flymake-posframe frame.
Only the `background' is used in this face."
  :group 'flymake-posframe)

(defface flymake-posframe-foreground-face
  '((t :inherit highlight))
  "The background color of the flymake-posframe frame.
Only the `foreground' is used in this face."
  :group 'flymake-posframe)

(defun flymake-posframe-get-diagnostic-text ()
  "Get the flymake diagnostic text for the thing at point."
  (flymake--diag-text (get-char-property (point) 'flymake-diagnostic)))

(defun flymake-posframe-hide ()
  (posframe-hide flymake-posframe-buffer)
  (dolist (hook flymake-posframe-hide-posframe-hooks)
    (remove-hook hook #'flymake-posframe-hide t)))

(defun flymake-posframe-display ()
  "Display diagnostic message on postframe"
  (when flymake-mode
    (if-let ((diag (get-char-property (point) 'flymake-diagnostic)))
        (unless (and (eq diag flymake-posframe-last-diag)
                     (frame-visible-p (buffer-local-value 'posframe--frame (get-buffer flymake-posframe-buffer))))
          (setq flymake-posframe-last-diag diag)
          (posframe-show
           flymake-posframe-buffer
           :internal-border-width flymake-posframe-internal-border-width
           :max-width flymake-posframe-max-width
           :max-height flymake-posframe-max-hight
           :timeout flymake-posframe-timeout
           :foreground-color (face-foreground 'flymake-posframe-foreground-face nil t)
           :background-color (face-background 'flymake-posframe-background-face nil t)
           :override-parameters flymake-posframe-parameters
           :string (concat (propertize
                            (pcase (flymake--lookup-type-property
                                    (flymake--diag-type diag)
                                    'flymake-category)
                              ('flymake-error flymake-posframe-error-prefix)
                              ('flymake-warning flymake-posframe-warning-prefix)
                              ('flymake-note flymake-posframe-note-prefix)
                              (_ flymake-posframe-unknown-prefix))
                            'face 'warning)
                           (flymake--diag-text diag)))

          (let ((current-posframe-frame
                 (buffer-local-value 'posframe--frame (get-buffer flymake-posframe-buffer))))
            (redirect-frame-focus current-posframe-frame (frame-parent current-posframe-frame)))

          (dolist (hook flymake-posframe-hide-posframe-hooks)
            (add-hook hook #'flymake-posframe-hide nil t)))
      (flymake-posframe-hide))))

;;;###autoload
(define-minor-mode flymake-posframe-mode
  "Minor mode for displaying flymake diagnostics at point."
  :lighter nil
  :group flymake-posframe
  (cond
   (flymake-posframe-mode
    (add-hook 'post-command-hook #'flymake-posframe-display nil 'local))
   (t
    (remove-hook 'post-command-hook #'flymake-posframe-display 'local))))

(provide 'flymake-posframe)
;;; flymake-posframe.el ends here
