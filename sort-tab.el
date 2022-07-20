;;; sort-tab.el --- Smarter tab solution for Emacs, it sort tab with using frequency.   -*- lexical-binding: t; -*-

;; Filename: sort-tab.el
;; Description: Provide an out of box configuration to use sort-tab in Emacs.
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2021, Andy Stewart, all rights reserved.
;; Created: 2021-10-26 22:14:34
;; Version: 1.0
;; Last-Updated: 2021-10-26 22:14:34
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/sort-tab.el
;; Keywords:
;; Compatibility: GNU Emacs 29.0.50
;;
;; Features that might be required by this library:
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Smarter tab solution for Emacs, it sort tab with using frequency.
;;

;;; Installation:
;;
;; Put sort-tab.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'sort-tab)
;; (sort-tab-mode t)
;;
;; No need more.
;;

(defgroup sort-tab nil
  "Display sort-tab in top of Emacs."
  :group 'convenience)

(defcustom sort-tab-buffer-name "*sort-tab*"
  "The buffer name of sort-tab."
  :type 'string)

(defcustom sort-tab-height 30
  "The height of sort-tab."
  :type 'integer)

(defcustom sort-tab-name-max-length 50
  "Max length of tab name."
  :type 'int)

(defcustom sort-tab-separator "|"
  "The separator between tabs."
  :type 'string)

(defcustom sort-tab-sort-weights 5
  "The sort weights, avoid frequent interchangements of two tabs with similar frequencies."
  :type 'integer)

(defcustom sort-tab-align 'left
  "The align of sort-tab."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Center" center)))

(defface sort-tab-current-tab-face
  `((t
     :background ,(face-background 'highlight) :foreground ,(face-foreground 'highlight) :bold t))
  "Face for current tab.")

(defface sort-tab-other-tab-face
  `((t
     :foreground ,(face-foreground 'default) :bold nil))
  "Face for inactive tabs.")

(defface sort-tab-separator-face
  `((t
     :foreground ,(face-attribute 'font-lock-comment-face :foreground) :bold t))
  "Face for separator.")

(defconst sort-tab-propertized-separator
  (propertize sort-tab-separator 'face 'sort-tab-separator-face))

(defvar sort-tab-mode-map
  (let ((km (make-sparse-keymap)))
    km)
  "Keymap to use in sort-tab mode.")

(defvar sort-tab-window nil)

(defvar sort-tab-buffer-freq-count-timer nil
  "Timer used for count buffer used frequency.")

(defvar-local sort-tab-buffer-freq 0
  "Used frequency of current buffer.")

(defvar sort-tab-count-freq-idle-time 1
  "Add used frequency after being idle for this much secs.")

(defvar sort-tab-visible-buffers nil)

(defvar sort-tab-last-active-buffer nil)

(define-minor-mode sort-tab-mode
  "Toggle display of a sort-tab.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled.

\\{sort-tab-mode-map}"
  :group 'sort-tab
  :require 'sort-tab
  :global t
  :keymap sort-tab-mode-map
  (if sort-tab-mode
      (sort-tab-turn-on)
    (sort-tab-turn-off)))

(defun sort-tab-get-buffer ()
  (get-buffer-create sort-tab-buffer-name))

(defun sort-tab-turn-on ()
  (interactive)
  ;; Create sort-tab buffer.
  (with-current-buffer (sort-tab-get-buffer)
    ;; Disable line numbers mode.
    (when display-line-numbers
      (setq-local display-line-numbers nil))
    ;; Disable tab-line.
    (when (version< "27.0" emacs-version)
      (setq-local tab-line-format nil))
    ;; Disable hl-line, header-line and mode-line in input buffer.
    (setq-local header-line-format nil)
    (setq-local mode-line-format nil)
    ;; Disable cursor type if option `disable-cursor' is non-nil.
    (setq-local cursor-type nil)
    ;; Set Mini height to make sure sort-tab window always 1-line height.
    (setq-local window-min-height 1)
    ;; Disable wrap line.
    (setq-local truncate-lines t)
    ;; Disable window resize.
    (setq-local window-size-fixed 'height))

  ;; Create sort-tab window.
  (sort-tab-create-window)

  ;; Start count buffer frequency.
  (sort-tab-start-count-freq)

  ;; Update sort-tab buffer list.
  (sort-tab-update-list)

  ;; Add update hook.
  (add-hook 'buffer-list-update-hook #'sort-tab-update-list))

(defun sort-tab-create-window ()
  ;; Split top window.
  (ignore-errors
    (dotimes (i 50)
      (windmove-up)))
  (split-window-vertically 1)

  ;; Record sort-tab window.
  (setq sort-tab-window (selected-window))
  (switch-to-buffer (sort-tab-get-buffer))
  (other-window 1)

  ;; Set window dedicated to make sure pop buffer won't use sort-tab window.
  (set-window-dedicated-p sort-tab-window t)

  ;; Make sure sort-tab window can skip `delete-other-windows' and 'other-window'.
  (set-window-parameter sort-tab-window 'no-delete-other-windows t)
  (set-window-parameter sort-tab-window 'window-side 'top)
  (set-window-parameter sort-tab-window 'window-slot 0)
  (set-window-parameter sort-tab-window 'no-other-window t))

(defun sort-tab-turn-off ()
  (interactive)
  (setq sort-tab-mode nil)

  (when (sort-tab-live-p)
    ;; Reset window parameter.
    (set-window-parameter sort-tab-window 'no-delete-other-windows nil)
    (set-window-parameter sort-tab-window 'window-side nil)
    (set-window-parameter sort-tab-window 'window-slot nil)
    (set-window-parameter sort-tab-window 'no-other-window nil)

    ;; Kill sort-tab window.
    (delete-window (get-buffer-window (sort-tab-get-buffer))))

  ;; Reset sort-tab window.
  (setq sort-tab-window nil)

  ;; Stop count.
  (sort-tab-stop-count-freq)

  ;; Remove update hook.
  (remove-hook 'buffer-list-update-hook #'sort-tab-update-list))

(defun sort-tab-live-p ()
  (and (buffer-live-p (get-buffer sort-tab-buffer-name))
       sort-tab-window
       (window-live-p sort-tab-window)))

(defun sort-tab-increase-buffer-freq ()
  "Increase the used frequency of current buffer by 1."
  (cl-incf sort-tab-buffer-freq))

(defun sort-tab-start-count-freq ()
  "Start counting buffer used frequency."
  (setq sort-tab-buffer-freq-count-timer
        (run-with-idle-timer sort-tab-count-freq-idle-time
                             t #'sort-tab-increase-buffer-freq)))

(defun sort-tab-stop-count-freq ()
  "Stop counting buffer used frequency."
  (cancel-timer sort-tab-buffer-freq-count-timer)
  (setq sort-tab-buffer-freq-count-timer nil))

(defun sort-tab-buffer-freq (buf)
  "Return the used frequency of buffer BUF."
  (or (buffer-local-value 'sort-tab-buffer-freq buf)
      0))

(defun sort-tab-get-buffer-mode (buf)
  (with-current-buffer buf
    major-mode))

(defun sort-tab-is-eaf-browser-buffer-p (buf)
  (with-current-buffer buf
    (and (eq major-mode 'eaf-mode)
         (equal eaf--buffer-app-name "browser"))))

(defun sort-tab-is-eaf-file-manager-buffer-p (buf)
  (with-current-buffer buf
    (and (eq major-mode 'eaf-mode)
         (equal eaf--buffer-app-name "file-manager"))))

(defun sort-tab-buffer-freq-higher-p (buf1 buf2)
  "Return t if the used frequency of BUF1 is higher than BUF2."
  (cond
   ((and (sort-tab-is-eaf-browser-buffer-p buf1)
         (not (sort-tab-is-eaf-browser-buffer-p buf2)))
    t)
   ((and (sort-tab-is-eaf-browser-buffer-p buf2)
         (not (sort-tab-is-eaf-browser-buffer-p buf1)))
    nil)
   ((and (sort-tab-is-eaf-file-manager-buffer-p buf1)
         (not (sort-tab-is-eaf-file-manager-buffer-p buf2)))
    nil)
   ((and (sort-tab-is-eaf-file-manager-buffer-p buf2)
         (not (sort-tab-is-eaf-file-manager-buffer-p buf1)))
    t)
   (t
    (> (/ (sort-tab-buffer-freq buf1) sort-tab-sort-weights)
       (/ (sort-tab-buffer-freq buf2) sort-tab-sort-weights)))))

(defun sort-tab-is-magit-buffer-p (buf)
  (with-current-buffer buf              ;not magit buffer
    (or (derived-mode-p 'magit-status-mode)
        (derived-mode-p 'magit-process-mode)
        (derived-mode-p 'magit-diff-mode)
        (derived-mode-p 'magit-log-mode)
        (derived-mode-p 'magit-status-mode)
        )))

(defun sort-tab-buffer-need-hide-p (buf)
  (let* ((name (buffer-name buf)))
    (or
     (cl-some (lambda (prefix) (string-prefix-p prefix name)) '("*" " *" "COMMIT_EDITMSG"))
     (eq (aref name 0) ?\s)
     (sort-tab-is-magit-buffer-p buf)
     )))

(defun sort-tab-is-normal-buffer-p (current-buffer)
  (and
   (not (window-minibuffer-p))
   (not (eq current-buffer sort-tab-last-active-buffer))
   (not (sort-tab-buffer-need-hide-p current-buffer))
   ))

(defun sort-tab-is-hidden-buffer-p (buf)
  (let* ((name (buffer-name buf)))
    (and
     (sort-tab-buffer-need-hide-p buf)
     (not (window-minibuffer-p))
     (not (cl-some (lambda (prefix) (string-prefix-p prefix name)) '(" *eldoc" " *snails" "*Help" "*Flycheck" "COMMIT_EDITMSG" " *rime" "*color-rg*")))
     (not (string-equal sort-tab-buffer-name name))
     (not (sort-tab-is-magit-buffer-p buf))
     )))

(cl-defmacro sort-tab-update-tabs (&rest body)
  `(with-current-buffer (sort-tab-get-buffer)
     ;; Clean buffer.
     (erase-buffer)

     ;; Update tabs.
     ,@body

     (when (eq sort-tab-align 'center)
       (goto-char (point-min))
       (insert sort-tab-propertized-separator)
       (let* ((width (window-width (get-buffer-window)))
              (content-length (length (buffer-string)))
              (padding (max 0 (/ (- width content-length) 2))))
         (goto-char (point-min))
         (insert (make-string padding ?\s))))

     ;; Record last active buffer.
     (setq sort-tab-last-active-buffer (current-buffer))
     ))

(defun sort-tab-update-list ()
  (let ((current-buffer (window-buffer)))
    (cond
     ;; Display tabs if current-buffer is normal buffer.
     ((sort-tab-is-normal-buffer-p current-buffer)
      ;; Debug usage.
      ;; (with-current-buffer (get-buffer-create "sort-tab-debug")
      ;;   (goto-char (point-max))
      ;;   (insert (format "**** %s %s\n"
      ;;                   last-command
      ;;                   (buffer-name current-buffer))))

      (let* ((current-tab-start-column 0)
             (current-tab-end-column 0)
             (tab-window (get-buffer-window (sort-tab-get-buffer)))
             found-current-tab
             tab)
        (sort-tab-update-tabs
         ;; Don't sort tabs if using sort-tab commands.
         (when (not (string-prefix-p "sort-tab-" (prin1-to-string last-command)))
           (setq sort-tab-visible-buffers (sort-tab-get-buffer-list)))

         (dolist (buf sort-tab-visible-buffers)
           ;; Insert tab.
           (setq tab (sort-tab-get-tab-name buf current-buffer))
           (insert tab)
           (insert sort-tab-propertized-separator)

           ;; Calculate the current tab column.
           (unless found-current-tab
             (when (eq buf current-buffer)
               (setq found-current-tab t)
               (setq current-tab-start-column current-tab-end-column))
             (setq current-tab-end-column (+ current-tab-end-column (length tab) (length sort-tab-separator)))))

         ;; Make tab always visible.
         (when tab-window
           (with-selected-window tab-window
             (cond ((> current-tab-end-column (+ (window-hscroll) (window-width)))
                    (scroll-left (+ (- current-tab-end-column (window-hscroll) (window-width)) (/ (window-width) 2))))
                   ((< current-tab-start-column (window-hscroll))
                    (set-window-hscroll tab-window current-tab-start-column))
                   ))))))
     ;; Only display hide buffer at top if current buffer is match hide rule.
     ((sort-tab-is-hidden-buffer-p current-buffer)
      (sort-tab-update-tabs
       ;; Insert current buffer.
       (insert (sort-tab-get-tab-name current-buffer current-buffer))
       (insert sort-tab-propertized-separator)))
     ;; Erase sort-tab content if current buffer is sort-tab buffer.
     ((string-equal sort-tab-buffer-name (buffer-name current-buffer))
      (sort-tab-update-tabs)))))

(defun sort-tab-get-tab-name (buf current-buffer)
  (propertize
   (format " %s "
           (let ((bufname (buffer-name buf))
                 (ellipsis "..."))
             ;; We need remove space in web page title.
             (when (sort-tab-is-eaf-browser-buffer-p buf)
               (setq bufname (replace-regexp-in-string "\\s-" "" bufname)))

             (if (> (length bufname) sort-tab-name-max-length)
                 (format "%s%s" (substring bufname 0 (- sort-tab-name-max-length (length ellipsis))) ellipsis)
               bufname)))
   'face
   (if (eq buf current-buffer)
       'sort-tab-current-tab-face
     'sort-tab-other-tab-face)))

(defun sort-tab-get-buffer-list ()
  (let ((bufs (buffer-list)))
    (setq bufs (cl-remove-if #'sort-tab-buffer-need-hide-p bufs))
    (setq bufs (sort bufs #'sort-tab-buffer-freq-higher-p))
    bufs))

(defun sort-tab-get-index ()
  (cl-position (window-buffer) sort-tab-visible-buffers :test #'eq))

(defun sort-tab-get-next-buffer ()
  (let ((index (sort-tab-get-index)))
    (cond
     ((or (null index) (eq index (1- (length sort-tab-visible-buffers))))
      (car sort-tab-visible-buffers))
     (t
      (nth (1+ index) sort-tab-visible-buffers)))))

(defun sort-tab-get-prev-buffer ()
  (let ((index (sort-tab-get-index)))
    (cond
     ((or (null index) (eq index 0))
      (car (last sort-tab-visible-buffers)))
     (t
      (nth (1- index) sort-tab-visible-buffers)))))

(defun sort-tab-get-first-buffer ()
  (cl-first sort-tab-visible-buffers))

(defun sort-tab-get-last-buffer ()
  (car (last sort-tab-visible-buffers)))

(defun sort-tab-select-prev-tab ()
  (interactive)
  (switch-to-buffer (sort-tab-get-prev-buffer)))

(defun sort-tab-select-next-tab ()
  (interactive)
  (switch-to-buffer (sort-tab-get-next-buffer)))

(defun sort-tab-select-first-tab ()
  (interactive)
  (switch-to-buffer (sort-tab-get-first-buffer)))

(defun sort-tab-select-last-tab ()
  (interactive)
  (switch-to-buffer (sort-tab-get-last-buffer)))

(defun sort-tab-close-current-tab ()
  (interactive)
  (let* ((buf (window-buffer))
         (prev-buffer (sort-tab-get-prev-buffer))
         (next-buffer (sort-tab-get-next-buffer))
         (last-buffer (sort-tab-get-last-buffer))
         (is-last-buffer (eq buf last-buffer)))
    ;; Then kill current buffer.
    (sort-tab-kill-buffer buf)
    ;; Switch to previous buffer if current buffer is last buffer,
    ;; otherwise switch to next buffer.
    (if is-last-buffer
        (when (buffer-live-p prev-buffer)
          (switch-to-buffer prev-buffer))
      (when (buffer-live-p next-buffer)
        (switch-to-buffer next-buffer))
      )))

(defun sort-tab-close-all-tabs ()
  (interactive)
  (let ((visible-buffers sort-tab-visible-buffers))
    (setq sort-tab-visible-buffers nil)
    (dolist (buf visible-buffers)
      (kill-buffer buf))))

(defun sort-tab-close-mode-tabs ()
  (interactive)
  (let ((modes (sort-tab-get-buffer-modes))
        (current-mode (sort-tab-get-current-buffer-mode))
        close-mode)
    (setq close-mode (completing-read (format "Close buffers match mode (%s): " current-mode) modes))
    (when (string-equal close-mode "")
      (setq close-mode current-mode))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (or (and (string-prefix-p "eaf:" close-mode)
                       (eq major-mode 'eaf-mode)
                       (string-equal eaf--buffer-app-name (cadr (split-string close-mode ":"))))
                  (string-equal (prin1-to-string major-mode) close-mode))
          (sort-tab-kill-buffer buffer)
          )))))

(defun sort-tab-kill-buffer (buffer)
  ;; Update `sort-tab-visible-buffers' first.
  (setq sort-tab-visible-buffers (delete buffer sort-tab-visible-buffers))
  (kill-buffer buffer))

(defun sort-tab-get-buffer-modes ()
  (let ((mode-table (make-hash-table :test 'equal))
        modes)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (sort-tab-is-normal-buffer-p buffer)
          (puthash (sort-tab-get-current-buffer-mode) "" mode-table))))
    (maphash (lambda (k v) (push k modes)) mode-table)
    modes))

(defun sort-tab-get-current-buffer-mode ()
  (if (eq major-mode 'eaf-mode)
      (format "eaf:%s" eaf--buffer-app-name)
    (prin1-to-string major-mode)))

(defun sort-tab-select-visible-nth-tab (&optional tab-index)
  (interactive "p")
  (switch-to-buffer (nth (1- tab-index) sort-tab-visible-buffers)))

(defun sort-tab-select-visible-tab ()
  (interactive)
  (let* ((event last-command-event)
         (key (make-vector 1 event))
         (key-desc (key-description key)))
    (sort-tab-select-visible-nth-tab
     (string-to-number (car (last (split-string key-desc "-")))))))

(defun sort-tab-kill-buffer-advisor (orig-fun &optional arg &rest args)
  (if (equal (buffer-name) sort-tab-buffer-name)
      (message "sort-tab buffer can't be kill, please use `sort-tab-turn-off' command to quit sort-tab.")
    (apply orig-fun arg args)))
(advice-add #'kill-buffer :around #'sort-tab-kill-buffer-advisor)

(provide 'sort-tab)

;;; sort-tab.el ends here
