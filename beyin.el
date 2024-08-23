;;; beyin.el --- HERE -*- lexical-binding: t -*-

;; Copyright (C) 2024  Kadir Can Çetin

;; Author: Kadir Can Çetin <kadircancetin@gmail.com>
;; Keywords: convenience, wp, HERE
;; Package-Requires: HERE
;; URL: https://github.com/kadircancetin/beyin
;; Version: 0.0.0

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
;;; HERE

;;; Code:

(require 'gptel)


(defgroup beyin nil "HERE")



(defface beyin-user-title-font
  '((t (:foreground "YellowGreen" :height 1.3)))
  "HERE"
  ;; :type 'integer
  :group 'beyin)

(defface beyin-asistant-title-font
  '((t (:foreground "Indianred2" :height 1.3)))
  "HERE"
  :group 'beyin)

(defface beyin-system-title-font
  '((t (:height 1.3)))
  "HERE"
  :group 'beyin)



(defcustom beyin--system-prompt
  "You are an LLM integrated within a text editor, designed to assist with concise and helpful responses. Users may select text, which will be enclosed in <context> tags."
  "HERE")


(defcustom beyin--default-buffer-name
  "BEYIN"
  "HERE")



(defun beyin--build-context()
  (interactive)
  (if (use-region-p)
      (concat
       "\n## CONTEXT"
       "\n<file> " (buffer-file-name) " </file>"
       "\n<context>"
       "\n```"
       "\n" (buffer-substring-no-properties (region-beginning) (region-end))
       "\n```"
       "\n</context>"
       "\n## INPUT"
       "\n")
    ""))

(defun beyin--parse-chat-content (content)
  "Parse chat content into a list of alists."
  (let ((lines (split-string content "\n"))
        (current-role nil)
        (current-content "")
        (result '()))
    (dolist (line lines)
      (cond
       ;; New role
       ((string-match "^# --\\(.*\\):$" line)
        (when (and current-role (not (string= current-content "")))
          (push `(:role ,current-role :content ,(string-trim current-content)) result)
          (setq current-content ""))
        (setq current-role (downcase (match-string 1 line))))
       ;; Content
       ((not (string= line ""))
        (setq current-content (concat current-content line "\n")))))
    ;; Add the last item
    (when (and current-role (not (string= current-content "")))
      (push `(:role ,current-role :content ,(string-trim current-content)) result))
    (nreverse result)))



(setq beyin--waiting-response-overlay nil)
(setq beyin--last-buffer nil)




(defun beyin--end-of-response-hook ()
  (delete-overlay beyin--waiting-response-overlay)
  (read-only-mode 0)
  (end-of-buffer)
  (insert "\n\n# --USER:\n")
  (font-lock-ensure))

;; TODO them make buffer local
(add-hook
 'gptel-post-response-functions
 (lambda (&rest _)
   (with-current-buffer beyin--last-buffer (beyin--end-of-response-hook))))

(add-hook
 'gptel-pre-response-hook
 (lambda()
   (with-current-buffer beyin--last-buffer
     (end-of-buffer)
     (insert "\n\n# --ASSISTANT:\n")
     (font-lock-ensure)
     (read-only-mode 1))))



(defun beyin--handle-new-session ()
  (interactive)
  (let ((has-old-session (get-buffer beyin--default-buffer-name))
        (context-msg (beyin--build-context)))

    (with-current-buffer (get-buffer-create beyin--default-buffer-name)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (if (not has-old-session)
            (insert (concat "# --SYSTEM:\n" beyin--system-prompt "\n\n# --USER:\n" context-msg))
          (insert context-msg))
        (beyin-mode)
        (visual-line-mode 1)))

    (unless (get-buffer-window beyin--default-buffer-name 0)
      (display-buffer (get-buffer-create beyin--default-buffer-name)
                      `((display-buffer-in-side-window)
                        (side . right)
                        (window-width . 99))))
    (select-window (get-buffer-window beyin--default-buffer-name 0))
    (goto-char (point-max))
    (god-local-mode 0)
    (beacon-blink)))

(defun beyin--handle-send-chat-msg ()
  (setq beyin--waiting-response-overlay (make-overlay (point-max) (point-max)))
  (overlay-put beyin--waiting-response-overlay 'after-string "\n\n--> LLM THINKING...")

  (let ((buffer (current-buffer)))
    (setq beyin--last-buffer buffer)
    (gptel-request
     (beyin--parse-chat-content (buffer-substring-no-properties (point-min) (point-max)))
     :buffer buffer
     :stream t
     :callback
     (lambda (response info)
       (delete-overlay beyin--waiting-response-overlay)
       (if (not response) (message "gptel-quick failed with message: %s" (plist-get info :status))
         (with-current-buffer buffer
           (let ((inhibit-read-only t))
             (goto-char (point-max))
             (insert response))
           (font-lock-ensure)))))))


;;;###autoload
(defun beyin-chat ()
  (interactive)
  (if (not (eq major-mode 'beyin-mode))
      (beyin--handle-new-session)
    (beyin--handle-send-chat-msg)))



(define-derived-mode beyin-mode markdown-mode "Beyin Mode")

(font-lock-add-keywords 'beyin-mode `(("^# --\\(USER\\):$" 1 'beyin-user-title-font prepend)) 'append)
(font-lock-add-keywords 'beyin-mode `(("^# --\\(ASSISTANT\\):$" 1 'beyin-asistant-title-font prepend)) 'append)
(font-lock-add-keywords 'beyin-mode `(("^# --\\(SYSTEM\\):$" 1 'beyin-system-title-font prepend)) 'append)

(add-to-list 'auto-mode-alist (cons "\\.beyin\\'" 'beyin-mode))

(define-key beyin-mode-map (kbd "C-g")
            (lambda ()
              (interactive)
              (beyin--end-of-response-hook)
              (gptel-abort (current-buffer))
              (keyboard-quit)))



(provide 'beyin)

;;; beyin.el ends here
