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
  "
<llm_info>
You are an LLM integrated within a text editor, designed to assist with brief, concise and helpful responses.

Users may select text. It there is selected text, it will be enclosed in <context> tag. This will help you to understand the what is question about.
</llm_info>

<review_task>
If user ask you to review a code, try to find bugs, not talk much about what is code for.
</review_task>"
  "HERE")


(defcustom beyin--default-buffer-name
  "BEYIN"
  "HERE")



(defun beyin--tokenize-buffer (content)
  (let ((lines (split-string content "\n"))
        (tokens '())
        (role-start "# --"))
    (dolist (line lines)
      (if (string-prefix-p role-start line)
          ;; tokenize role
          (let* (;; remove role start
                 (role-name (substring line (length role-start)))
                 ;; remove `:` end of role
                 (role-name (if (string-suffix-p ":" role-name)
                                (substring role-name 0 -1)
                              role-name)))
            (push (cons 'role role-name) tokens))
        ;; tokenize text
        ;; combine lines
        (if (eq (car (first tokens)) 'text)
            (push (cons 'text (concat (cdr (pop tokens)) "\n" line)) tokens)
          (push (cons 'text line) tokens))))
    (reverse tokens)))


(defun beyin--parse-for-gptel (tokens)
  (let ((result '())
        (last-role nil))
    (dolist (token tokens)
      (pcase (car token)
        ('role
         (setq last-role (downcase (cdr token))))
        ('text
         (when last-role
           (push `(:role ,last-role :content ,(string-trim (cdr token))) result)))))
    (nreverse result)))


(defun beyin--parse-buffer--for-gptel ()
  "Parse chat content into a list of alists."
  (interactive)
  (beyin--parse-for-gptel (beyin--tokenize-buffer (buffer-substring-no-properties (point-min) (point-max)))))



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



(defun beyin-chat--build-region-context()
  (interactive)
  (concat
   "\n#### CONTEXT:"
   (if (buffer-file-name)
       (concat "\n<file> " (buffer-file-name) " </file>"))
   "\n<context>"
   "\n```"
   "\n" (buffer-substring-no-properties (region-beginning) (region-end))
   "\n```"
   "\n</context>"
   "\n#### INPUT:"
   "\n"))


(defun beyin--handle-send-chat-msg ()
  (setq beyin--waiting-response-overlay (make-overlay (point-max) (point-max)))
  (overlay-put beyin--waiting-response-overlay 'after-string
               (concat "\n\n--> "
                       (if (eq (type-of gptel-model) 'string)
                           gptel-model
                         (symbol-name gptel-model))
                       " THINKING..."))

  (let ((buffer (current-buffer)))
    (setq beyin--last-buffer buffer)

    (gptel-curl-get-response
     (list :data (list
                  :model (gptel--model-name gptel-model)
                  :messages (apply 'vector  (beyin--parse-buffer--for-gptel))
                  :stream t
                  :temperature 0)
           :buffer buffer
           :position (point-marker))

     (lambda (response info)
       (delete-overlay beyin--waiting-response-overlay)
       (if (not response) (message "gptel-quick failed with message: %s" (plist-get info :status))
         (with-current-buffer buffer
           (let ((inhibit-read-only t))
             (goto-char (point-max))
             (insert response))
           (font-lock-ensure)))))))


(defun beyin-chat--open-and-jump-buffer()
  (interactive)
  (let ((has-old-session (get-buffer beyin--default-buffer-name)))
    (with-current-buffer (get-buffer-create beyin--default-buffer-name)
      (let ((inhibit-read-only t))
        (beyin-mode)
        (gptel-mode)
        (visual-line-mode 1)
        (goto-char (point-max))

        ;; initialize system message
        (unless has-old-session
          (insert (concat "# --SYSTEM:\n" beyin--system-prompt "\n\n# --USER:\n"))
          (re-search-backward "# --SYSTEM" nil t)
          (markdown-back-to-heading)
          (outline-hide-subtree)
          (goto-char (point-max)))))

    (unless (get-buffer-window beyin--default-buffer-name 0)
      (display-buffer (get-buffer-create beyin--default-buffer-name)
                      `((display-buffer-in-side-window)
                        (side . right)
                        (window-width . 80))))

    (select-window (get-buffer-window beyin--default-buffer-name 0))
    (goto-char (point-max))
    (god-local-mode 0)
    (beacon-blink)))

(defun beyin-chat--handle-region()
  (interactive)
  (let ((context-msg (beyin-chat--build-region-context)))
    (beyin-chat--open-and-jump-buffer)
    (with-current-buffer (get-buffer-create beyin--default-buffer-name)
      (goto-char (point-max))
      (insert context-msg)
      (re-search-backward "#### CONTEXT" nil t)
      (markdown-back-to-heading)
      (outline-hide-subtree)
      (setq markdown-cycle-subtree-status 'folded)
      (goto-char (point-max)))))



;;;###autoload
(defun beyin-chat ()
  (interactive)
  (cond
   ((region-active-p) (beyin-chat--handle-region))
   ((eq major-mode 'beyin-mode) (beyin--handle-send-chat-msg))
   (t (beyin-chat--open-and-jump-buffer))))


(define-derived-mode beyin-mode markdown-mode "Beyin Mode")

(font-lock-add-keywords 'beyin-mode `(("^# --\\(USER\\):$" 1 'beyin-user-title-font prepend)) 'append)
(font-lock-add-keywords 'beyin-mode `(("^# --\\(ASSISTANT\\):$" 1 'beyin-asistant-title-font prepend)) 'append)
(font-lock-add-keywords 'beyin-mode `(("^# --\\(SYSTEM\\):$" 1 'beyin-system-title-font prepend)) 'append)

(add-to-list 'auto-mode-alist (cons "\\.beyin\\'" 'beyin-mode))

(advice-add 'keyboard-quit :before
            (lambda ()
              (when (eq major-mode 'beyin-mode)
                (gptel-abort (current-buffer)))))



(provide 'beyin)

;;; beyin.el ends here
