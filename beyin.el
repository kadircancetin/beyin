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
(require 'cl)
(require 's)

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

(defvar beyin-after-buffer-opened-hook nil
  "My custom hook")

(defvar beyin-after-insert-llm-response-hook nil
  "My custom hook")



(defcustom beyin--system-prompt
  "
<llm_info>
You are an LLM integrated within a text editor, designed to assist with brief, concise and helpful responses.

Users may select text. It there is selected text, it will be enclosed in <context> tag. This will help you to understand the what is question about.
</llm_info>

<review_task>
If user ask you to review a code, try to find bugs, not talk much about what is code for.
</review_task>

<grammar_fix>
If user say grammar, fix user grammar
</grammar_fix>
"
  "HERE")


(defcustom beyin--default-buffer-name
  "BEYIN"
  "HERE")



(defun beyin--tokenize-buffer (content)
  "returns '((role . \"SYSTEM\") (text . \"test 000\") (role . \"ASSISTANT\") (text . \"test  \") "
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


(defun beyin--parse-conversation-as-list-of-string (tokens)
  "Only calculate USER and ASSISTANT messages. Returns list of string of the conversation. Starting
from user. Return will be like '(\"user msg\" nil \"user msg2\" \"asistant msg\")"
  (let ((result '())
        (current-role nil)
        (expected-role "user"))

    (dolist (token tokens)
      (let ((key (car token))
            (value (cdr token)))
        (pcase key
          ('role
           (setq value (downcase value))
           (setq current-role value)
           (unless (member value '("user" "assistant"))
             (setq current-role nil)))
          ('text
           (when current-role
             (if (s-equals? current-role expected-role)
                 (progn
                   (push value result)
                   (if (s-equals? expected-role "user")
                       (setq expected-role "assistant")
                     (setq expected-role "user")))
               (progn
                 (push nil result)
                 (push value result))))))))
    (nreverse result)))


(defun beyin--get-last-system-message (tokens)
  "returns string"
  (let ((found nil)
        (result nil))
    (dolist (token tokens)
      (let ((key (car token))
            (value (cdr token)))
        (pcase key
          ('role
           (when (s-equals? (downcase value) "system")
             (setq found t)))
          ('text
           (when found
             (setq found nil)
             (setq result value))))))
    result))

(defun beyin--if-last-message-empty-user-message (tokens)
  "returns nil or t"
  (let ((found nil)
        (last-user-message nil))
    (dolist (token tokens)
      (let ((key (car token))
            (value (cdr token)))
        (pcase key
          ('role
           (when (s-equals? (downcase value) "user")
             (setq found t)))
          ('text
           (when found
             (setq found nil)
             (setq last-user-message value))))))

    (if (s-blank? (s-trim last-user-message))
        t
      nil)))




(setq beyin--last-buffer nil)



(defun beyin--end-of-response-hook ()
  (end-of-buffer)
  (unless (beyin--if-last-message-empty-user-message (beyin--tokenize-buffer (buffer-substring-no-properties (point-min) (point-max))))
    (insert "\n\n# --USER:\n"))

  (gptel--update-status " READY" 'success))

;; TODO them make buffer local
(add-hook
 'gptel-post-response-functions
 (lambda (&rest _)
   (with-current-buffer beyin--last-buffer (beyin--end-of-response-hook))))

(add-hook
 'gptel-pre-response-hook
 (lambda()
   (with-current-buffer beyin--last-buffer
     (gptel--update-status " Waiting LLM..." 'warning)
     (end-of-buffer)
     (insert "\n\n# --ASSISTANT:\n"))))




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
  (let ((buffer (current-buffer)))
    (setq beyin--last-buffer buffer)

    (let ((tokens (beyin--tokenize-buffer (buffer-substring-no-properties (point-min) (point-max)))))
      (if (beyin--if-last-message-empty-user-message tokens)
          (delete-window)
        (gptel--update-status " Waiting LLM..." 'warning)
        (gptel-request (beyin--parse-conversation-as-list-of-string tokens)
          :buffer buffer
          :stream t
          :system (beyin--get-last-system-message tokens)
          :callback
          (lambda (response info)
            (if (not response)
                (message "gptel-quick failed with message: %s" (plist-get info :status))
              (with-current-buffer buffer
                (run-hooks 'beyin-after-insert-llm-response-hook)
                (when (stringp response)
                  (save-excursion
                    (end-of-buffer)
                    (insert response)))))))))))


(defun beyin-chat--open-and-jump-buffer()
  (interactive)
  (let ((has-old-session (get-buffer beyin--default-buffer-name)))
    (with-current-buffer (get-buffer-create beyin--default-buffer-name)
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
        (goto-char (point-max))))

    (unless (get-buffer-window beyin--default-buffer-name 0)
      (display-buffer (get-buffer-create beyin--default-buffer-name)
                      `((display-buffer-in-side-window)
                        (side . right)
                        (window-width . 80))))

    (select-window (get-buffer-window beyin--default-buffer-name 0))
    (goto-char (point-max))
    (run-hooks 'beyin-after-buffer-opened-hook)))

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
                (gptel-abort (current-buffer))
                (beyin--end-of-response-hook)
                )))



(provide 'beyin)

;;; beyin.el ends here
