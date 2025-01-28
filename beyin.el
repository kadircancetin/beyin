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



(defun beyin--tokenize-role-line (line)
  "Helper function to tokenize a role line."
  (let* ((role-name (substring line (length "# --")))
         (role-name (if (string-suffix-p ":" role-name)
                        (substring role-name 0 -1)
                      role-name)))
    (unless role-name
      (error "Invalid role line: Missing role name after '# --'"))
    (cons 'role role-name)))

(defun beyin--tokenize-text-line (line tokens)
  "Helper function to tokenize a text line."
  (if (eq (car (first tokens)) 'text)
      (cons (cons 'text (concat (cdr (pop tokens)) "\n" line)) tokens)
    (cons (cons 'text line) tokens)))

(defun beyin--tokenize-buffer-content (content)
  "Returns '((role . \"SYSTEM\") (text . \"test 000\") (role . \"ASSISTANT\") (text . \"test  \"))"
  (let ((lines (split-string content "\n"))
        (tokens '())
        (role-start "# --"))
    (dolist (line lines)
      (if (string-prefix-p role-start line)
          (push (beyin--tokenize-role-line line) tokens)
        (setq tokens (beyin--tokenize-text-line line tokens))))
    (reverse tokens)))


(defun beyin--extract-conversation-turns (tokens)
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
           ;; If the token is a role, update the current role.
           ;; Only consider 'user' and 'assistant' roles.
           (setq value (downcase value))
           (setq current-role (if (member value '("user" "assistant"))
                                  value
                                nil)))
          ('text
           ;; If the token is text and we have a current role:
           (when current-role
             (cond
              ((string= current-role expected-role)
               ;; If the current role matches the expected role, add the text to the result.
               (push value result)
               ;; Switch the expected role.
               (setq expected-role (if (string= expected-role "user")
                                       "assistant"
                                     "user")))
              (t
               ;; Otherwise, add nil (to indicate a skipped turn) and then the text.
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

(defun beyin--last-user-message-empty-p (tokens)
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




(setq beyin--last-used-buffer nil)



(defun beyin--handle-end-of-response ()
  (end-of-buffer)
  (unless (beyin--last-user-message-empty-p (beyin--tokenize-buffer-content (buffer-substring-no-properties (point-min) (point-max))))
    (beyin--insert-role-marker "USER"))

  (gptel--update-status " READY" 'success))

;; TODO them make buffer local
(add-hook
 'gptel-post-response-functions
 (lambda (&rest _)
   (with-current-buffer beyin--last-used-buffer (beyin--handle-end-of-response))))

(add-hook
 'gptel-pre-response-hook
 (lambda()
   (with-current-buffer beyin--last-used-buffer
     (gptel--update-status " Waiting LLM..." 'warning)
     (beyin--insert-role-marker "ASSISTANT"))))




(defun beyin--insert-role-marker (role)
  "Insert a role marker at the end of the current buffer.
ROLE should be a string like \"USER\" or \"ASSISTANT\"."
  (end-of-buffer)
  (insert (format "\n\n# --%s:\n" role)))


(defun beyin--prepare-region-context()
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
    (setq beyin--last-used-buffer buffer)

    (let ((tokens (beyin--tokenize-buffer-content (buffer-substring-no-properties (point-min) (point-max)))))
      (if (beyin--last-user-message-empty-p tokens)
          (when (get-buffer-window buffer)
            (delete-window (get-buffer-window buffer)))
        (gptel--update-status " Waiting LLM..." 'warning)
        (gptel-request (beyin--extract-conversation-turns tokens)
          :buffer buffer
          :stream t
          :system (beyin--get-last-system-message tokens)
          :callback
          (lambda (response info)
            (if (not response)
                (insert "# --ASSISTANT:\n\ngptel failed with message: %s" (plist-get info :status))
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
  (let ((context-msg (beyin--prepare-region-context)))
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

(dolist (role-face '(("USER" . beyin-user-title-font)
                     ("ASSISTANT" . beyin-asistant-title-font)
                     ("SYSTEM" . beyin-system-title-font)))
  (font-lock-add-keywords 'beyin-mode
                          `((,(concat "^# --\\(" (car role-face) "\\):$") 1 ',(cdr role-face) prepend)) 'append))


(add-to-list 'auto-mode-alist (cons "\\.beyin\\'" 'beyin-mode))

(advice-add 'keyboard-quit :before
            (lambda ()
              (when (eq major-mode 'beyin-mode)
                (gptel-abort (current-buffer))
                (beyin--handle-end-of-response)
                )))



(provide 'beyin)

;;; beyin.el ends here
