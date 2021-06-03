(defun make-toggle-function (buffer-name buffer-create-fn &optional switch-cont)
  "Makes a toggle-function to have raise-or-create behaviour.

Creates a toggle-function that executes BUFFER-CREATE-FN if a
buffer named BUFFER-NAME doesn't exist, switches to the buffer
named BUFFER-NAME if it exists, and switches to the previous
buffer if we are currently visiting buffer BUFFER-NAME.

The SWITCH-CONT argument is a function which, if given, is called
after the buffer has been created or switched to.  This allows
running further actions that setup the state of the buffer or
modify it."
  (lambda ()
    (interactive)
    (let ((target-buf (get-buffer buffer-name)))
     (if target-buf
     (if (eq (current-buffer) target-buf)
         (progn
           (message "switching to other buffer")
           (switch-to-buffer nil))
         (progn
           (message "switching back...")
           (switch-to-buffer buffer-name)
           (when switch-cont (funcall switch-cont))))
       (message "creating buffer...")
       (funcall buffer-create-fn)
       (when switch-cont (funcall switch-cont))))))

;; TODO: this causes runtime errors, since defservlet* is a macro!
(after! simple-httpd
    '(defservlet* capture/:keys/:txt text/plain ()
     (org-capture-string txt keys)))

(defun my/extract-package-name-from-flycheck-error ()
  (interactive)
  (let ((err-at-point (call-interactively #'flycheck-copy-errors-as-kill)))
    (with-temp-buffer
      (insert err-at-point)
      (search-backward-regexp "It is a member of the hidden package")
      (let ((matching-line (thing-at-point 'line)))
        (save-match-data
          (and (string-match "‘\\(.+\\)’" matching-line)
               (kill-new (match-string 1 matching-line))))))))

(setq doom-theme 'doom-monokai-pro)

(setq display-line-numbers-type 'relative)

(defun my/switch-window-then-focus (direction)
    (cond
        ((eq direction 'v) (call-interactively 'evil-window-vsplit))
        ((eq direction 'h) (call-interactively 'evil-window-split))
    )
    (other-window 1))
(defun my/switch-window-then-focus-vertical ()
  (interactive)
  (my/switch-window-then-focus 'v)
)
(defun my/switch-window-then-focus-horizontal ()
  (interactive)
  (my/switch-window-then-focus 'h)
)
(map! :map evil-window-map "v" #'my/switch-window-then-focus-vertical)
(map! :map evil-window-map "s" #'my/switch-window-then-focus-horizontal)

(after! python
  (sp-local-pair 'python-mode "f\"" "\"" :trigger "f\""))

(setq doom-leader-key "SPC")
(setq doom-localleader-key "SPC m")
(setq doom-leader-alt-key "M-SPC")
(setq doom-localleader-alt-key "M-SPC m")

(evil-set-initial-state 'calc-mode 'emacs)

(when (daemonp)
  (exec-path-from-shell-initialize))

(setq comint-scroll-to-bottom-on-output t)

(setq completion-styles `(basic partial-completion emacs22 initials ,(if (version<= emacs-version "27.0") 'helm-flex 'flex)))

(after! helm-files
(add-hook 'helm-mode-hook 'key-chord-mode)
(setq key-chord-one-keys-delay 0.02)
(key-chord-define helm-find-files-map ".." #'helm-find-files-up-one-level))

(map! :map comint-mode-map
      "S-<up>" #'helm-comint-input-ring)

(setq helm-ff-skip-git-ignored-files t)

(map! :map flycheck-mode-map
      "M-<down>" #'flycheck-next-error
      "M-<right>" #'flycheck-next-error
      "M-<up>" #'flycheck-previous-error
      "M-<left>" #'flycheck-previous-error)

(defun my/dired-here ()
  (interactive)
  (find-file "."))
(map! :leader ">" #'my/dired-here)

(add-hook 'compilation-shell-minor-mode 'goto-address-mode)

(defun my/create-lazy-project (filename)
  (interactive "MProject name? ")
    (let ((new-dirname (concat (getenv "HOME") "/Projects/" filename)))
        (make-directory new-dirname)
        (funcall-interactively #'projectile-add-known-project new-dirname)))
(map! :map doom-leader-project-map "n" #'my/create-lazy-project)

(add-to-list 'display-buffer-alist
            `(,"^\\*eww\\*"
                (display-buffer-reuse-window
                display-buffer-in-side-window)
                (reusable-frames     . visible)
                (side                . right)
                (window-height       . 0.45)
                ))

(add-to-list 'display-buffer-alist
            `(,"^\\*ob-ipython-inspect\\*"
                (display-buffer-reuse-window
                display-buffer-in-side-window)
                (reusable-frames     . visible)
                (side                . right)
                (window-height       . 0.40)
                ))
(map! :map org-mode-map "C-c ." :desc "Inspect" #'ob-ipython-inspect)

(add-hook 'after-init-hook 'company-tng-mode)

(setq org-directory "~/org/")

(use-package pdf-view-restore
  :after pdf-tools
  :config
  (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode))

(setq pdf-view-restore-filename "~/.emacs.d/.pdf-view-restore")

(defun my/haskell-extract-type-alias (type-name)
  (interactive "*sName for type alias: ")
  (let (chosen-symbol bounds start-pt end-pt)
        (setq chosen-symbol (thing-at-point 'symbol))
        (setq bounds (bounds-of-thing-at-point 'symbol))
        (setq start-pt (car bounds))
        (setq end-pt (cdr bounds))
  (delete-region start-pt end-pt)
  (insert type-name)
  (save-excursion
    (evil-insert-newline-above)
    (insert (format "type %s = %s" type-name chosen-symbol)))
  ))

(map! :after haskell-mode
      :map haskell-mode-map
      :localleader (:prefix ("x" . "extract")
                    :desc "Extract type alias" "t" #'my/haskell-extract-type-alias))

;; (add-hook 'haskell-mode-hook #'lsp)
;; (add-hook 'haskell-literate-mode-hook #'lsp)

(defun my/haskell--prompt-for-import ()
  (interactive)
  (completing-read "Which module?" (my/haskell--get-import-suggestions (thing-at-point 'symbol))))
(defun my/haskell-auto-import ()
  (interactive)
  (haskell-add-import (my/haskell--prompt-for-import)))

(map! :after haskell-mode
      :map haskell-mode-map
      :localleader (:prefix ("I" . "imports")
                    :desc "Auto-import" "i" #'my/haskell-auto-import))

(with-eval-after-load "haskell-mode"
    ;; This changes the evil "O" and "o" keys for haskell-mode to make sure that
    ;; indentation is done correctly. See
    ;; https://github.com/haskell/haskell-mode/issues/1265#issuecomment-252492026.
    (defun haskell-evil-open-above ()
      (interactive)
      (evil-digit-argument-or-evil-beginning-of-line)
      (haskell-indentation-newline-and-indent)
      (evil-previous-line)
      (haskell-indentation-indent-line)
      (evil-append-line nil))

    (defun haskell-evil-open-below ()
      (interactive)
      (evil-append-line nil)
      (haskell-indentation-newline-and-indent))

    (evil-define-key 'normal haskell-mode-map
      "o" 'haskell-evil-open-below
      "O" 'haskell-evil-open-above)
  )

(add-hook 'haskell-mode 'num3-mode)

(defun my//get-haskage-package-versions (package-name)
  (split-string (shell-command-to-string (format "python3 ~/scripts/get-hackage-versions.py %s" package-name)) "\n"))

(defun my/fancy-add-cabal-import (package-name)
  (interactive "sPackage name? ")
    (haskell-cabal-add-dependency
     (format "%s >= %s" package-name
             (completing-read "Which version?"
                              (reverse (my//get-haskage-package-versions package-name)))))
    (dante-restart))

(map! :after haskell-mode
      :map haskell-mode-map
      :localleader (:prefix ("I" . "imports")
                    :desc "Fancy import" "I" #'my/fancy-add-cabal-import))

(after! purescript-mode
    (add-hook 'purescript-mode-hook 'psc-ide-mode))

(defun ambrevar/sly-prepare-prompt (old-func &rest args)
  (let ((package (nth 0 args))
        (new-prompt (format "%s:%s"
                            (abbreviate-file-name default-directory)
                            (nth 1 args)))
        (error-level (nth 2 args))
        (condition (nth 3 args)))
    (funcall old-func package new-prompt error-level condition)))

(advice-add 'sly-mrepl--insert-prompt :around #'ambrevar/sly-prepare-prompt)

(map! :map sly-mrepl-mode-map
      :i "<up>" #'sly-mrepl-previous-input-or-button
      :i "<down>" #'sly-mrepl-next-input-or-button)

(add-hook 'doc-view-mode-hook 'auto-revert-mode)

(defun my/eshell-prompt () (concat
                            (propertize (eshell/pwd) 'face '(:foreground "orchid"))
                            (propertize " λ" 'face '(:foreground "green"))
                            " "
                            ))
(setq eshell-prompt-function #'my/eshell-prompt)

(defalias 'eshell/vi 'find-file-other-window)  ;; :^)
(defalias 'eshell/vim 'find-file-other-window)

(map! :map eshell-mode-map :n "G" #'+eshell/goto-end-of-prompt)

(defun my/query-replace-whole-buffer ()
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (call-interactively #'replace-string)
        ))

(map! "M-SPC" 'embark-act)
(map! :map embark-active-map "M-SPC" 'my/query-replace-whole-buffer)
(map! :map embark-variable-map "M-SPC" 'my/query-replace-whole-buffer)
(map! :map embark-symbol-map "M-SPC" 'my/query-replace-whole-buffer)
(map! :map embark-general-map "M-SPC" 'my/query-replace-whole-buffer)

(map! :map evil-window-map "<up>" #'evil-window-up)
(map! :map evil-window-map "<down>" #'evil-window-down)
(map! :map evil-window-map "<left>" #'evil-window-left)
(map! :map evil-window-map "<right>" #'evil-window-right)

(defun my/goto-mark-then-center (&rest args)
    (interactive)
    (let ((goto-mark-function (if (require 'evil-owl nil 'noerror)
                              #'evil-owl-goto-mark-line
                              #'evil-goto-mark-line)))
        (call-interactively goto-mark-function)
        (evil-scroll-line-to-center nil)
    )
)
(map! :map evil-normal-state-map "'" #'my/goto-mark-then-center)

(defun my/is-comment-p ()
  (s-starts-with-p "#" (thing-at-point 'line t)))

(defun my/wrap-fn-for-ob-code-block (fn)
  (interactive)
  (if (and (org-in-src-block-p) (not (my/is-comment-p)))
    (progn
        (call-interactively 'org-edit-special)
        (call-interactively fn)
    )
    (call-interactively fn)))

(defun my/wrap-fn-that-switches-ob-code-blocks (fn)
  (interactive)
  (call-interactively 'org-edit-src-abort)
  (call-interactively fn)
  (call-interactively 'org-edit-special))

(defun my/append-line-or-open-ob-code-window ()
  (interactive)
  (funcall-interactively 'my/wrap-fn-for-ob-code-block 'evil-org-append-line)
)

(defun my/open-above-or-open-ob-code-window ()
  (interactive)
  (funcall-interactively 'my/wrap-fn-for-ob-code-block 'evil-org-open-above)
)

(defun my/open-below-or-open-ob-code-window ()
  (interactive)
  (funcall-interactively 'my/wrap-fn-for-ob-code-block 'evil-org-open-below)
)

(defun my/append-or-open-ob-code-window ()
  (interactive)
  (funcall-interactively 'my/wrap-fn-for-ob-code-block 'evil-append)
)

(defun my/insert-or-open-ob-code-window ()
  (interactive)
  (funcall-interactively 'my/wrap-fn-for-ob-code-block 'evil-insert)
)

(defun my/jump-to-next-ob-code-block ()
  (interactive)
  (my/wrap-fn-that-switches-ob-code-blocks 'org-babel-next-src-block))

(defun my/jump-to-prev-ob-code-block ()
  (interactive)
  (my/wrap-fn-that-switches-ob-code-blocks 'org-babel-previous-src-block))

(when nil (map! :after evil-org
      :map evil-org-mode-map
      :n "a" #'my/append-or-open-ob-code-window
      :n "A" #'my/append-line-or-open-ob-code-window
      :n "o" #'my/open-below-or-open-ob-code-window
      :n "O" #'my/open-above-or-open-ob-code-window
      :n "i" #'my/insert-or-open-ob-code-window
      :n "{{" #'org-babel-previous-src-block
      :n "}}" #'org-babel-next-src-block
))

(evil-define-minor-mode-key 'normal 'org-src-mode "q" 'org-edit-src-abort)
(evil-define-minor-mode-key 'normal 'org-src-mode "}}" 'my/jump-to-next-ob-code-block)
(evil-define-minor-mode-key 'normal 'org-src-mode "{{" 'my/jump-to-prev-ob-code-block)

(setq ob-mermaid-cli-path (expand-file-name "~/node_modules/.bin/mmdc"))

(setq lsp-gopls-staticcheck t)
(setq lsp-eldoc-render-all t)
(setq lsp-gopls-complete-unimported t)

(use-package! lsp-mode
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred))

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; (global-set-key (kbd "<down-mouse-3>") 'strokes-do-stroke)
;; (setq strokes-use-strokes-buffer nil)

(use-package! company
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ipython . t)
))

(use-package! company-lsp
  :commands company-lsp)

(use-package! ob-ipython)

(setq lsp-ui-doc-enable nil
      lsp-ui-peek-enable t
      lsp-ui-sideline-enable t
      lsp-ui-imenu-enable t
      lsp-ui-flycheck-enable t)
;;
(put 'narrow-to-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (company-lsp company lsp-mode)))
 '(safe-local-variable-values
   (quote
    ((pyvenv-activate . "./env")
     (setenv "GOOGLE_APPLICATION_CREDENTIALS" "polar-standard-246307-5ff6b8064ee7.json")
     (pyenv-activate . "./env")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq my/interactive-map-template "* Map for =%s=\n#+begin_src emacs-lisp :results raw\n(defun my/%s-command ()\n\t(interactive)\n\t-- TODO: implement me!\n)\n(map! :map %s my/%s-command)\n#+end_src")
(defun my/interactive-map-open ()
  (interactive)
  (let* ((minor-maps (mapcar #'car minor-mode-map-alist))
         (chosen-map (completing-read "Which map? " (cons (format "%s-map" major-mode) minor-maps))))
    (switch-to-buffer-other-window (make-temp-name (format "*interactive-mapper*" chosen-map)))
    (org-mode)
    (insert (format my/interactive-map-template chosen-map chosen-map chosen-map chosen-map))
    ))

(defun my/delete-frame-if-only-window ()
  (when (eq (length (window-list)) 1)
    (delete-frame)))
(add-hook 'org-capture-after-finalize-hook #'my/delete-frame-if-only-window)
(defun my/make-capture-frame ()
  (interactive)
  (make-frame '((name . "emacs-capture")))
  (select-frame-by-name "emacs-capture")
  (org-capture)
  )
