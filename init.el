;;; .emacs --- Settings for emacs
;;; Commentary:
;;; Code:

(require 'cask "~/.cask/cask.el")
(setq user_bundle (cask-initialize "~/.cask"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (ac-html add-node-modules-path vue-mode scala-mode yaml-mode php-mode flycheck-rust racer magit org-preview-html markdown-mode helm-c-yasnippet helm-ag flycheck-pos-tip auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "PaleTurquoise2"))))
 '(magit-diff-added ((t (:background "black" :foreground "green"))))
 '(magit-diff-added-highlight ((t (:background "white" :foreground "green"))))
 '(magit-diff-removed ((t (:background "black" :foreground "blue"))))
 '(magit-diff-removed-hightlight ((t (:background "white" :foreground "blue"))))
 '(magit-hash ((t (:foreground "red")))))

;; flycheck
(require 'flycheck)
(require 'flycheck-pos-tip)
(add-hook 'after-init-hook #'global-flycheck-mode)
(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

;; gtags
;;(add-to-list 'load-path "/usr/local/share/gtags")
;;(autoload 'gtags-mode "gtags" "" t)

(require 'helm-gtags)
(helm-gtags-mode t)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(setq helm-gtags-auto-update t)
(setq helm-gtags-mode-hook
  '(lambda ()
  (local-unset-key "\C-t")
  ; 文脈から判断してジャンプ
  (local-set-key "\C-t\C-t" 'helm-gtags-dwim)
  ; 定義元へ
  (local-set-key "\C-t\C-d" 'helm-gtags-find-tag)
  ; 参照元へ
  (local-set-key "\C-t\C-r" 'helm-gtags-find-rtag)
  ; 変数の定義元/参照先へ
  (local-set-key "\C-t\C-s" 'helm-gtags-find-symbol)
  ; 前のバッファへ
  (local-set-key "\C-t\C-p" 'helm-gtags-previous-history)
  ; 次のバッファへ
  (local-set-key "\C-t\C-n" 'helm-gtags-next-history)
  ; ファイルへ
  (local-set-key "\C-t\C-f" 'helm-gtags-find-file)
  ))

;; バックアップファイルを作らない
(setq make-backup-files nil)
;; オートセーブファイルを作らない
(setq auto-save-default nil)

;;
;; Auto Complete
;;
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)
(add-to-list 'ac-modes 'text-mode)         ;; text-modeでも自動的に有効にする
(add-to-list 'ac-modes 'fundamental-mode)  ;; fundamental-mode
(add-to-list 'ac-modes 'org-mode)
(add-to-list 'ac-modes 'latex-mode)
(add-to-list 'ac-modes 'yatex-mode)
(add-to-list 'ac-modes 'rust-mode)
(add-to-list 'ac-modes 'mhtml-mode)
(add-to-list 'ac-modes 'go-mode)
(setq ac-use-menu-map t)       ;; 補完メニュー表示時にC-n/C-pで補完候補選択
(setq ac-use-fuzzy t)          ;; 曖昧マッチ
(setq ac-auto-start nil)       ;; 自動的に開始しない
(ac-set-trigger-key "TAB")

;; warp
;;(add-to-list 'load-path "/home/ryota/Downloads/Warp")
;;(require 'warp)
;;(global-set-key (kbd "C-c C-w C-w") warp-mode) ;; Modify key bind as you want.
;; Set markdown converter (if you want)
;;(add-to-list 'warp-format-converter-alist
;;               '("\\.md\\|\\.markdown\\|\\.textile\\|\\.rdoc\\|\\.org\\|\\.creole\\|\\.mediawiki\\|\\.rst\\|\\.asciidoc\\|\\.pod"
;;                 nil
;;                 (lambda ()
;;                   (let* ((string (buffer-string))
;;                          (ext (file-name-extension (buffer-file-name)))
;;                          (temp-file (concat "warp-temp." ext)))
;;                     (with-temp-file temp-file
;;                       (insert string))
;;                     (list "github-markup" temp-file)))))

;; 起動時の画面設定
(setq initial-frame-alist
      (append (list
	       '(width . 120)
	       '(height . 60)
	       '(top . 0)
	       '(left . 0)
	       '(font . "DejaVu Sans Mono-11")
	       )
	      initial-frame-alist))
(setq default-frame-alist initial-frame-alist)

;; 現在行をハイライト
(global-hl-line-mode t)

;;; 対応する括弧を光らせる
(setq show-paren-delay 0)
(show-paren-mode 1)
(setq show-paren-style 'expression)
(set-face-background 'show-paren-match "light goldenrod yellow")

;; 行番号表示
(require 'linum)
(global-linum-mode)
;;; モードラインに桁数を表示
(column-number-mode 1) 

;; Helm
(require 'helm)
(require 'helm-config)
(helm-mode 1)
;; キーバインド
;;(define-key global-map (kbd "C-x b")   'helm-buffers-list)
(define-key global-map (kbd "C-x b") 'helm-for-files)
(define-key global-map (kbd "C-x C-f") 'helm-find-files)
(define-key global-map (kbd "M-x")     'helm-M-x)
(define-key global-map (kbd "M-y")     'helm-show-kill-ring)
;; For find-file etc.
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
;; For helm-find-files etc.
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)

(defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-exist activate)
  "Execute command only if CANDIDATE exists"
  (when (file-exists-p candidate)
    ad-do-it))

;; yasnippet
(require 'yasnippet)
;;(require 'helm-c-yasnippet)
;;(setq helm-yas-space-match-any-greedy t)
;;(global-set-key (kbd "C-c y") 'helm-yas-complete)
(push '("emacs.+/snippets/" . snippet-mode) auto-mode-alist)
(yas-global-mode 1)

;; mark-multiple
(require 'inline-string-rectangle)
(global-set-key (kbd "C-x r t") 'inline-string-rectangle)

(require 'mark-more-like-this)
(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)
;; (global-set-key (kbd "C-M-m") 'mark-more-like-this) ; like the other two, but takes an argument (negative is previous)
(global-set-key (kbd "C-*") 'mark-all-like-this)

;; (add-hook 'sgml-mode-hook
;;           (lambda ()
;;             (require 'rename-sgml-tag)
;;             (define-key sgml-mode-map (kbd "C-c C-r") 'rename-sgml-tag)))
(add-hook 'html-mode-hook
          (lambda ()
            (require 'rename-sgml-tag)
            (define-key html-mode-map (kbd "C-c C-r") 'rename-sgml-tag)))

;; My functions
(defun insert-enumerate (number)
  (interactive "nNumber_of_items:")
  (insert "\\begin{enumerate}\n")
  (setq count 0)
  (while (< count number)
    (insert "\\item \n")
    (setq count (1+ count)))
  (insert "\\end{enumerate}\n"))

(defun insert-itemize (number)
  (interactive "nNumber_of_items:")
  (insert "\\begin{itemize}\n")
  (setq count 0)
  (while (< count number)
    (insert "\\item \n")
    (setq count (1+ count)))
  (insert "\\end{itemize}\n"))

(defun insert-description (number)
  (interactive "nNumber_of_items:")
  (insert "\\begin{description}\n")
  (setq count 0)
  (while (< count number)
    (insert "\\item[] \n")
    (setq count (1+ count)))
  (insert "\\end{description}\n"))

;; Magit
(setq-default magit-auto-revert-mode nil)
(setq vc-handled-backends '())
(eval-after-load "vc" '(remove-hook 'find-file-hooks 'vc-find-file-hook))
(global-set-key (kbd "C-x m") 'magit-status)

;;; Rust mode
;;; racerやrustfmt、コンパイラにパスを通す
;;(add-to-list 'exec-path (expand-file-name "~/.cargo/bin/"))
;;; rust-modeでrust-format-on-saveをtにすると自動でrustfmtが走る
;;(eval-after-load "rust-mode"
;;  '(setq-default rust-format-on-save t))
;;; rustのファイルを編集するときにracerとflycheckを起動する
;;(add-hook 'rust-mode-hook (lambda ()
;;                            (racer-mode)
;;                            (flycheck-rust-setup)))
;;; racerのeldocサポートを使う
;;(add-hook 'racer-mode-hook #'eldoc-mode)
;;; racerの補完サポートを使う
;;(add-hook 'racer-mode-hook (lambda ()
;;                             (company-mode)
                             ;;; この辺の設定はお好みで
;;                             (set (make-variable-buffer-local 'company-idle-delay) 0.1)
;;                             (set (make-variable-buffer-local 'company-minimum-prefix-length) 0)))

;; vue-mode
;;(require 'flycheck)
;;(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
;;(eval-after-load 'vue-mode
;;  '(add-hook 'vue-mode-hook #'add-node-modules-path))
;;(flycheck-add-mode 'javascript-eslint 'vue-mode)
;;(flycheck-add-mode 'javascript-eslint 'vue-html-mode)
;;(flycheck-add-mode 'javascript-eslint 'css-mode)
;;(add-hook 'vue-mode-hook 'flycheck-mode)

;; js-mode
;;;; set indent 2 spaces
(setq js-indent-level 2)

;; go-mode
(add-hook 'before-save-hook 'gofmt-before-save)
(require 'go-eldoc) ;; Don't need to require, if you install by package.el
(add-hook 'go-mode-hook 'go-eldoc-setup)
(add-to-list 'load-path (concat (getenv "GOPATH")  "/pkg/mod/github.com/golang/lint@v0.0.0-20200302205851-738671d3881b/misc/emacs"))
(require 'golint)

;; Key Bind
(global-set-key (kbd "C-x <left>")  'windmove-left)
(global-set-key (kbd "C-x <down>")  'windmove-down)
(global-set-key (kbd "C-x <up>")    'windmove-up)
(global-set-key (kbd "C-x <right>") 'windmove-right)

;;; .emacs ends here
