;; Aaron Jackson <asj@cs.nott.ac.uk>
;; caffe-mode.el

(setq caffe-mode-highlights
      '((".*\s{" . font-lock-function-name-face)
        ("^\s*.+:" . font-lock-keyword-face)
        ("#+.*" . font-lock-comment-face)
        ("'.*'" . font-lock-string-face)
        ("[A-z]" . font-lock-constant-face)
        ("false\\|true" . font-lock-constant-face)
        ("[0-9.e]+" . font-lock-constant-face)
        ))

(define-derived-mode caffe-mode fundamental-mode
  (setq font-lock-defaults '(caffe-mode-highlights))
  (setq mode-name "Caffe"))

(provide 'caffe-mode)
