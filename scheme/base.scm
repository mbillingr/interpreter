(define-library (scheme base)
  ; TODO: re-export stuff from core
  ; TODO: R7RS compliant base library
  (export + - * /)
  (import (builtin core)))
