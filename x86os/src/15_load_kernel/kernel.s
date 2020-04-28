%include "../include/define.s"
%include "../include/macro.s"

ORG KERNEL_LOAD

[BITS  32]
;; entry point
kernel:
  ; ending process
  jmp $

;; padding
  times KERNEL_SIZE - ($ - $$) db 0
