int_keyboard:
  pusha
  push ds
  push es

  mov ax, 0x0010
  mov ds, ax
  mov es, ax

  ; red KB buffer
  in al, 0x60

  ; save key code
  cdecl ring_wr, _KEY_BUFF, eax

  ; end of interrupt
  outp 0x20, 0x20

  ; return to register
  pop es
  pop ds
  popa

  iret

ALIGN 4, db 0
_KEY_BUFF: times ring_buff_size db 0
