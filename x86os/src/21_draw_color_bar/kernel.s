%include "../include/define.s"
%include "../include/macro.s"

ORG KERNEL_LOAD

[BITS  32]
;; entry point
kernel:
  ; get font address
  mov esi, BOOT_LOAD + SECT_SIZE ; ESI = 0x7C00
  movzx eax, word [esi + 0]      ; EAX = [ESI + 0] // segment
  movzx ebx, word [esi + 2]      ; EBX = [ESI + 2] // offset
  shl eax, 4                     ; EAX <<= 4
  add eax, ebx                   ; EAX += EBX
  mov [FONT_ADR], eax            ; FONT[0] = EAX

  ; display character
  cdecl draw_char, 0, 0, 0x010F, 'A'
  cdecl draw_char, 1, 0, 0x010F, 'B'
  cdecl draw_char, 2, 0, 0x010F, 'C'

  cdecl draw_char, 0, 0, 0x0402, '0'
  cdecl draw_char, 1, 0, 0x0212, '1'
  cdecl draw_char, 2, 0, 0x0212, '_'


  cdecl draw_font, 63, 13       ; display fonts
  cdecl draw_color_bar, 63, 4   ; display color bar

  cdecl draw_str, 25, 14, 0x010F, .s0  ; display string

  ; end process
  jmp $

.s0: db "  Hello, kernel!", 0

;; modules
%include "../modules/protect/vga.s"
%include "../modules/protect/draw_char.s"
%include "../modules/protect/draw_font.s"
%include "../modules/protect/draw_str.s"
%include "../modules/protect/draw_color_bar.s"

ALIGN 4, db 0
FONT_ADR: dd 0

;; padding
  times KERNEL_SIZE - ($ - $$) db 0
