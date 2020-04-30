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

  cdecl draw_font, 63, 13       ; display fonts
  cdecl draw_color_bar, 63, 4   ; display color bar

  cdecl draw_str, 25, 14, 0x010F, .s0  ; display string

  ; draw x
  cdecl draw_pixel, 8, 4, 0x01
  cdecl draw_pixel, 9, 5, 0x01
  cdecl draw_pixel, 10, 6, 0x02
  cdecl draw_pixel, 11, 7, 0x02
  cdecl draw_pixel, 12, 8, 0x03
  cdecl draw_pixel, 13, 9, 0x03
  cdecl draw_pixel, 14, 10, 0x04
  cdecl draw_pixel, 15, 11, 0x04

  cdecl draw_pixel, 15, 4, 0x03
  cdecl draw_pixel, 14, 5, 0x03
  cdecl draw_pixel, 13, 6, 0x04
  cdecl draw_pixel, 12, 7, 0x04
  cdecl draw_pixel, 11, 8, 0x01
  cdecl draw_pixel, 10, 9, 0x01
  cdecl draw_pixel, 9, 10, 0x02
  cdecl draw_pixel, 8, 11, 0x02

  ; end process
  jmp $

.s0: db "  Hello, kernel!", 0

;; modules
%include "../modules/protect/vga.s"
%include "../modules/protect/draw_char.s"
%include "../modules/protect/draw_font.s"
%include "../modules/protect/draw_str.s"
%include "../modules/protect/draw_color_bar.s"
%include "../modules/protect/draw_pixel.s"

ALIGN 4, db 0
FONT_ADR: dd 0

;; padding
  times KERNEL_SIZE - ($ - $$) db 0
