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

  ; display string
  cdecl draw_str, 25, 14, 0x010F, .s0

  ; display time(cannot use with interrupt)
  ; .10L:

  ; cdecl rtc_get_time, RTC_TIME
  ; cdecl draw_time, 72, 0, 0x0700, dword [RTC_TIME]

  ; jmp .10L

  ; draw line
  cdecl draw_line, 100, 100, 0, 0, 0x0F
  cdecl draw_line, 100, 100, 200, 0, 0x0F
  cdecl draw_line, 100, 100, 200, 200, 0x0F
  cdecl draw_line, 100, 100, 0, 200, 0x0F

  cdecl draw_line, 100, 100, 50, 0, 0x02
  cdecl draw_line, 100, 100, 150, 0, 0x03
  cdecl draw_line, 100, 100, 150, 200, 0x04
  cdecl draw_line, 100, 100, 50, 200, 0x05

  cdecl draw_line, 100, 100, 0, 50, 0x02
  cdecl draw_line, 100, 100, 200, 50, 0x03
  cdecl draw_line, 100, 100, 200, 150, 0x04
  cdecl draw_line, 100, 100, 0, 150, 0x05

  cdecl draw_line, 100, 100, 100, 0, 0x0F
  cdecl draw_line, 100, 100, 200, 100, 0x0F
  cdecl draw_line, 100, 100, 100, 200, 0x0F
  cdecl draw_line, 100, 100, 0, 100, 0x0F


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

  ; draw rectangle
  cdecl draw_rect, 100, 100, 200, 200, 0x03
  cdecl draw_rect, 400, 250, 150, 150, 0x05
  cdecl draw_rect, 350, 400, 300, 100, 0x06

  ; interrupt
  push 0x11223344
  pushf
  call 0x0008:int_default

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
%include "../modules/protect/draw_line.s"
%include "../modules/protect/draw_rect.s"
%include "../modules/protect/itoa.s"
%include "../modules/protect/rtc.s"
%include "../modules/protect/draw_time.s"
%include "./modules/interrupt.s"

ALIGN 4, db 0
FONT_ADR: dd 0
RTC_TIME: dd 0

;; padding
  times KERNEL_SIZE - ($ - $$) db 0
