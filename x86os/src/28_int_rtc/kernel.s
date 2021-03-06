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

  ; initialize interrupt vector
  cdecl init_int
  cdecl init_pic

  set_vect 0x00, int_zero_div
  set_vect 0x28, int_rtc

  ; enable devide interrupt
  cdecl rtc_int_en, 0x10

  ; enable IMR
  outp 0x21, 0b1111_1011
  outp 0xA1, 0b1111_1110

  ; allow CPU to interrupt
  sti

  ; display fonts
  cdecl draw_font, 63, 13       ; display fonts
  cdecl draw_color_bar, 63, 4   ; display color bar

  ; display string
  cdecl draw_str, 25, 14, 0x010F, .s0

  ; display time(cannot use with interrupt)
 .10L:

  mov eax, [RTC_TIME]
  cdecl draw_time, 72, 0, 0x0700, eax

  jmp .10L

  ; end process
  jmp $

.s0: db "  Hello, kernel!", 0

ALIGN 4, db 0
FONT_ADR: dd 0
RTC_TIME: dd 0

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
%include "../modules/protect/interrupt.s"
%include "../modules/protect/pic.s"
%include "../modules/protect/int_rtc.s"

;; padding
  times KERNEL_SIZE - ($ - $$) db 0
