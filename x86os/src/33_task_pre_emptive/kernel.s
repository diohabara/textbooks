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

  ; setting for TSS descriptor
  set_desc GDT.tss_0, TSS_0
  set_desc GDT.tss_1, TSS_1

  ; setting for LDT
  set_desc GDT.ldt, LDT, word LDT_LIMIT

  ; load GDT
  lgdt [GDTR]

  ; setting for stack
  mov esp, SP_TASK_0

  ; initialize task register
  mov ax, SS_TASK_0
  ltr ax

  ; initialize interrupt vector
  cdecl init_int
  cdecl init_pic

  set_vect 0x00, int_zero_div
  set_vect 0x20, int_timer
  set_vect 0x21, int_keyboard
  set_vect 0x28, int_rtc

  ; enable devide interrupt
  cdecl rtc_int_en, 0x10

  ; enable IMR
  outp 0x21, 0b1111_1000        ; slave PIC/KBC/timer
  outp 0xA1, 0b1111_1110        ; RTC

  ; allow CPU to interrupt
  sti

  ; display fonts
  cdecl draw_font, 63, 13       ; display fonts
  cdecl draw_color_bar, 63, 4   ; display color bar

  ; display string
  cdecl draw_str, 25, 14, 0x010F, .s0

  .10L:

  ; call task
  jmp SS_TASK_1:0

  ; rotating bar
  cdecl draw_rotation_bar

  ; get key code
  cdecl ring_rd, _KEY_BUFF, .int_key
  cmp eax, 0
  je .10E

  ; display key code
  cdecl draw_key, 2, 29, _KEY_BUFF

  .10E:
  jmp .10L


.s0: db "  Hello, kernel!", 0
.int_key: dd 0

ALIGN 4, db 0
FONT_ADR: dd 0
RTC_TIME: dd 0

;; task
%include "descriptor.s"
%include "modules/int_timer.s"
%include "tasks/task_1.s"

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
%include "../modules/protect/int_keyboard.s"
%include "../modules/protect/ring_buff.s"
%include "../modules/protect/draw_rotation_bar.s"

;; padding
  times KERNEL_SIZE - ($ - $$) db 0
