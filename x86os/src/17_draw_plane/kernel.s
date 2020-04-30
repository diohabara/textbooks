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

  ; 8 bit horizontal line
  mov ah, 0x07                  ; if the color is quite lighter use "0x0F"
  mov al, 0x02
  mov dx, 0x03C4
  out dx, ax

  mov [0x000A_0000 + 0], byte 0xFF

  mov ah, 0x04
  out dx, ax

  mov [0x000A_0000 + 1], byte 0xFF

  mov ah, 0x02
  out dx, ax

  mov [0x000A_0000 + 2], byte 0xFF

  mov ah, 0x01
  out dx, ax

  mov [0x000A_0000 + 3], byte 0xFF

  ; horizontal line across the display
  mov ah, 0x02
  out dx, ax

  lea edi, [0x000A_0000 + 80]
  mov ecx, 80
  mov al, 0xFF
  rep stosb

  ; 8 dots rectangle
  mov edi, 1                    ; number of lines

  shl edi, 8
  lea edi, [edi * 4 + edi + 0xA_0000]

  mov [edi + (80 * 0)], word 0xFF
  mov [edi + (80 * 1)], word 0xFF
  mov [edi + (80 * 2)], word 0xFF
  mov [edi + (80 * 3)], word 0xFF
  mov [edi + (80 * 4)], word 0xFF
  mov [edi + (80 * 5)], word 0xFF
  mov [edi + (80 * 6)], word 0xFF
  mov [edi + (80 * 7)], word 0xFF

  ; a character
  mov esi, 'A'
  shl esi, 4
  add esi, [FONT_ADR]

  mov edi, 2
  shl edi, 8
  lea edi, [edi * 4 + edi + 0xA_0000]

  mov ecx, 16

  .10L:
  movsb
  add edi, 80 - 1
  loop .10L

  ; end process
  jmp $

ALIGN 4, db 0
FONT_ADR: dd 0

;; padding
  times KERNEL_SIZE - ($ - $$) db 0
