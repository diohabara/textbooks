;;; padding
;;; times (1024 * 8) - ($ - $$) db 0

  BOOT_LOAD equ 0x7C00
  ORG BOOT_LOAD

;;; macro
%include "../include/macro.s"

;;; entry point
entry:
  ;; BIOS parameter block
  jmp ipl
  times   90 - ($ - $$) db 0x90

;;; initial program loader
ipl:
  cli

  mov ax, 0x0000
  mov ds, ax
  mov es, ax
  mov ss, ax
  mov sp, BOOT_LOAD

  sti
  mov [BOOT.DRIVE], dl

  ;; display string
  cdecl puts, .s0

  ;; read next 512 bytes
  mov ah, 0x02
  mov al, 1
  mov cx, 0x0002
  mov dh, 0x00
  mov dl, [BOOT.DRIVE]
  mov bx, 0x7C00 + 512
  int 0x13
.10Q: jnc .10E
.10T: cdecl puts, .e0
      call  reboot
.10E:
  ;; jump to next stage
  jmp stage_2

  ;; data
.s0  db "Booting...", 0x0a, 0x0D, 0
.e0 db "Error:sector read", 0

ALIGN 2, db 0
BOOT:
.DRIVE: dw 0

;; modules
%include "../modules/real/puts.s"
%include "../modules/real/reboot.s"


;;; boot flag
  times    510 - ($ - $$) db 0x00
  db 0x55, 0xAA

stage_2:
  ;; display string
  cdecl puts, .s0

  ;; end of process
  jmp $

  ;; data
.s0  db "2nd stage...", 0x0A, 0x0D, 0

  ;; padding
  times (1024 * 8) - ($ - $$) db 0
