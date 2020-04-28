%include "../include/define.s"
%include "../include/macro.s"

  ORG   BOOT_LOAD

entry:
  jmp ipl
  times   90 - ($ - $$) db 0x90

ipl:
  cli

  mov ax, 0x0000
  mov ds, ax
  mov es, ax
  mov ss, ax
  mov sp, BOOT_LOAD

  sti

  mov [BOOT + drive.no], dl

  ;; display string
  cdecl puts, .s0

  ;; read next 512 bytes
  mov bx, BOOT_SECT - 1
  mov cx, BOOT_LOAD + SECT_SIZE

  cdecl read_chs, BOOT, bx, cx

  cmp ax, bx
.10Q: jz .10E
.10T: cdecl puts, .e0
      call  reboot
.10E:
  ;; jump to next stage
  jmp stage_2

  ;; data
.s0  db "Booting...", 0x0a, 0x0D, 0
.e0  db "Error:sector read", 0

;;; info about boot drive
ALIGN 2, db 0
BOOT:
  istruc  drive
  at  drive.no, dw  0
  at  drive.cyln, dw  0
  at  drive.head, dw  0
  at  drive.sect, dw  0
  iend

;;; modules
%include  "../modules/real/puts.s"
%include  "../modules/real/reboot.s"
%include  "../modules/real/read_chs.s"

;;; boot flag
  times    510 - ($ - $$) db 0x00
  db 0x55, 0xAA

;;;  stage2(boot process)
stage_2:
  ;; display string
  cdecl puts, .s0

  ;; end of process
  jmp $

  ;; data
  .s0  db "2nd stage...", 0x0A, 0x0D, 0

  ;;; padding
  times (BOOT_SIZE) - ($ - $$) db 0
