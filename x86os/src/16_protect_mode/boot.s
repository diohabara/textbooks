;; macro
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

  ; display string
  cdecl puts, .s0

  ; read next 512 bytes
  mov bx, BOOT_SECT - 1
  mov cx, BOOT_LOAD + SECT_SIZE

  cdecl read_chs, BOOT, bx, cx

  cmp ax, bx
  .10Q: jz .10E
  .10T: cdecl puts, .e0
  call  reboot
  .10E:
  ; jump to next stage
  jmp stage_2

;; data
.s0  db "Booting...", 0x0a, 0x0D, 0
.e0  db "Error:sector read", 0

;; info about boot drive
ALIGN 2, db 0
BOOT:
istruc  drive
at  drive.no, dw  0
at  drive.cyln, dw  0
at  drive.head, dw  0
at  drive.sect, dw  2
iend

;;; modules
%include  "../modules/real/puts.s"
%include  "../modules/real/reboot.s"
%include  "../modules/real/read_chs.s"

;; boot flag
  times    510 - ($ - $$) db 0x00
  db 0x55, 0xAA

;; info in real mode
FONT:
.seg: dw 0
.off: dw 0
ACPI_DATA:
.adr: dd 0
.len: dd 0

;;; modules after 512 bytes
%include "../modules/real/itoa.s"
%include "../modules/real/get_drive_param.s"
%include "../modules/real/get_font_adr.s"
%include "../modules/real/get_mem_info.s"
%include "../modules/real/kbc.s"
%include "../modules/real/lba_chs.s"
%include "../modules/real/read_lba.s"

;;;  stage2(boot process)
stage_2:
  ; display string
  cdecl puts, .s0

  ; get drive info
  cdecl get_drive_param, BOOT
  cmp ax, 0
  .10Q: jne .10E
  .10T: cdecl puts, .e0
  call reboot
  .10E:

  ; display drive info
  mov ax, [BOOT + drive.no]
  cdecl itoa, ax, .p1, 2, 16, 0b0100

  mov ax, [BOOT + drive.cyln]
  cdecl itoa, ax, .p2, 4, 16, 0b0100

  mov ax, [BOOT + drive.head]
  cdecl itoa, ax, .p3, 2, 16, 0b0100

  mov ax, [BOOT + drive.sect]
  cdecl itoa, ax, .p4, 2, 16, 0b0100

  cdecl puts, .s1

  ; end of process
  jmp stage_3rd

;; data
.s0 db "2nd stage...", 0x0A, 0x0D, 0
.s1 db "  Drive:0x"
.p1 db "  , C:0x"
.p2 db "    , H:0x"
.p3 db "  , S:0x"
.p4 db "  ", 0x0A, 0x0D, 0
.e0 db "Can't get drive paramter.", 0

;; stage3(boot process)
stage_3rd:
  ; display string
  cdecl puts, .s0

  ; use font in BIOS
  cdecl get_font_adr, FONT

  ; display font address
  cdecl itoa, word [FONT.seg], .p1, 4, 16, 0b0100
  cdecl itoa, word [FONT.off], .p2, 4, 16, 0b0100
  cdecl puts, .s1

  ; get and display memory info
  cdecl get_mem_info

  mov eax, [ACPI_DATA.adr]
  cmp eax, 0
  je .10E

  cdecl itoa, ax, .p4, 4, 16, 0b0100
  shr eax, 16
  cdecl itoa, ax, .p3, 4, 16, 0b0100

  cdecl puts, .s2
  .10E:

  ; end of process
  jmp stage_4

  ;data
.s0 db "3rd stage...", 0x0A, 0x0D, 0
.s1: db "Font Address="
.p1: db "ZZZZ:"
.p2: db "ZZZZ", 0x0A, 0x0D, 0
  db	0x0A, 0x0D, 0

.s2: db " ACPI data="
.p3: db "ZZZZ:"
.p4: db "ZZZZ", 0x0A, 0x0D, 0

;; stage4(boot process)
stage_4:
  ; display string
  cdecl puts, .s0

  ; test for keyboard LED
  cdecl puts, .s2

  mov bx, 0
  .10L:
  mov ah, 0x00
  int 0x16

  cmp al, '1'
  jb .10E

  cmp al, '3'
  ja .10E

  mov cl, al
  dec cl
  and cl, 0x03
  mov ax, 0x0001
  shl ax, cl
  xor bx, ax

  ; sending LED command
  cli

  cdecl KBC_Cmd_Write, 0xAD

  cdecl KBC_Data_Write, 0xEED
  cdecl KBC_Data_Read, .key

  cmp [.key], byte 0xFA
  jne .11F

  cdecl KBC_Data_Write, bx
  jmp .11F
  .11F:
  cdecl itoa, word [.key], .e1, 2, 16, 0b0100
  cdecl puts, .e0
  .11E:

  cdecl KBC_Cmd_Write, 0xAE

  sti

  jmp	.10L
  .10E:

  ; display string
  cdecl puts, .s3

  ; ending process
  jmp stage_5

.s0: db "4th stage...", 0x0A, 0x0D, 0
.s1: db " A20 Gate Enabled.", 0x0A, 0x0D, 0
.s2: db " Keyboard LED Test...", 0
.s3: db " (done)", 0x0A, 0x0D, 0
.e0: db "["
.e1: db "ZZ]", 0

.key: dw 0

;; stage5(boot process)
stage_5:
  ; display string
  cdecl puts, .s0

  ; read kernel
  cdecl read_lba, BOOT, BOOT_SECT, KERNEL_SECT, BOOT_END

  cmp ax, KERNEL_SECT
  .10Q: jz .10E
  .10T: cdecl puts, .e0
  call reboot
  .10E:
  ; end process
  jmp stage_6

;; data
.s0 db "5th stage...", 0x0A, 0x0D, 0
.e0 db " Failure load kernel...", 0x0A, 0x0D, 0

;; stage6(boot process)
stage_6:
  ; display string
  cdecl puts, .s0

  ; wait for user input
  .10L:
  mov ah, 0x00
  int 0x16
  cmp al, ' '
  jne .10L

  ; video mode
  mov ax, 0x0012
  int 0x10

  ; end process
  jmp $

.s0 db "6th stage...", 0x0A, 0x0D, 0x0A, 0x0D
  db "[Push SPACE key to protect mode...]", 0x0A, 0x0D, 0

;; padding
  times (BOOT_SIZE) - ($ - $$) db 0
