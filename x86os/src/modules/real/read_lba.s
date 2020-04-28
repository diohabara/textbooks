read_lba:
  ; construction of stack frame
  push bp
  mov bp, sp

  ; saving register
  push si

  ; preprocess
  mov si, [bp + 4]

  ; LBA -> CHS
  mov ax, [bp + 6]
  cdecl lba_chs, si, .chs, ax

  ; copying drive number
  mov al, [si + drive.no]
  mov [.chs + drive.no], al

  ; reading sector
  cdecl read_chs, .chs, word[bp + 8], word[bp + 10]

  ; returing register
  pop si

  ; destruction of stack frame
  mov sp, bp
  pop bp

  ret

ALIGN 2
.chs:   times drive_size db 0
