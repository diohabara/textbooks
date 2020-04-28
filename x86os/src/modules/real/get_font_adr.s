get_font_adr:
  ; construction of stack frame
  push bp
  mov bp, sp

  ; saving register
  push ax
  push bx
  push si
  push es
  push bp

  ; get arguments
  mov si, [bp + 4]

  ; get font address
  mov ax, 0x1130
  mov bh, 0x06
  int 10h

  ; save font address
  mov [si + 0], es
  mov [si + 2], bp

  ; returning to register
  pop bp
  pop es
  pop si
  pop bx
  pop ax

  ; destruction of stack frame
  mov sp, bp
  pop bp

  ret
