lba_chs:
  ; construction of stack frame
  push bp
  mov bp, sp

  ; saving register
  push ax
  push bx
  push dx
  push si
  push di


  mov si, [bp + 4]
  mov di, [bp + 6]

  mov al, [si + drive.head]
  mul byte [si + drive.sect]
  mov bx, ax

  mov dx, 0
  mov ax, [bp + 8]
  div bx

  mov [di + drive.cyln], ax

  mov ax, dx
  div byte [si + drive.sect]

  movzx dx, ah
  inc dx

  mov ah, 0x00

  mov [di + drive.head], ax
  mov [di + drive.sect], dx

  ; returing register
  pop di
  pop si
  pop dx
  pop bx
  pop ax

  ; destruction of stack frame
  mov sp, bp
  pop bp

  ret
