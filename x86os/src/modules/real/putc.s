putc:
    ; construction of stack
    push bp
    mov bp, sp

    ; saving register
    push ax
    push bx

    ; start of process
    mov al, [bp + 4]
    mov ah, 0x0E
    mov bx, 0x0000
    int 0x10

    ; return to register
    pop bx
    pop ax

    ; destruction of stack frame
    mov sp, bp
    pop bp

    ret