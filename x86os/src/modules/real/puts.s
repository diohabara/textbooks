puts:
    ; construction of stack frame
    push bp
    mov bp, sp

    ; saving of register
    push ax
    push bx
    push si

    ; get arguments
    mov si, [bp + 4] ; SI = address of string

    ; start of process
    mov ah, 0x0E
    mov bx, 0x0000
    cld
.10L:

    lodsb

    cmp al, 0
    je  .10E

    int 0x10
    jmp .10L
.10E:

    ; returning register
    pop si
    pop bx
    pop ax

    ; destruction of stack frame
    mov sp, bp
    pop bp

    ret
