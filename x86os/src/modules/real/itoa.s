itoa:
        ; construction of stack frame
        push bp
        mov  bp, sp
        
        ; saving register
        push     ax
        push     bx
        push     cx
        push     dx
        push     si
        push     di
        
        ; get arguments
        mov   ax, [bp + 4]
        mov   si, [bp + 6]
        mov   cx, [bp + 8]

        mov   di, si
        add   di, cx
        dec   di

        mov   bx, word [bp + 12]

        ; signed decision
        test     bx, 0b0001
.10Q:   je       .10E
        cmp      ax, 0
.12Q:   jge      .12E
        or       bx,    0b0010
.12E:
.10E:

        ; signed output decision
        test bx, 0b0010
.20Q:   je   .20E
        cmp  ax, 0
.22Q:   jge  .22F
        neg  ax
        mov  [si], byte '-'
        jmp  .22E
.22F:
        mov [si], byte '+'
.22E:
        dec     cx
.20E:
        
        ; transformation into ASCII
        mov     bx, [bp + 10]
.30L:
        mov     dx, 0
        div     bx

        mov     si, dx
        mov     dl, byte [.ascii + si]

        mov     [di], dl
        dec     di

        cmp     ax, 0
        loopnz  .30L
.30E:
        
        ; fill in blank
        cmp    cx, 0
.400:   je     .40E
        mov    al, ' '
        cmp    [bp + 12], word 0b0100
.42Q:   jne    .42E
        mov    al, '0'
.42E:
        std
        rep     stosb
.40E:
        ; returning to register
        pop     di
        pop     si
        pop     dx
        pop     cx
        pop	bx
        pop	ax

        ; destruction of stack frame
        mov     sp, bp
        pop     bp

        ret

.ascii  db      "0123456789ABCDEF"
        
