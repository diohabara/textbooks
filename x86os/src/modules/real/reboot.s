reboot:
        ; display message
        cdecl puts, .s0

        ; waiting for key
.10L:
        mov	ah, 0x10
        int	0x16

        cmp	al, ' '
        jne	.10L

        ; output newline
        cdecl puts, .s1

        ; reboot
        int	0x19

        ; string data
.s0 db 0x0A, 0x0D, "Push SPACE key to reboot...", 0
.s1 db 0x0A, 0x0D, 0x0A, 0x0D, 0
