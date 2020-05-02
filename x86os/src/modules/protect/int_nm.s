get_tss_base:
  mov eax, [GDT + ebx + 2]
  shl eax, 8
  mov al, [GDT + ebx + 7]
  ror eax, 8

  ret

save_fpu_context:
  fnsave [eax + 104]
  mov [eax + 104 + 108], dword 1

  ret

load_fpu_context:
  cmp [eax + 104 + 108], dword 0
  jne .10F
  fninit
  jmp .10E
  .10F:
  frstor [eax + 104]
  .10E:
  ret

int_nm:
  ; save register
  pusha
  push ds
  push es

  ; kernel selector
  mov ax, DS_KERNEL
  mov ds, ax
  mov es, ax

  ; clear task switch flag
  clts

  ; task for prev/this time
  mov edi, [.last_tss]
  str esi
  and esi, ~0x0007

  ; check if it is the first time
  cmp edi, 0
  je .10F

  cmp esi, edi
  je .12E

  cli

  ; prev
  mov ebx, edi
  call get_tss_base
  call save_fpu_context

  ; this time
  mov ebx, esi
  call get_tss_base
  call load_fpu_context

  sti

  .12E:
  jmp .10E
  .10F:

  cli

  ; thsi time
  mov ebx, esi
  call get_tss_base
  call load_fpu_context

  sti

  .10E:
  mov [.last_tss], esi

  pop es
  pop ds
  popa

  iret

ALIGN 4, db 0
.last_tss: dd 0
