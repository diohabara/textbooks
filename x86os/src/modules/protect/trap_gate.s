trap_gate_81:
  ; display a character
  cdecl draw_char, ecx, edx, ebx, eax

  iret

trap_gate_82:
  ; display a dot
  cdecl draw_pixel, ecx, edx, ebx

  iret
