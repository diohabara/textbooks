int_timer:
  push ebp
  mov ebp, esp

  str ax
  cmp ax, SS_TASK_0
  je .11L
  cmp ax, SS_TASK_1
  je .12L

  jmp SS_TASK_0:0
  jmp .10E

  .11L:
  jmp SS_TASK_1:0
  jmp .10E

  .12L:
  jmp SS_
