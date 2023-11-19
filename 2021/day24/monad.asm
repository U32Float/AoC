mov x3, 0
mov x4, 26
mov x6, {1}
ldr x0, [x6, #0]
mul x3, x3, x4
mov x2, x0
add x2, x2, #14
add x3, x3, x2
ldr x0, [x6, #8]
mul x3, x3, x4
mov x2, x0
add x2, x2, #8
add x3, x3, x2
ldr x0, [x6, #16]
mul x3, x3, x4
mov x2, x0
add x2, x2, #4
add x3, x3, x2
ldr x0, [x6, #24]
mul x3, x3, x4
mov x2, x0
add x2, x2, #10
add x3, x3, x2
ldr x0, [x6, #32]
mov x1, x3
udiv x10, x1, x4
msub x1, x10, x4, x1
udiv x3, x3, x4
sub x1, x1, #3
cmp x1, x0
beq 1f
mul x3, x3, x4
mov x2, x0
add x2, x2, #14
add x3, x3, x2
b 1f
1:
ldr x0, [x6, #40]
mov x1, x3
udiv x10, x1, x4
msub x1, x10, x4, x1
udiv x3, x3, x4
sub x1, x1, #4
cmp x1, x0
beq 1f
mul x3, x3, x4
mov x2, x0
add x2, x2, #10
add x3, x3, x2
b 1f
1:
ldr x0, [x6, #48]
mul x3, x3, x4
mov x2, x0
add x2, x2, #4
add x3, x3, x2
ldr x0, [x6, #56]
mov x1, x3
udiv x10, x1, x4
msub x1, x10, x4, x1
udiv x3, x3, x4
sub x1, x1, #8
cmp x1, x0
beq 1f
mul x3, x3, x4
mov x2, x0
add x2, x2, #14
add x3, x3, x2
b 1f
1:
ldr x0, [x6, #64]
mov x1, x3
udiv x10, x1, x4
msub x1, x10, x4, x1
udiv x3, x3, x4
sub x1, x1, #3
cmp x1, x0
beq 1f
mul x3, x3, x4
mov x2, x0
add x2, x2, #1
add x3, x3, x2
b 1f
1:
ldr x0, [x6, #72]
mov x1, x3
udiv x10, x1, x4
msub x1, x10, x4, x1
udiv x3, x3, x4
sub x1, x1, #12
cmp x1, x0
beq 1f
mul x3, x3, x4
mov x2, x0
add x2, x2, #6
add x3, x3, x2
b 1f
1:
ldr x0, [x6, #80]
mul x3, x3, x4
mov x2, x0
add x2, x2, #0
add x3, x3, x2
ldr x0, [x6, #88]
mov x1, x3
udiv x10, x1, x4
msub x1, x10, x4, x1
udiv x3, x3, x4
sub x1, x1, #6
cmp x1, x0
beq 1f
mul x3, x3, x4
mov x2, x0
add x2, x2, #9
add x3, x3, x2
b 1f
1:
ldr x0, [x6, #96]
mul x3, x3, x4
mov x2, x0
add x2, x2, #13
add x3, x3, x2
ldr x0, [x6, #104]
mov x1, x3
udiv x10, x1, x4
msub x1, x10, x4, x1
udiv x3, x3, x4
sub x1, x1, #12
cmp x1, x0
beq 1f
mul x3, x3, x4
mov x2, x0
add x2, x2, #12
add x3, x3, x2
b 1f
1:
b 2f
2:
mov {0}, x3