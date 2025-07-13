bits 64
default rel

section .bss
    read_number resq 1
    stack_memory resq 256  ; Our virtual stack
    stack_ptr resq 1       ; Stack pointer

section .data
    read_format db "%ld", 0
    newline db 10, 0
    prompt db "Enter number: ", 0
    string_literal_0 db "Enter two numbers:", 0
    string_literal_1 db "Numbers are different!", 0
    string_literal_2 db "Numbers are equal!", 0

section .text
    global main
    extern printf
    extern scanf

main:
    push rbp
    mov rbp, rsp
    and rsp, -16  ; Align stack
    sub rsp, 64   ; Reserve space for calls
    ; Initialize our virtual stack
    lea rax, [rel stack_memory]
    mov [rel stack_ptr], rax

    ; Print string
    lea rdi, [rel string_literal_0]
    xor eax, eax
    call printf
    lea rdi, [rel newline]
    xor eax, eax
    call printf
    ; Read number
    lea rdi, [rel prompt]
    xor eax, eax
    call printf
    lea rdi, [rel read_format]
    lea rsi, [rel read_number]
    xor eax, eax
    call scanf
    ; Push the read value
    mov rax, [rel stack_ptr]
    mov rbx, [rel read_number]
    mov [rax], rbx
    add qword [rel stack_ptr], 8
    ; Read number
    lea rdi, [rel prompt]
    xor eax, eax
    call printf
    lea rdi, [rel read_format]
    lea rsi, [rel read_number]
    xor eax, eax
    call scanf
    ; Push the read value
    mov rax, [rel stack_ptr]
    mov rbx, [rel read_number]
    mov [rax], rbx
    add qword [rel stack_ptr], 8
    ; Subtract
    sub qword [rel stack_ptr], 8
    mov rax, [rel stack_ptr]
    mov rbx, [rax]
    sub qword [rel stack_ptr], 8
    mov rax, [rel stack_ptr]
    sub [rax], rbx
    add qword [rel stack_ptr], 8
    ; Jump if equal to zero
    mov rax, [rel stack_ptr]
    sub rax, 8
    cmp qword [rax], 0
    je equal
    ; Print string
    lea rdi, [rel string_literal_1]
    xor eax, eax
    call printf
    lea rdi, [rel newline]
    xor eax, eax
    call printf
    ; Halt program
    jmp program_exit
equal:
    ; Print string
    lea rdi, [rel string_literal_2]
    xor eax, eax
    call printf
    lea rdi, [rel newline]
    xor eax, eax
    call printf
    ; Halt program
    jmp program_exit

program_exit:
    mov rsp, rbp
    pop rbp
    mov eax, 0
    ret
