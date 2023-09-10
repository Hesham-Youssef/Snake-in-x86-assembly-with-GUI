DATA SECTION
    number_buffer DB 20 DUP 0
    number_len DB 0

CODE SECTION
    START:
        SCORETOSTRING:  
        mov ax, 35
        mov bx, 10
        mov ecx, 0
        notzero:
            xor dx, dx
            div bx
            add dx, 48
            mov [number_buffer+ecx], dx
            inc ecx
            test ax, ax
            jnz notzero
        mov b[number_buffer+ecx], 0
        mov [number_len], cl
        ret
