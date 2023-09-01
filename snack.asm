
CELL STRUCT ; 64 byte total
    coord   DD      5
    next    DD      0
CELL ENDS


DATA SECTION
    ; HEAP_ZERO_MEMORY equ 0x00000008
    head    CELL    <500, 0> ; [y | x]
    tail    CELL    
    cNode   CELL     

    valx    DW      5
    valy    DW      8

    foodCoord   DD  0

    hHeap   DD      ?
    dwBytes equ      sizeof CELL
    dwFlags equ     0x00000008 ; HEAP_ZERO_MEMORY (sets newly allocated mem to zero)

CODE SECTION

    FOODEATEN: ;Reallocate extra cell and randomize the foods position

    RET


    CONTROLLER:
        MOV eax, [head.coord]

        ADD ax, [valx]
        ROL eax, 16
        ADD ax, [valy] ; [x | y]
        ROL eax, 16

        mov ebx, [foodCoord] ; [y | x]

        CMP eax, ebx
        JNE >NEXT
        CALL FOODEATEN
    NEXT:
        mov ebx, ADDR head ;advancing the snake
        L1:
            mov eax, [ebx]
            mov ebx, [ebx + 4]
            test ebx, ebx
            mov [ebx], eax
            jnz L1
        RET



    DRAWER:


    ADDNEWNODE:
        invoke HeapAlloc, [hHeap], dwFlags, 10
        mov ebx, [tail]
        mov [tail], eax
        mov [ebx + 4], eax
        RET

    INITSNAKE:
        call GetProcessHeap
        mov [hHeap], eax

        mov [tail], ADDR head
        mov ecx, 0x00960096
        L1:
            push ecx
            CALL ADDNEWNODE
            pop ecx
            mov [ebx], ecx
            dec ecx
            cmp ecx, 0x00960091
            jne L1
        RET


    START:
        call INITSNAKE
        
    
        RET
