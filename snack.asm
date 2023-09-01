
CELL STRUCT ; 64 byte total
    coord   DD      0
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

        RET



    DRAWER:

    ADDNEWNODE:
        mov eax, [hHeap]
        push [hHeap], dwFlags, dwBytes
        call HeapAlloc
        mov [tail.next], eax
        mov [tail.coord], ecx
        mov [tail], eax

        RET

    INITSNAKE:
        INVOKE GetProcessHeap
        mov [hHeap], eax

        mov [tail], ADDR head
        mov ecx, 5
        L1:
            CALL ADDNEWNODE
        RET

    START:
        call INITSNAKE
        
        mov ecx, 5
        mov [cNode], ADDR head
        L1:
            mov eax, [cNode.coord]
            mov ebx, [cNode.next]
            mov [cNode], ebx
            loop L1