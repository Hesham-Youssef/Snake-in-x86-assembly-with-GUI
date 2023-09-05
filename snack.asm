
CELL STRUCT ; 8 byte total
    coord   DD      0
    next    DD      0
    prev    DD      0
CELL ENDS


DATA SECTION
    ; HEAP_ZERO_MEMORY equ 0x00000008
    ISALIVE DB      1   

    SEED    DD      ?
    MULTI   equ     1664525
    INCRE   equ     1013904223
    MODU    equ     4294967295

    GRIDSIZE equ    100

    HEIGHT  equ     730
    WIDTH   equ     708
    SCALE   equ     7      ;assuming we the original grid is 100x100
    SIDELEN equ     7      

    hASB    DD      ?
    hwnd    DD      0

    RCKEEP DD 0

    head    DD      ?
    tail    DD      ?

    valx    DW      -1
    valy    DW      0

    foodCoord   DD  ?

    hHeap   DD      ?
    dwBytes equ     sizeof CELL + 1
    dwFlags equ     0x00000008 ; HEAP_ZERO_MEMORY (sets newly allocated mem to zero)

    hInst   DD      0

    WINDOW_CLASSNAME DB 'hello',0

    hDC   DD 0            ;to keep the handle of the device context
    PAINTSTRUCT DD 16 DUP 0  ;structure to hold stuff from Windows on WM_PAINT
    MSG DD 7 DUP 0

    MESSAGES DD (ENDOF_MESSAGES-$-4)/8      ;=number to be done
            DD  0x0113,TIMER,1h,CREATE,2h,DESTROY,0Fh,PAINT,0x0100,KEYDOWN
    ENDOF_MESSAGES: 

    KEYS    DD  LEFT_KEY
            DD  UP_KEY
            DD  RIGHT_KEY
            DD  DOWN_KEY


    WNDCLASS DD 10D DUP 0 ;structure to send to RegisterClass holding data:-
                    ;+0 window class style (CS_)
                    ;+4 pointer to Window Procedure
                    ;+8 no. of extra bytes to allocate after structure
                    ;+C no. of extra bytes to allocate after window instance
                    ;+10 handle to instance of this window class
                    ;+14 handle to the class icon
                    ;+18 handle to the class cursor
                    ;+1C identifies the class background brush
                    ;+20 pointer to resource name for class menu
                    ;+24 pointer to string for window class name

    RECT    DD  4   DUP 0   ;+0 upper left x
                            ;+4 upper left y
                            ;+8 lower right x
                            ;+C lower right y

    REDBRUSH    DD      ?
CODE SECTION

    LEFT_KEY:
        mov ax, [valx]
        test ax, ax
        jnz >continue
        mov w[valx], -1
        mov w[valy], 0
        ret
    
    UP_KEY:
        mov ax, [valy]
        test ax, ax
        jnz >continue
        mov w[valy], -1
        mov w[valx], 0
        ret 

    RIGHT_KEY:
        mov ax, [valx]
        test ax, ax
        jnz >continue
        mov w[valx], 1
        mov w[valy], 0
        ret 

    DOWN_KEY:
        mov ax, [valy]
        test ax, ax
        jnz >continue
        mov w[valy], 1
        mov w[valx], 0
        ret

    RAND:
        mov eax, [SEED]
        IMUL eax, MULTI
        ADD  eax, INCRE
        xor edx, edx
        mov ebx, MODU
        div  ebx
        mov [SEED], edx

        mov eax, edx
        xor edx, edx

        mov bx, GRIDSIZE
        div bx
        mov bx, dx

        shr eax, 16
        shl ebx, 16

        mov bx, GRIDSIZE
        div bx
        mov bx, dx

        mov eax, ebx

        ret

    FOODEATEN: ;Reallocate extra cell and randomize the foods position
        ; call ADDNEWNODE
        PUSH 0,ADDR RCKEEP      ;RCKEEP receives output from API
        PUSH 20D,'from inside foodeaten'    ;24=length of string
        PUSH [hASB]                ;handle to active screen buffer
        CALL WriteFile

        mov ecx, 3
        ADDCELLS:
            push ecx
            call ADDNEWNODE
            mov ebx, [ebx]
            mov [eax], ebx
            pop ecx
            loop ADDCELLS
            
        call RAND
        mov [foodCoord], eax

        RET



    CONTROLLER:
        mov al, [ISALIVE]
        test al, al
        jz >goon

        mov esi, [head]
        

        mov eax, d[esi]

        ADD ax, [valx]
        js >DEAD
        cmp ax, 100
        jge >DEAD

        ROL eax, 16
        ADD ax, [valy] ; [x | y]
        js >DEAD
        cmp ax, 100
        jge >DEAD
        ROL eax, 16

        mov ebx, esi
        snakestart:
            test ebx, ebx
            jz >snakedone
            
            cmp eax, [ebx]
            jne >nooverlab
            jmp >DEAD
            nooverlab:
                mov ebx, [ebx + 4]
                jmp snakestart
        snakedone: 
        

        mov edi, [tail]
        mov ebx, d[edi+8]

        mov d[ebx+4], 0
        
        mov d[esi + 8], edi

        mov d[edi+4], esi
        mov d[edi+8], 0
        
        mov [head], edi
        mov [tail], ebx

        mov d[edi], eax
        

        mov ebx, [foodCoord] ; [y | x]

        CMP eax, ebx
        JNE >NOTHINGEATEN
            CALL FOODEATEN
        NOTHINGEATEN:

        JMP >goon

        DEAD:
            mov B[ISALIVE], 0
        goon:
        RET



    


    ADDNEWNODE:
        invoke HeapAlloc, [hHeap], dwFlags, 9 ;8 for the node and one extra because otherwise an error will occur
        mov ebx, [tail]
        mov [ebx+4], eax
        mov [tail], eax
        mov [eax + 8], ebx
        RET

    INITSNAKE:
        rdseed eax
        mov [SEED], eax

        push 0x000000FF ;red
        call CreateSolidBrush
        mov [REDBRUSH], eax

        call RAND
        mov [foodCoord], eax

        call GetProcessHeap
        mov [hHeap], eax
        
        invoke HeapAlloc, [hHeap], dwFlags, 9 ; wrong size (i don't even know how does this still works)
        mov [head], eax
    

        mov [tail], eax
        mov [eax], 0x00320031
        mov ecx, 0x00320032
        L1:
            push ecx
            CALL ADDNEWNODE
            pop ecx
            mov [eax], ecx
            inc ecx
            cmp ecx, 0x00320038
            jne L1
        RET

    CREATE:                 ;one of the few messages dealt with by this prog
        XOR EAX,EAX             ;return zero to make window
        RET
        ;
    DESTROY:                ;one of the few messages dealt with by this prog
        PUSH 0
        CALL PostQuitMessage    ;exit via the message loop
        STC                     ;go to DefWindowProc too
        RET

    DRAWWITHCOORD: ; put coord into edx before calling
        imul dx, SCALE
        mov [eax], dx
        ADD dx, SIDELEN
        mov [eax+8], dx

        shr edx, 16
        imul dx, SCALE
        mov [eax+4], dx
        ADD dx, SIDELEN
        mov [eax+12], dx

        push ecx
        push ADDR RECT
        push [hDC]

        call FillRect

        ret


    PAINT:
        MOV EBX,ADDR PAINTSTRUCT
        PUSH EBX,[hwnd]           ;EBP+8h=hwnd
        CALL BeginPaint             ;get device context to use, initialise paint
        MOV [hDC],EAX
        
        push 0x00000000
        push eax
        call SetBkColor
    ;     mov eax, [hwnd]
    ;     test eax, eax
    ;     jz >first
    ;     PUSH 0, 0    ;rectangle bottom, right
    ;     PUSH 500,500     ;rectangle top, left
    ;     PUSH [hDC]
    ;     CALL Ellipse
    ; first:


        mov ebx, [head]              ;could be improved by utilizing the fact that most 
                                        ;of the information is already in the stack 
                                        ;and just shifting the stack
                                        ;pointer to reinclude the data
    
        mov eax, ADDR RECT
        mov edx, [ebx]
        mov ecx, 27
        call DRAWWITHCOORD 
        mov ebx, [ebx + 4]

        DRAWSNAKE:
            test ebx, ebx
            jz >snakeend
            
            mov eax, ADDR RECT
            mov edx, [ebx]
            mov ecx, 18
            call DRAWWITHCOORD 

            mov ebx, [ebx + 4]
            jmp DRAWSNAKE
        snakeend:

        mov eax, ADDR RECT
        mov edx, [foodCoord]
        mov ecx, [REDBRUSH]         ;note: don't forget (color + 1)
        call DRAWWITHCOORD

        PUSH ADDR PAINTSTRUCT, [hwnd]           ;EBP+8h=hwnd
        CALL EndPaint
        XOR EAX,EAX
        RET

    TIMER:

        call CONTROLLER

        push 1
        push 0
        push [hwnd]
        call InvalidateRect

        push [hwnd]
        CALL UpdateWindow
        RET 10h





    KEYDOWN:
        mov eax, [ebp+10h]  ;note: key is in the wparam
        sub eax, 0x25
        js >continue
        cmp eax, 3
        jg >continue
        call [KEYS + eax*4]

    continue:
        xor eax, eax
        ret

    GENERAL_WNDPROC:        ;eax can be used to convey information to the call
        PUSH EBP                ;use ebp to avoid using eax which may hold information
        MOV EBP,[ESP+10h]       ;uMsg
        MOV ECX,[EDX]           ;get number of messages to do
        ADD EDX,4               ;jump over size dword
        L2:
            DEC ECX
            JS >L3
            CMP [EDX+ECX*8],EBP     ;see if its the correct message
            JNZ L2                  ;no
            MOV EBP,ESP
            PUSH ESP,EBX,EDI,ESI    ;save registers as required by Windows
            ADD EBP,4               ;allow for the extra call to here
            ;now [EBP+8]=hwnd, [EBP+0Ch]=uMsg, [EBP+10h]=wParam, [EBP+14h]=lParam,
            CALL [EDX+ECX*8+4]      ;call the correct procedure for the message
            POP ESI,EDI,EBX,ESP
            JNC >L4                 ;nc=return value in eax - don't call DefWindowProc
        L3:
            PUSH [ESP+18h],[ESP+18h],[ESP+18h],[ESP+18h]     ;allowing for change of ESP
            CALL DefWindowProcA
        L4:
            POP EBP
        RET
    ;
    ;******************* This is the actual window procedure
    WndProcTable:
        MOV EDX,ADDR MESSAGES   ;give edx the list of messages to deal with
        CALL GENERAL_WNDPROC    ;call the generic message handler
        RET 10h              ;restore the stack as required by caller


    INITIALISE_WNDCLASS:    ;get ready to register the window class
        MOV EBX,ADDR WNDCLASS
        MOV EAX,9

        L1:
            MOV D[EBX+EAX*4],0      ;fill it with zeroes
            DEC EAX
            JNS L1
            ;***** add things to window class for all windows in the program ..
        MOV EAX,[hInst]         ;get handle to the process
        MOV [EBX+10h],EAX       ;make it the window class
        PUSH 32512              ;IDC_ARROW common cursor
        PUSH 0
        CALL LoadCursorA        ;get in eax, handle to arrow cursor
        MOV [EBX+18h],EAX       ;and give to WNDCLASS
        MOV D[EBX+1Ch],6D       ;set background brush to COLOR_WINDOW+1
        RET


    START:
        PUSH -11D               ;STD_OUTPUT_HANDLE
        CALL GetStdHandle
        mov [hASB], eax
        
        

        PUSH 0
        CALL GetModuleHandleA   ;get handle to the process
        MOV [hInst],EAX         ;record it in data label hInst

        CALL INITSNAKE
        CALL INITIALISE_WNDCLASS

        MOV D[EBX],1h+2h+40h    ;CS_VREDRAW+CS_HREDRAW+CS_CLASSDC (window class style)
        MOV D[EBX+4],ADDR WndProcTable          ;window procedure
        MOV D[EBX+24h],ADDR WINDOW_CLASSNAME    ;window class name
        PUSH EBX                ;address of structure with window class data
        CALL RegisterClassA     ;register the window class
        PUSH 0,[hInst],0,0      ;owner=desktop
        PUSH HEIGHT               ;height
        PUSH WIDTH               ;width
        PUSH 50D,50D            ;position y then x
        PUSH 90000000h +0C00000h+80000h +20000h      ;window style
        ;(POPUP+VISIBLE)+CAPTION+SYSMENU+MINIMIZEBOX
        PUSH 'THE ADVENTURES OF ASM THE SNAKE'       ;window title
        PUSH ADDR WINDOW_CLASSNAME     ;window class name
        PUSH 0                  ;extended window style
        CALL CreateWindowExA    ;make window, returning handle in EAX
        mov [hwnd], eax

        push ADDR TIMER
        push 0x3D
        push 42
        push eax
        call SetTimer


        call INITSNAKE

        L1:
            PUSH 0,0,0
            PUSH ADDR MSG
            CALL GetMessageA        ;wait for message from Windows
            OR EAX,EAX              ;see if it is WM_QUIT
            JZ >L2                  ;yes
            PUSH ADDR MSG
            CALL TranslateMessage   ;no so convert message to character if necessary
            PUSH ADDR MSG
            CALL DispatchMessageA   ;and send the message to the window procedure
            push ebx, ADDR WINDOW_CLASSNAME
            JMP L1                  ;after message dealt with, loop back for next one
        L2:
            PUSH [hInst],ADDR WINDOW_CLASSNAME   ;message was WM_QUIT
            CALL UnregisterClassA   ;ensure class is removed
            PUSH [MSG+8h]           ;exit code (send contents of wParam)
            CALL ExitProcess        ;return to Windows in the manner it prefers
        
    
        RET
