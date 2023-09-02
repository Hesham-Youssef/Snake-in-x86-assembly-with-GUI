
CELL STRUCT ; 8 byte total
    coord   DD      0
    next    DD      0
CELL ENDS


DATA SECTION
    ; HEAP_ZERO_MEMORY equ 0x00000008
    head    CELL    <500, 0> ; [y | x]
    tail    DD      ?

    valx    DW      5
    valy    DW      8

    foodCoord   DD  0

    hHeap   DD      ?
    dwBytes equ      sizeof CELL
    dwFlags equ     0x00000008 ; HEAP_ZERO_MEMORY (sets newly allocated mem to zero)

    hInst   DD      0

    WINDOW_CLASSNAME DB 'hello',0

    hDC   DD 0            ;to keep the handle of the device context
    PAINTSTRUCT DD 16 DUP 0  ;structure to hold stuff from Windows on WM_PAINT
    MSG DD 7 DUP 0

    MESSAGES DD (ENDOF_MESSAGES-$-4)/8      ;=number to be done
            DD  1h,CREATE,2h,DESTROY,0Fh,PAINT
    ENDOF_MESSAGES: 

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
CODE SECTION

    FOODEATEN: ;Reallocate extra cell and randomize the foods position
        call ADDNEWNODE
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
            test ebx, ebx
            jz >DONE
            mov eax, [ebx]
            mov ebx, [ebx + 4]
            mov [ebx], eax
            jmp L1
        DONE:

        RET



    DRAWER:


    ADDNEWNODE:
        invoke HeapAlloc, [hHeap], dwFlags, 9 ;8 for the node and one extra because otherwise an error will occur
        mov ebx, [tail]
        mov [tail], eax
        mov [ebx + 4], eax
        RET

    INITSNAKE:
        call GetProcessHeap
        mov [hHeap], eax

        mov [tail], ADDR head
        mov [head], 0x00320031
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


    PAINT:
        MOV EBX,ADDR PAINTSTRUCT
        PUSH EBX,[EBP+8h]           ;EBP+8h=hwnd
        CALL BeginPaint             ;get device context to use, initialise paint
        MOV [hDC],EAX


        mov ebx, ADDR head
        DRAWSNAKE:
            test ebx, ebx
            jz >snakeend
            
            mov eax, ADDR RECT

            mov edx, [ebx]
            imul dx, 5
            mov [eax], dx
            ADD dx, 5
            mov [eax+8], dx

            shr edx, 16
            imul dx, 5
            mov [eax+4], dx
            ADD dx, 5
            mov [eax+12], dx

            push 9
            push ADDR RECT
            push [hDC]

            call FillRect
            mov ebx, [ebx + 4]
            jmp DRAWSNAKE

        snakeend:

        PUSH ADDR PAINTSTRUCT, [EBP+8h]           ;EBP+8h=hwnd
        CALL EndPaint
        XOR EAX,EAX
        RET

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
        RET 10h                 ;restore the stack as required by caller
    

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
        PUSH 500               ;height
        PUSH 500               ;width
        PUSH 50D,50D            ;position y then x
        PUSH 90000000h +0C00000h+40000h +80000h +20000h     +10000h      ;window style
        ;(POPUP+VISIBLE)+CAPTION+SIZEBOX+SYSMENU+MINIMIZEBOX+MAXIMIZEBOX
        PUSH 'Hello World window made by GoAsm'       ;window title
        PUSH ADDR WINDOW_CLASSNAME     ;window class name
        PUSH 0                  ;extended window style
        CALL CreateWindowExA    ;make window, returning handle in EAX

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
