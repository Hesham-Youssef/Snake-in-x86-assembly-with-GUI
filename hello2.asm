;------------------------------------------------------------------
;
;      HelloWorld3 - copyright Jeremy Gordon 2002
;
;      "HELLO WORLD" WINDOWS GDI PROGRAM - demonstrating using
;      formalised structures, automated stack frames using FRAME..ENDF
;      USEDATA and USES, INVOKE, definitions and named re-usable labels 
;      - in GoAsm syntax
;
;      Assemble using GoAsm HelloWorld3 (produces PE COFF file)
;      Then link using:-
;      GoLink HelloWorld3.obj user32.dll kernel32.dll gdi32.dll
;      (add -debug coff to run through the program using the debugger)
;
;------------------------------------------------------------------
;
;First lets specify a couple of formalised structures
WNDCLASSEX STRUCT
      cbSize        DD 30h    ;+0 size of the structure (WNDCLASSEX only)
      style         DD ?      ;+4 window class style (CS_)
      lpfnWndProc   DD ?      ;+8 pointer to Window Procedure
      cbClsExtra    DD ?      ;+C no. of extra bytes to allocate after structure
      cbWndExtra    DD ?      ;+10 no. of extra bytes to allocate after window instance
      hInstance     DD ?      ;+14 handle to instance for this window class
      hIcon         DD ?      ;+18 handle to the class icon
      hCursor       DD ?      ;+1C handle to the class cursor
      hbrBackground DD ?      ;+20 identifies the class background brush
      lpszMenuName  DD ?      ;+24 pointer to resource name for class menu
      lpszClassName DD ?      ;+28 pointer to string for window class name
      hIconSm       DD ?      ;+2C handle to the small icon
      ENDS
;
RECT STRUCT
      left    DD ?
      top     DD ?
      right   DD ?
      bottom  DD ?
      ENDS
;
PAINTSTRUCT STRUCT
      hdc           DD ?
      fErase        DD ?
      rcPaint       RECT
      fRestore      DD ?
      fIncUpdate    DD ?
      rgbReserved   DB 32 DUP ?
      ENDS
;
;and now some definitions for later use
;*** first some window class styles
      CS_VREDRAW    =1h
      CS_HREDRAW    =2h
      CS_CLASSDC    =40h
      ;*** now some window styles
      WS_POPUP      =80000000h
      WS_VISIBLE    =10000000h
      WS_CAPTION    =0C00000h
      WS_SIZEBOX    =40000h
      WS_SYSMENU    =80000h
      WS_MINIMIZEBOX=20000h
      WS_MAXIMIZEBOX=10000h
      ;*** messages we shall be dealing with
      WM_CREATE     =1h
      WM_DESTROY    =2h
      WM_PAINT      =0Fh
      ;*** and miscellaneous
      COLOR_WINDOW  =5
;------------------------------------------------------------------
;
DATA SECTION
;
;------------------------------------------------------------------
      wcex WNDCLASSEX       ;establish the WNDCLASS structure in data
      hInst DD 0            ;handle to the process itself
      MSG DD 7 DUP 0        ;structure to hold messages from Windows as follows:-
      ;hWnd, +4=message, +8=wParam, +C=lParam, +10h=time, +14h/18=pt
;------------------------------------------------------------------
;
CONST SECTION
;
;------------------------------------------------------------------
;
WINDOW_CLASSNAME DB 'WC',0            ;string to hold name of window class
;
;------------------------------------------------------------------
;
CODE SECTION
;
;------------------------------------------------------------------
INITIALISE_WNDCLASS:    ;get ready for window
      MOV EBX,ADDR wcex
      ADD EBX,4               ;jump over size dword
      MOV EAX,10
L1:
      MOV D[EBX+EAX*4],0      ;fill rest of structure with zeroes
      DEC EAX
      JNS L1
;***** add things to window class for all windows in the program ..
      MOV EAX,[hInst]         ;get handle to the process
      MOV [wcex.hInstance],EAX   ;give to window class structure
      PUSH 32512              ;IDC_ARROW common cursor
      PUSH 0
      CALL LoadCursorA        ;get in eax, handle to arrow cursor
      MOV [wcex.hCursor],EAX  ;and give to WNDCLASS
      MOV D[wcex.hbrBackground],COLOR_WINDOW+1  ;set background brush
      RET
;
;*******************
CREATE:                 ;the only message actually dealt with in this program
      XOR EAX,EAX             ;return zero to make window
      RET
;
DESTROY:                ;one of the few messages dealt with by this prog
      PUSH 0
      CALL PostQuitMessage    ;exit via the message loop
      STC                     ;go to DefWindowProc too
      RET
;
;------------------------------------------------------------
;
CONST SECTION
;
;------------------------------------------------------------
;******************** Window message table 
;(In a real program this would deal with many more messages)
;
      MESSAGES DD WM_CREATE,    CREATE       ;the message then the code address
            DD WM_DESTROY,  DESTROY
            DD WM_PAINT,      PAINT
;------------------------------------------------------------
;
CODE SECTION
;
;------------------------------------------------------------
;******************* This is the actual window procedure
WndProc:
      FRAME hwnd,uMsg,wParam,lParam     ;establish stack frame and get parameters
      ;
      MOV EAX,[uMsg]          ;get in eax message sent by Windows
      MOV ECX,SIZEOF MESSAGES/8         ;get number of messages to look at
      MOV EDX,ADDR MESSAGES
L2:
      DEC ECX
      JS >.notfound
      CMP [EDX+ECX*8],EAX     ;see if its the correct message
      JNZ L2                  ;no
      CALL [EDX+ECX*8+4]      ;call the correct procedure for the message
      JNC >.exit
      .notfound
      INVOKE DefWindowProcA,[hwnd],[uMsg],[wParam],[lParam]
      .exit
      RET
      ENDF                    ;finish this satck frame
;
;**** and now a procedure, outside the FRAME, which must address the 
;**** local data held on the stack in the WndProc FRAME
;This paints an ellipse in the rectangle provided by Windows on the
;message WM_PAINT. This rectangle is the area which needs updating, eg
;on resizing or if the window is uncovered by another.  Painting is
;done using the device context provided by Windows.
;
PAINT:
      USEDATA WndProc         ;use parameters sent to WndProc
      USES EBX,EDI,ESI        ;save registers as required by Windows
      LOCAL lpPaint:PAINTSTRUCT, hDC             ;establish local data
      ;
      INVOKE BeginPaint, [hwnd],ADDR lpPaint     ;get in eax the DC to use
      MOV [hDC],EAX                              ;save it in local data
      INVOKE Ellipse, [hDC],[lpPaint.rcPaint.left],   \
                        [lpPaint.rcPaint.top] ,   \
                        [lpPaint.rcPaint.right],  \
                        [lpPaint.rcPaint.bottom]
;
;       other paint procedures would go here
      ;
      INVOKE EndPaint, [hwnd],ADDR lpPaint       ;give DC back to Windows
      XOR EAX,EAX             ;return not carry and eax=0
      RET
      ENDU                    ;finish using parameters sent to WndProc
      ;
;*******************************************************************
START:
;
      INVOKE GetModuleHandleA, 0        ;get handle to the process
      MOV [hInst],EAX                   ;record it in data label
      CALL INITIALISE_WNDCLASS          ;get ready to register window class
      ;********** now add things specific to the window to be made
      MOV D[wcex.style],CS_VREDRAW+CS_HREDRAW+CS_CLASSDC
      MOV [wcex.lpfnWndProc],ADDR WndProc                ;window procedure
      MOV [wcex.lpszClassName],ADDR WINDOW_CLASSNAME     ;window class name
      INVOKE RegisterClassExA,ADDR wcex       ;register the window class
      INVOKE CreateWindowExA, 0, ADDR WINDOW_CLASSNAME,   \
            'Hello World window made by GoAsm',          \ ;title string
            WS_POPUP|WS_VISIBLE|WS_CAPTION|WS_SIZEBOX    \ ;window style
            |WS_SYSMENU|WS_MINIMIZEBOX|WS_MAXIMIZEBOX,   \
            50,50,                                       \ ;x-pos then y-pos
            320,200,                                     \ ;width then height
            0,0,[hInst],0
      ;************************ now enter the main message loop
      .messloop
      INVOKE GetMessageA, ADDR MSG,0,0,0
      OR EAX,EAX              ;see if it is WM_QUIT (eax=0)
      JZ >.quit               ;yes
      INVOKE TranslateMessage, ADDR MSG
      INVOKE DispatchMessageA, ADDR MSG
      JMP .messloop           ;after message dealt with, loop back for next one
      .quit
      INVOKE UnregisterClassA, ADDR WINDOW_CLASSNAME,[hInst]
      INVOKE ExitProcess, [MSG+8h]
