; ==============================================================================
;                    Draw frame using console arguments
; ==============================================================================
.model tiny
.code
.186
org 100h
locals @@
; --Constants-------------------------------------------------------------------
VIDEO_SEG      = 0b800h         ; Video segment address
CONSOLE_ADR    = 80h            ; Console args  address
DISPLAY_WIDTH  = 80d
TEMPS_AMOUNT   = 04h            ; Templates amount
SHADOW_ATTR    = 4eh
SHADOW_OFFS    = 2d
; --End of constants------------------------------------------------------------

start: jmp main

; --Terminate program macro-----------------------------------------------------
; brief:        Terminate DOS program
; entry:        AL      - Exit code
.terminate      macro
                mov ah, 4ch     ; DOS FN 4ch - exit
                int 21h
                endm
; --End of terminate program macro----------------------------------------------

; --draw_frame_line procedure--------------------------------------------------------
; brief:        Draw one line of frame
; entry:        AH    - Frame attributes
;               BH    - Frame width
;               DX    - Next line offset
;               DS:SI - Frame template address
;               ES:DI - Cursor position
; destroys:     SI, DI, CX, AL
; assumes:      ES = Video Segment
; ------------------------------------------------------------------------------
draw_frame_line     proc

                    lodsb                                ; Get left symbol
                    stosw                                ; Draw it

                    mov  cl, bh                          ; CH := Frame width
                    xor  ch, ch
                    lodsb                                ; Get middle symbol
                    rep stosw                            ; Draw it width times

                    lodsb                                ; Get left symbol
                    stosw                                ; Draw it

                    add  di, ( DISPLAY_WIDTH - 2) * WORD ; Move to next line end
                    sub  di, dx                          ; Move to line start

                    ret

                    endp
; --End of draw_frame_line procedure--------------------------------------------------------

; BAH: Make documentation
; --Draw frame shadow procedure--------------------------------------------------------
; brief:        Draw frame shadow
; entry:        BX    - Frame size
;               ES:DI - Cursor position
; destroys:     AX, SI, DI, CX, DX
; assumes:      ES = Video Segment
; ------------------------------------------------------------------------------
draw_frame_shadow       proc

                add  di, SHADOW_OFFS * WORD   ; Offset shadow start position
                mov  ah, SHADOW_ATTR          ; Set shadow attributes

                mov  cl, bh                   ; CX := frame width
                xor  ch, ch                   ;
@@draw_hor:     mov es:[di + 1], ah           ; Change symbol attribute
                add  di, WORD                 ; Move cursor to next position
                loop @@draw_hor

                mov  cx, SHADOW_OFFS          ; Set amount of vertical lines
@@draw_vert:    mov  si, cx                   ; Save CX
                mov  dx, di                   ; Save cursor position
                mov  cl, bl                   ; Set shadow height
                add  cl, 2d                   ; Height + top and bottom lines
                xor  ch, ch
@@vert_loop:
                mov  es:[di + 1], ah          ; Change symbol attribute
                sub  di, DISPLAY_WIDTH * WORD ; Move cursor to next position
                loop @@vert_loop

                mov  di, dx                   ; Load cursor position
                add  di, WORD                 ; Move to next verical line
                mov  cx, si                   ; Load CX
                loop  @@draw_vert

                ret

                endp
; --End of draw frame shadow procedure--------------------------------------------------------

; --draw_empty_frame procedure--------------------------------------------------------
; brief:        Draw frame without text
; entry:         [bp + 4] - Template address
;                [bp + 6] - Attributes
;                [bp + 8] - Size ([bp + 8] - Width, [bp + 9] - Height)
;               [bp + 10] - Position
; destroys:     AX, BX, CX, DX, SI, DI
; assumes:      ES = Video Segment
; ------------------------------------------------------------------------------
draw_empty_frame  proc

            push bp                 ; Prologue
            mov  bp, sp             ;

            mov  si, [bp + 4]          ; Get template
            mov  ah, byte ptr [bp + 6] ; Get attributes
            mov  bx, [bp + 8]          ; Get size
            mov  di, [bp + 10]         ; Get position

            mov  dl, bh             ; DX := frame width / 2
            xor  dh, dh             ;
            shl  dx, 1              ;

            call draw_frame_line    ; Draw line

            xor  ch, ch             ; CX := height
            mov  cl, bl
@@height:   push cx                 ; Save CX
            call draw_frame_line    ; Draw line

            pop  cx                 ; Load CX
            sub  si, 3              ; Move to middle line template start
            loop @@height

            add  si, 3              ; Move to bottom line template
            call draw_frame_line    ; Draw line

            call draw_frame_shadow

            pop  bp                 ; Epilogue
            ret  4 * WORD           ;

            endp
; --End of draw_empty_frame procedure--------------------------------------------------------

; --parse_args procedure--------------------------------------------------------
; brief:        Parse frame settings from console
; entry:        ES:DI - Cursor position
; BAH: make return value through CF
; return:       AX - Error code (0 - no error, -1 - error)
;               BX - Frame size
;               CX - Frame template address
;               DX - Frame attribute
;               SI - Text address
; ------------------------------------------------------------------------------
parse_args  proc

            mov  si, CONSOLE_ADR      ;
            lodsb                     ; Get console symbols amount
            test  al, al
            je    @@error             ; If (amount of symbols == 0) => error
            inc   si                  ; Skip space

            call get_number           ; Get width
            cmp  al, 0                ; Check get_number return value
            jb   @@error
            mov  dh, al

            call get_number           ; Get height
            cmp  al, 0                ; Check get_number return value
            jb   @@error
            mov  dl, al
            push dx

            call get_number           ; Get attribute
            cmp  al, 0                ; Check get_number return value
            jb  @@error
            mov  dx, ax

            call get_temp             ; Get template
            test ax, ax               ; Check get_number return value
            je   @@error

            inc si                    ; Skip space
            mov cx, ax                ; Return console args through registers
            pop bx
                                      ; DX and SI already has the correct value
            xor ax, ax                ; Return no error
            ret

@@error:    mov  ax, -1               ; Return error code
            ret

            endp
; --End of parse_args procedure--------------------------------------------------------

; --get_temp procedure--------------------------------------------------------
; brief:        Get frame template address
; entry:        DS:SI - Console cursor position
; destroys:     SI, BX, CX
; return:       AX template address ( 0 if error )
; ------------------------------------------------------------------------------
get_temp        proc

                lodsb                 ; Get first symbol
                cmp  al, '*'          ; If (symbol == '*') => new template
                je   @@new_temp

                ; BAH: Use get_number procedure?
                sub  al, '0'          ; Get template number

                cmp  al, TEMPS_AMOUNT ; Check template number validity
                ja   @@error

                mov  bl,  09h         ; Get template start address
                mul  bl               ; Get address relatively frame_temps
                add  ax, offset frame_temps
                ret

@@new_temp:     mov  bx, si           ; Template address := console cursor address
                mov  cx, 09h
@@check:        lodsb                 ; Check for new template validity
                cmp  al, ' '
                je   @@error
                cmp  al, 0Dh
                je   @@error

                loop @@check

                mov  ax, bx           ; Return template address
                ret

@@error:        xor  ax, ax
                ret

                endp
; --End of get_temp procedure--------------------------------------------------------

; --get_number procedure--------------------------------------------------------
; brief:        Get decimal number from string
; entry:        DS:SI - Console cursor position
; return:       AX number ( -1 if error )
; destroys:     BX, CX, SI
; ------------------------------------------------------------------------------
get_number      proc

                ; BAH: Remake with ax instead of BX
                xor  ax, ax
                xor  bh, bh
                mov  cl, 10               ; CL := notation factor (decimal number system)

@@get_digit:    mov  bl, byte ptr ds:[si] ; Load symbol from console to BL
                inc  si

                cmp  bl, ' '              ; If space symbol => number read
                je   @@final
                cmp  bl, 0Dh              ; '\r' ASCII code
                je   @@final

                cmp  bl, '0'              ; Check the digit for validity
                jl   @@error
                cmp  bl, '9'
                jg   @@error

                sub  bl, '0'               ; Convert symbol to digit

                mul  cl                    ; AX := AL * 10
                add  ax, bx
                jmp  @@get_digit

@@error:        mov  al, -1                ; Return error
@@final:        ret

                endp
; --End of get_number procedure--------------------------------------------------------

; --get_str_len procedure--------------------------------------------------------
; brief:        Get '$' terminated string length
; entry:        AX   - String start address
; return:       AX     String length ( 0 if error )
; destroys:     DI, SI, CX
; assumes:      ES = Data Segment
;               String max length = 80d
; ------------------------------------------------------------------------------
get_str_len     proc

                mov   di, ax       ; Save string start address
                mov   si, ax       ;
                mov   al, '$'      ; Set the symbol looking for
                mov   cx, 40d      ; Max string length

                repne scasb        ; Find '$' symbol
                je    @@found
                xor   ax, ax       ; If byte not found AX:= 0
                ret

@@found:        mov   ax, di       ; Move return value to AX
                dec   ax
                sub   ax, si       ; Get string length
                ret

                endp
; --End of get_str_len procedure--------------------------------------------------------

; --print_frame_title procedure--------------------------------------------------------
; brief:        Print frame title text
; entry:        AH  - Text attributes
;               DI  - Title start position
;               CX  - String length
;               SI  - String address
; destroys:     SI, DI, CX, BX, DX
; assumes:      ES = Data Segment
; ------------------------------------------------------------------------------
print_frame_title proc

                ; Calculate start text position
                xchg bh, bl               ; BAH: mb change width and height pos
                xor  bh, bh

                mov  dx, bx
                sub  dx, cx
                and  dx, 0fffeh           ;  Make a number multiple of 2 (set right bit to 0)
                add  di, 2
                add  di, dx

@@print_sym:    lodsb                     ; Print string
                stosw
                loop @@print_sym

                ret

                endp

main    proc

        push bp         ; Prologue
        mov  bp, sp     ;
        sub  sp, 8      ;


        call parse_args         ; Parse console arguments
        test ax, ax             ; Check return value
        jne  @@exit_err
        mov [bp - 2], si        ; Save text address
        mov [bp - 4], dx        ; Save text attribute

        push cx                 ; Save CX
        mov  ax, si
        call get_str_len        ; Get text length
        pop cx                  ; Load CX
        mov [bp - 6], ax        ; Save text length
        cmp al, bh
        jb @@without_resize     ; if text larger than width resize frame
        mov bh, al
@@without_resize:
        mov  si, VIDEO_SEG
        mov  es, si             ; Set ES to video segment

        mov  [bp - 8], (5 * DISPLAY_WIDTH + 10) * WORD   ; Set frame position

        push [bp - 8]            ; Position
        push bx                  ; Size
        push dx                  ; Attribute
        push cx                  ; Template
        call draw_empty_frame    ; Draw  frame

        mov si, [bp - 2]         ; Load text address
        mov ah, [bp - 4]         ; Load text attribute
        mov cx, [bp - 6]         ; Load text length
        mov di, [bp - 8]         ; Load frame position
        call print_frame_title   ; Print frame text

        xor ah, ah

@@exit: mov sp, bp               ; Epilogue
        pop bp                   ;
        .terminate

@@exit_err:
        mov dx, offset err_msg ; DX := err_msg address
        mov ah, 09h            ; DOS fn 09h - print a string
        int 21h
        mov al, 1              ; Set exit return value
        jmp @@exit

        endp

; --Constants-------------------------------------------------------------------
frame_temps  db 9 dup(20h)
             db 0dah, 0c4h, 0bfh, 0b3h, 020h, 0b3h, 0c0h, 0c4h, 0d9h
             db 0c9h, 0cdh, 0bbh, 0bah, 020h, 0bah, 0c8h, 0cdh, 0bch
             db 003h, 003h, 003h, 003h, 020h, 003h, 003h, 003h, 003h

err_msg      db 'Exit with error$'
; --End of constants------------------------------------------------------------

end start

