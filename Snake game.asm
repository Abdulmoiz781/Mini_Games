org 0x100

jmp start

intilializing:
	
	body_positions times 100 dw 0  ; Stores X,Y pairs (max 100 segments)
	walls_index dw 0
	body_index dw 0    
	rno: db 12
	cno: db 40
	fruit_x: db 0
	fruit_y: db 0
	fc: db 0 ;food_collision logic
	total: db 0
	snake_direction: db 'R'  ; U=Up, D=Down, L=Left, R=Right
	snake_length:   dw 3     ; Current length
	score:          dw 0 	; Player score
	score_msg:db 'Score:','$'
	score_msg_2:db 'Your Score Was: '
	game_over_msg: db 'Game Over :(' ,'$'
	game_over:      db 0 
	exit_msg: db 'Exit successfully' ,'$'
	restart_msg: db 'Press R to Play Again'
	exit_msg_2: db 'Press E to Exit'
	

clearscr:
    push es
    push ax
    push di
    mov ax, 0xb800
    mov es, ax
    xor di, di
    mov ax, 0x0720  ; space char
	;mov ax, 0x1720
    mov cx, 2000    ; 80x25 words
    rep stosw
    pop di
    pop ax
    pop es
    ret
	
	
r: 
	mov al,'>'
	jmp apply_it

l:
	mov al,'<'
	jmp apply_it
	
u:
	mov al,'^'
	jmp apply_it

	
d:
	mov al,'v'
	jmp apply_it

draw_head:
    mov ax, 0xB800
    mov es, ax
    xor ax, ax
    	
store_row:
    ;mov [rno], al

    mov al, [rno]    
    mov bl, 80       
    mul bl          
    add al, [cno]    
    adc ah, 0        
    shl ax, 1        
    mov di, ax      
    mov ah,07h

	;mov al,'*'
	cmp byte [snake_direction],'L'
	je l
	
	cmp byte [snake_direction],'R'
	je r
	
	cmp byte [snake_direction],'U'
	je u
	
	cmp byte [snake_direction],'D'
	je d

	
	apply_it:
	
	mov [es:di],ax
	
	ret
	
clear_head:
	mov ax,0xb800
	mov es,ax
	xor ax,ax
	
	mov al, [rno]    ; AL = row (1)
    mov bl, 80       ; 80 columns per row
    mul bl           ; AX = rno * 80
    add al, [cno]    ; AX += cno (1)
    adc ah, 0        ; Handle carry (r using cuz the vaue will be too large
	                                 ;and will generate a carry)
    shl ax, 1        ; AX *= 2 (each cell is 2 bytes)
    mov di, ax   
		
	mov ax,0720h
	
	mov [es:di],ax
	
	ret
	

draw_body: ;Correct
    mov ax, 0xB800
    mov es, ax
    
    mov cx, [snake_length]
    dec cx             ; Don't draw the head
    jz done

    mov si, [body_index]

looping:
    sub si, 2
    js wraparound     ; If si < 0, wrap

draw_segment:
    ; Get X, Y from body_positions[si]
    mov al, [body_positions + si + 1]  ; Y
    mov bl, 80
    mul bl
    add al, [body_positions + si]     ; X
    adc ah, 0
    shl ax, 1
    mov di, ax

    mov ah, 0x02
    mov al, 'O'
    mov [es:di], ax

    loop looping
    jmp done

wraparound:
    mov si, 198     ; 100 * 2 - 2 = last valid position in body_positions arr
    jmp draw_segment

done:
     ret
	
	
clear_body:
    mov ax, 0xB800
    mov es, ax
    push si

    mov cx, [snake_length]
    cmp cx, 1
    jbe skip_clear   ; Don't clear anything if there's no tail yet

    shl cx, 1                  ; cx = length * 2
    mov si, [body_index]
    sub si, cx
    jns no_wrap
    add si, 200

no_wrap:
    mov al, [body_positions + si + 1]  ; Y
    mov bl, 80
    mul bl
    add al, [body_positions + si]     ; X
    adc ah, 0
    shl ax, 1
    mov di, ax

    mov ax, 0x0720
    mov [es:di], ax

skip_clear:
    pop si
    ret




update_positions:
    ; Save current head position to body
    mov si, [body_index]
    mov al, [cno]
    mov [body_positions+si], al   ; Store X
    mov al, [rno]
    mov [body_positions+si+1], al ; Store Y
    
    ; Update index (circular buffer)
    add si, 2
    cmp si, 100*2  ; Max buffer size
    jb store_index
    xor si, si   ; Wrap around

store_index:
    mov [body_index], si
    ret

	

fruits_logic:
    call generate_random_fruit 
	call check_fruit_on_walls	
    call check_fruit_on_snake
    ret

generate_random_fruit:
    ; Generate random X (1-78) and Y (1-23)
    mov ah, 00h
    int 1Ah        ; CX:DX = timer ticks
    mov ax, dx
    xor dx, dx
    mov cx, 78     ; X range: 1-78
    div cx
    add dl, 1      ; Ensure X is 1-78
    mov [fruit_x], dl

    ; Reuse DX for Y
	mov ah, 00h
    int 1Ah
    mov ax, dx
    xor dx, dx
    mov cx, 23     ; Y range: 1-23
    div cx
    add dl, 1
    mov [fruit_y], dl
    ret
	
check_fruit_on_walls:
	push ax
	mov al,[fruit_x]
	cmp al,1
	jl regenerate_fruit
	cmp al,78
	jg regenerate_fruit
	mov al,[fruit_y]
	cmp al,1
	jl regenerate_fruit
	cmp al,23
	jg regenerate_fruit
	pop ax
	ret

check_fruit_on_snake:
	push si
	push ax
    mov si, 0      ; Start at first body segment
	mov cx, [snake_length]  ; Number of segments to check
.check_loop:
    ; Compare X
    mov al, [fruit_x]
    cmp al, [body_positions + si]
    jne .next_segment

    ; Compare Y
    mov al, [fruit_y]
    cmp al, [body_positions + si + 1]
    je .regenerate  ; If both X and Y match, regenerate

.next_segment:
    add si, 2       ; Move to next x,y pair
    loop .check_loop
	
	pop ax
	pop si

    ; If no collision, draw the fruit
    mov ax, 0xB800
    mov es, ax
    mov al, [fruit_y]
    mov bl, 80
    mul bl
    add al, [fruit_x]
    adc ah, 0
    shl ax, 1
    mov di, ax
    mov ax, 0x0D0F   ; Red 'O'
    mov [es:di], ax
    ret
	
.regenerate:
    pop ax
    pop si
    jmp regenerate_fruit

regenerate_fruit:
    call generate_random_fruit  ; Get new random position
	call check_fruit_on_walls
    jmp check_fruit_on_snake   ; Re-check (loop until valid)
	ret

food_collision:
	mov byte [fc], 0

	mov al, [rno]
    cmp al, [fruit_y]
	jne no_collision
	
	collission:
		inc byte[fc]
		mov al, [cno]
		cmp al, [fruit_x]
		jne no_collision
		
	next:
		inc byte[fc]
		cmp byte [fc], 2
		jne no_collision
		
	C_detected:	
		inc word [score]
		call displaying_score
		inc word [snake_length] ;to increase snake length
		call fruits_logic 
	
	no_collision:
		ret
		
displaying_score:
    push es
    pusha
    
    mov ax, 0xB800
    mov es, ax
    
    ; 2. Display "SCORE: " text
    ; (Calculate position once, e.g., top-right: row 0, column 70)
    mov di, (0*80 + 70) * 2  ; (Y*80 + X)*2
    
    ; Write "SCORE: " directly to video memory
    mov si, score_msg
    mov cx, 7                 ; Length of "SCORE: "
.write_text:
    mov al, [si]
    mov ah, 0x0F              ; White on black
    mov [es:di], ax
    inc si
    add di, 2
    loop .write_text
  
display_s:  
    ; 3. Convert score to ASCII and display
    mov ax, [score]           ; Load 16-bit score
    mov bx, 10                ; Divisor for digit extraction
    mov cx, 0                 ; Digit counter
    
.convert_loop:
    xor dx, dx                ; Clear DX for division
    div bx                    ; AX = quotient, DX = remainder (digit)
    add dl, '0'               ; Convert to ASCII
    push dx                   ; Save digit (reverse order)
    inc cx
    test ax, ax
    jnz .convert_loop
    
    ; Write digits to screen (right-to-left)
    mov di, (0*80 + 76) * 2   ; Adjust position for digits
.write_digits:
    pop ax                    ; Get digit from stack
    mov ah, 0x0F              ; Attribute
    mov [es:di], ax
    add di, 2                 ;to move right to print next num
    loop .write_digits
    
    popa
    pop es
    ret
	
collision_with_body:
    pusha
    push es

    mov al, [cno]    ; head X
    mov bl, [rno]    ; head Y

    ; get total segments to check
    mov cx, [snake_length]
    dec cx
    jz .no_collision   ; if snake length is 1, there's no body to check

    mov si, [body_index]
    sub si, 2          ; points to the last added head segment
    js .wrap_si

    jmp .start_loop

.wrap_si:
    add si, 200        ; wrap to last position in buffer

.start_loop:
    mov dx, si         ; store index of head (to skip it)

.loop:
    cmp si, dx         ; skip checking the current head position
    je .skip_compare

    mov dl, [body_positions + si]       ; body X
    mov dh, [body_positions + si + 1]   ; body Y

    cmp al, dl         ; head X == body X?
    jne .next
    cmp bl, dh         ; head Y == body Y?
    je .collision_found

.skip_compare:
.next:
    sub si, 2
    js .wrap_loop
    jmp .continue

.wrap_loop:
    add si, 200

.continue:
    loop .loop

.no_collision:
    pop es
    popa
    ret

.collision_found:
    mov byte [game_over], 1
    call display_game_over
    jmp exit_2

display_game_over:
    push es
    pusha

    ; Set up video memory
    mov ax, 0xB800
    mov es, ax

    ; Show "Game Over :("
    mov di, (0*80 + 66) * 2  ; Row 0, Col 65
    mov si, game_over_msg
    mov cx, 12               ; Length of message
.write_text:
    mov al, [si]
    mov ah, 0x0F             ; White on black
    mov [es:di], ax
    inc si
    add di, 2
    loop .write_text

    ; Show "Your Score Was: "
    mov di, (11*80 + 28) * 2  ; Row 11, Col 28
    mov si, score_msg_2
    mov cx, 16               ; Adjust to your message length
.write_text_2:
    mov al, [si]
    mov ah, 0x0F
    mov [es:di], ax
    inc si
    add di, 2
    loop .write_text_2

    ; Now show the actual score (converted to ASCII)
    ; di is already at the end of "Your Score Was: "
    mov ax, [score]          ; Score in AX
    mov bx, 10
    mov cx, 0

.convert_digits:
    xor dx, dx
    div bx
    add dl, '0'
    push dx
    inc cx
    test ax, ax
    jnz .convert_digits

.display_digits:
    pop ax
    mov ah, 0x0F
    mov [es:di], ax
    add di, 2 
    loop .display_digits

    popa
    pop es
    ret

draw_walls:
    mov ax, 0xB800
    mov es, ax

    ; Top and Bottom borders
    mov cx, 80         ; 80 columns
    xor di, di         ; Start of video memory

top_bottom_loop:
    mov ax, 0x0723     ; Attribute 07, char '#'
    mov [es:di], ax         ; Top border
    mov [es:di + 3840], ax  ;Bottom border
    add di, 2
    loop top_bottom_loop

    ; Left and Right borders
    mov cx, 25         ; 25 rows
    mov bx, 0          ; Start at column 0
left_right_loop:
    mov ax, 0x0723     ; Attribute 07, char '#'
    mov di, bx
    mov [es:di], ax           ; Left border
    mov [es:di + 158] , ax    ; Right border (79*2 = 158)
    add bx, 160               ; Go to next row (80 cols * 2 bytes)
    loop left_right_loop

    ret
		
handling_walls_collision:
    
    cmp byte [rno], 0 ;top row
    je .collision_detected

    cmp byte [rno], 24 ;bottom row
    je .collision_detected

    cmp byte [cno], 0 ;left col
    je .collision_detected

    cmp byte [cno], 79 ;rigth col
    je .collision_detected

    ; No collision
    ret

.collision_detected:
    mov byte [game_over], 1
    call display_game_over
    jmp exit_2
	
process_input:

	cmp byte [game_over], 1
    je game_over_routine
    ; Small delay to control snake speed
    mov ah, 86h         ; BIOS wait function
    mov cx, 0           ; High 16 bits of delay (very short delay)
    mov dx, 50000       ; Adjust this value for speed
    int 15h             ; Wait

    ; Check for key press (non-blocking)
    mov ah, 01h
    int 16h
    jz no_input_3

    ; Key is available
    mov ah, 00h
    int 16h

    cmp ah, 48h        ; Up arrow
    je check_up
    
    cmp ah, 50h        ; Down arrow
    je check_down
    
    cmp ah, 4Bh        ; Left arrow
    je check_left
    
    cmp ah, 4Dh        ; Right arrow
    je check_right

    cmp al, 'e'
    je exit

    cmp al, 'r'
    je restart

no_input_3:

    ; Call movement based on current direction
    mov al, [snake_direction]

    cmp al, 'U'
    je up
    cmp al, 'D'
    je down
    cmp al, 'L'
    je left
    cmp al, 'R'
    je right

    jmp process_input

    
check_up:
    cmp byte [snake_direction], 'D'  
    je no_input_3
    jmp up

check_down:
    cmp byte [snake_direction], 'U' 
    je no_input_3
    jmp down

check_left:
    cmp byte [snake_direction], 'R'  
    je no_input_3
    jmp left

check_right:
    cmp byte [snake_direction], 'L'  
    je no_input_3
    jmp right
	
game_over_routine:
    call display_game_over

.wait_for_restart_or_exit:
    mov ah, 01h
    int 16h
    jz .wait_for_restart_or_exit  ; No key? Wait again

    mov ah, 00h
    int 16h

    cmp al, 'r'
    je restart            ; Restart game

    cmp al, 'e'
    je exit             ; Exit game

    jmp .wait_for_restart_or_exit
 

no_input_2:
    jmp process_input       ; Repeat input check
			

up:
    mov byte [snake_direction], 'U'

	call clear_head
	
	call update_positions
	call clear_body
    dec byte [rno]
    call draw_head
	call draw_body
	call collision_with_body
	call handling_walls_collision
	cmp byte [game_over], 1
    je process_input
	call food_collision
    jmp process_input
	
down:
	mov byte [snake_direction],'D'
	
	call clear_head
	
	call update_positions
	call clear_body
	inc byte [rno]
	call draw_head
	call draw_body
	call collision_with_body
	call handling_walls_collision
	cmp byte [game_over], 1
    je process_input
	call food_collision
	jmp process_input 
	
	
left:
	mov byte [snake_direction],'L'
	
	call clear_head
	
	call update_positions
	call clear_body
	dec byte[cno]
	call draw_head
	
	call draw_body
	call collision_with_body
	call handling_walls_collision
	cmp byte [game_over], 1
    je process_input
	call food_collision
	jmp process_input 
	
right: 
	mov byte [snake_direction],'R'
	
	call clear_head
	
	call update_positions
	call clear_body
	inc byte[cno]
	call draw_head
	
	call draw_body
	call collision_with_body
	call handling_walls_collision
	cmp byte [game_over], 1
    je process_input
	call food_collision
	jmp process_input 
	
exit:
	mov dx, exit_msg
    mov ah, 09h
    int 21h
	mov ax,0x4c00
	int 21h
	
exit_2:
	;mov ax,0x4c00
	;int 21h
	
	mov di, (12*80 + 28) * 2  ; Row 11, Col 28
    mov si, restart_msg
    mov cx, 21               ; Adjust to your message length
write_text_3:
    mov al, [si]
    mov ah, 0x0F
    mov [es:di], ax
    inc si
    add di, 2
    loop write_text_3

	mov di, (13*80 + 28) * 2  ; Row 11, Col 28
    mov si, exit_msg_2
    mov cx, 15              ; Adjust to your message length
write_text_4:
    mov al, [si]
    mov ah, 0x0F
    mov [es:di], ax
    inc si
    add di, 2
    loop write_text_4
	
no_input:
    jmp process_input
	
restart:
    ; Clear game over flag
    mov byte [game_over], 0

    ; Reset snake direction and length
    mov byte [snake_direction], 'R'
    mov word [snake_length], 3

    ; Reset score
    mov word [score], 0

    ; Reset body index
    mov word [body_index], 0

    ; Clear body positions
    mov cx, 100 * 2
    mov di, body_positions
    xor ax, ax
    rep stosb

    ; Set initial head position
    mov byte [rno], 12     ; Middle row
    mov byte [cno], 40     ; Middle column
	jmp start
    ret

	
start:
	; Set up segment register for video memory

	mov ax, cs       ; Set DS = CS
    mov ds, ax

    ; Place head at starting position: row 12, column 40
    mov byte [rno], 12     
    mov byte [cno], 40     

    ; Initialize the head first
    mov ax, [rno]         ; Y (row)
    mov bx, [cno]         ; X (column)
    mov di, 0             ; Start index in body_positions

    ; Store head position at the beginning of the body_positions array
    mov [body_positions + di], bx     ; Store X at body_positions[0]
    mov [body_positions + di + 1], ax ; Store Y at body_positions[1]

    ; Initialize remaining body segments (tail first)
    mov cx, [snake_length] 
    dec cx                 ; Skip the head, start initializing body
    jz done_init          

    mov si, 2             ; Start from the second position in the body
    mov bx, 39            ;body_x 1 step behind head
    mov ax, 12            ;body_y same as head

init_body_loop:
    mov [body_positions + si], bx    
    mov [body_positions + si + 1], ax 
    add si, 2              
    dec cx
    jnz init_body_loop

done_init:
    ; Set body_index to the next available slot
    mov [body_index], si
	
	call clearscr
	
	call displaying_score
	call draw_walls
	;call collision_with_walls
	call handling_walls_collision
	call fruits_logic
	call draw_head
	call draw_body
	call process_input