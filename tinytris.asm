;
; ## Assumptions ##
; - text-mode (0xb8000) is active @ entry-point (ax=1; int 0x13)
; - es=ss=ds @ entry-point
; - sp has BOARD_BYTES accessible bytes @ entry-point
;
; ## Registers ##
;
; bp = board
; cl = piece-x
; si = piece_ptr into board
; dx = piece
; di = always zero
;
; ax, bx = clobber (hehe sometimes at least)
;
bits 16
%ifndef BOOT_SECTOR
org 0x100
%else
org 0x7c00
%endif

%define SPEED 8
%define AUTO_FALL

%define SCREEN_WIDTH 80
%define SCREEN_HEIGHT 25
%define BOARD_INNER_WIDTH 10
%define BOARD_WIDTH (BOARD_INNER_WIDTH + 2)
%define BOARD_HEIGHT SCREEN_HEIGHT
%define BOARD_BYTES (BOARD_HEIGHT * 2)

%macro init_board 0
	;sub sp, BOARD_BYTES
	mov bp, sp
	mov ax, 0x1003
	mov cx, BOARD_HEIGHT - 1
	mov di, sp
	repne stosw
	dec cx
	or word [di], cx
%endmacro

%macro draw_board 0
%if 1
	push 0xb800
	pop es
	pusha
	mov bl, (BOARD_HEIGHT - 1)
@@next_row:
	mov dx, 0x1000
@@next_col
	test dx, word [bp]
	mov ah, 0x44
	jnz @@draw
	mov ah, 0x77
@@draw:
	stosw
	stosw
	shr dx, 1
	jnz @@next_col
	add di, ((SCREEN_WIDTH * 2) - ((BOARD_WIDTH + 1) * 4))
	inc bp
	inc bp
	;
	; We only initialize bl, but because bx points to the piece
	; data, bh high bit is never set. dec bx will work and saves
	; a byte over bl.
	;
	dec bx
	jnb @@next_row
	popa
%endif
%endmacro

;
; ABCD    MIEA    PONM    DHLP
; EFGH    NJFB    LKJI    BGKO
; IJKL    OKGC    HGFE    CFJN
; MNOP    PLHD    DBCA    AEIM
;
%macro rotate_piece 0
	pusha
	mov si, 0x8888
	mov cx, 0x0c03 ; ch al ah ax
@@next:
	push dx		; ABCD    ...A
	and dx, si	; EFGH -> ...E
	shr dx, cl	; IJKL -> ...I
	mov ax, dx	; MNOP    ...M

	push cx
	mov cl, 15
@@solve:
	mov bx, dx
	shl bx, cl
	or ax, bx
	sub cl, 5
	jnz @@solve

	shr ax, 0x0C
	mov cl, ch
	shl ax, cl
	pop cx

	or di, ax ; ax = rotated

	pop dx
	shr si, 1
	dec cx
	sub ch, 0x04
	jnb @@next
	mov [bp-12], di ;; popa.dx = di
	popa
%endmacro

%macro remove_lines 0
	push ds
	pop es
@@next:
	cmp word [si], 0x1FFF
	ja @@done
	push si
	jnz @@next_row
	mov di, si
	std
	lodsw ; sub si, 2
@@more:
	movsw
	cmp si, bp
	jne @@more
@@next_row:
	pop si
	cld
	lodsw ; si+=2
	jmp @@next
@@done:
	; nothing
%endmacro

%macro next_piece 0
@@invalid:
	in al, 0x40
	cmp al, 7
	jnb @@invalid
	shl al, 1
	movzx bx, al
	mov dx, word [bx + pieces]
	mov cl, 4
	mov si, bp
%endmacro

; 09 14 - or word [si], dx
; 23 14 - and word [si], dx
; 31 14 - xor word [si], dx
; 85 14 - test word [si], dx
%macro piece_operation_merge 0
	mov al, 0x09
	call piece_operation
%endmacro

%macro piece_operation_remove 0
	mov al, 0x31
	call piece_operation
%endmacro

%macro piece_operation_test 0
	mov al, 0x23 ; Enter test-mode
	call piece_operation
%endmacro

%if 1
start:
	cld
	init_board
get_next_piece:
	remove_lines
	next_piece
	xor di, di
redraw:
	piece_operation_test
	pushf
	jz @@no_collide
	dec si
	dec si
@@no_collide:
	piece_operation_merge
	draw_board
	popf
	jnz short get_next_piece
	piece_operation_remove
%ifdef AUTO_FALL
	add bl, SPEED
	jnc short @@no_advance
%endif
	lodsw ; move piece down

%ifdef AUTO_FALL
@@no_advance:
	;
	; Read keyboard
	;
	;
	; Non-blocking keyboard:
	;
	mov ah, 1
	int 0x16
	jz short redraw  ; (6 bytes total i think)
%endif
	;
	; Read keyboard (blocking)
	;
	push redraw ; return address

	push cx ; old X-position
	push dx ; old rotation
	mov ax, di
	int 0x16
	sub al, 'j'
	jz short @@move_left
	dec al
	jz short @@do_rotate
	dec al
	jnz short @@undo_move
	dec cx
	jmp short @@move_collision_test
@@do_rotate:
	rotate_piece
	dec cx ; negate the inc cx below (instead of jmp)
@@move_left:
	inc cx
@@move_collision_test:
	piece_operation_test
	jnz short @@undo_move
	pop ax
	pop ax
	ret
@@undo_move:
	pop dx
	pop cx
	ret
%endif

piece_operation:
	mov byte [piece_mode], al ; Enter test-mode
	pusha
@next:
	mov ax, dx
	and ax, 0x0f
	shl ax, cl
piece_mode:
	and word [si], ax
	or di, ax
	lodsw
	shr dx, 4
	jnz @next
	or di, di
	popa
	ret

pieces:
	dw 0x2222 ; I
	dw 0x4460 ; L
	dw 0x2260 ; J
	dw 0x06C0 ; S
	dw 0x0C60 ; Z
	dw 0x0660 ; O
	dw 0x04E0 ; T

clobber_mem:

%ifdef BOOT_SECTOR
times 510-$+$$ db 0xCC
db 0x55, 0xAA
%endif

