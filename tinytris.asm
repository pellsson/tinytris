;
; ## Registers ##
;
; bx = board
; cl = piece-x
; si = piece_ptr into board
; dx = piece
; di = always zero
;
; ax, bp = clobber (hehe sometimes at least)
;
bits 16
%ifndef BOOT_SECTOR
org 0x100
%else
org 0x7c00
%endif

%define RANDOM_USE_PIT
%define SPEED 8
%define AUTO_FALL

%define SCREEN_WIDTH 80
%define SCREEN_HEIGHT 25
%define BOARD_INNER_WIDTH 10
%define BOARD_WIDTH (BOARD_INNER_WIDTH + 2)
%define BOARD_HEIGHT SCREEN_HEIGHT
%define BOARD_BYTES (BOARD_HEIGHT * 2)

%macro init_board 0
	mov bx, sp
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
	mov cl, (BOARD_HEIGHT - 1)
@@next_row:
	mov dx, 0x1000
@@next_col
	test dx, word [bx]
	mov ah, 0x44
	jnz @@draw
	mov ah, 0x77
@@draw:
	stosw
	stosw
	shr dx, 1
	jnz @@next_col
	add di, ((SCREEN_WIDTH * 2) - ((BOARD_WIDTH + 1) * 4))
	inc bx
	inc bx

	dec cx
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
	mov bp, 0x0003
@@next:
	mov cx, bp

	push dx		; ABCD    ...A
	and dx, si	; EFGH -> ...E
	shr dx, cl	; IJKL -> ...I
	mov ax, dx	; MNOP    ...M

	mov cx, 3
@@solve:
	shl dx, 5
	or ax, dx
	dec cx
	jnz @@solve

	shr ax, 0x0C
	imul cx, bp, 0x04
	shl ax, cl

	or di, ax ; ax = rotated

	pop dx
	shr si, 1
	dec bp
	jnb @@next
	mov [bx-12], di ;; popa.dx = di
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
	cmp si, bx
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
%ifdef RANDOM_USE_PIT
	in al, 0x40
%else
	rdtsc
%endif
	aam 0x07
	shl al, 1
	movzx bp, al
	mov dx, word [bp + pieces]
	mov cl, 4
	mov si, bx
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
	add ch, SPEED
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
	jz short move_left
	dec al
	jz short do_rotate
	dec al
	jnz short undo_move
	dec cx
	jmp short move_collision_test

do_rotate:
	rotate_piece
	dec cx ; negate the inc cx below (instead of jmp)
move_left:
	inc cx
move_collision_test:
	piece_operation_test
	jnz short undo_move
	pop ax
	pop ax
	ret
undo_move:
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

