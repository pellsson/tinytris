;
; ## Assumptions ##
; - text-mode (0xb8000) is active @ entry-point (ax=1; int 0x13)
; - es=ss=ds @ entry-point
;
; ## Registers ##
;
; bp = board
; cl = piece-x
; si = piece_ptr into board
; dx = piece
; bx = patch_ptr
;
; ax, di = clobber (hehe sometimes at least)
;
bits 16
%ifndef BOOT_SECTOR
org 0x100
%else
org 0x7c00
%endif

%define SCREEN_WIDTH 80
%define SCREEN_HEIGHT 25
%define BOARD_INNER_WIDTH 10
%define BOARD_WIDTH (BOARD_INNER_WIDTH + 2)
%define BOARD_HEIGHT SCREEN_HEIGHT
%define BOARD_BYTES (BOARD_HEIGHT * 2)

%macro init_board 0
	sub sp, BOARD_BYTES
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
	mov si, bp
	xor di, di
	mov bl, BOARD_HEIGHT
@@next_row:
	mov dx, 0x1000
@@next_col
	test dx, word [si]
	mov ah, 0x44
	jnz @@draw
	mov ah, 0x77
@@draw:
	stosw
	stosw
	shr dx, 1
	jnz @@next_col
	add di, ((SCREEN_WIDTH * 2) - ((BOARD_WIDTH + 1) * 4))
	lodsw ; add si, 2
	dec bl
	jnz @@next_row
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
	xor bp, bp
	mov si, 0x8888
	mov cl, 0x03
	mov al, 0x0c
@@next:
	push dx		; ABCD    ...A
	and dx, si	; EFGH -> ...E
	shr dx, cl	; IJKL -> ...I
	mov di, dx	; MNOP    ...M

	push cx
	mov cl, 15
@@solve:
	mov bx, dx
	shl bx, cl
	or di, bx
	sub cl, 5
	jnz @@solve

	pop cx
	;
	; TODO : This feels dumb, but i can no longer understand
	;        what it does :) #too-old
	;
	shr di, 12
	xchg ax, cx
	shl di, cl
	xchg ax, cx
	sub al, 4

	or bp, di ; bp = rotated

	pop dx
	shr si, 1
	dec cl
	jnb @@next
	pop cx
	push bp
	popa
	xchg di, dx
%endmacro

%macro remove_lines 0
	push ds
	pop es
@@next:
	push si
	cmp word [si], 0x1FFF
	ja @@done
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
	movzx di, al
	mov dx, [pieces + di]
	mov cl, 4
	mov si, bp
%endmacro

; 09 14 - or word [si], dx
; 31 14 - xor word [si], dx
; 85 14 - test word [si], dx
%macro piece_operation_merge 0
	mov byte [bx], 0x09
	call piece_operation
%endmacro

%macro piece_operation_remove 0
	mov byte [bx], 0x31
	call piece_operation
%endmacro

%macro piece_operation_test 0
	call piece_operation
%endmacro

%if 1
start:
	cld
	init_board
	mov bx, piece_mode
get_next_piece:
	remove_lines
	next_piece
redraw:
	piece_operation_test
	pushf
	jz @@no_collide
	dec si
	dec si
@@no_collide:
	piece_operation_merge ; SIZE: can reduce 3 bytes here, just change piece_mode once
	draw_board
	popf
	jnz get_next_piece
	piece_operation_remove
	lodsw

	call run
	jmp short redraw

run:
	;
	; Read keyboard
	;
	;
	; Non-blocking keyboard:
	;
	; mov ah, 1
	; int 0x16
	; jz redraw
	;
	; Read keyboard (blocking)
	;
	push cx ; old X-position
	push dx ; old rotation
	xor ah, ah
	int 0x16
	sub al, 'j'
	jz short @@move_left
	dec al
	jz short @@do_rotate
	dec al
	jnz short @@undo_move
	dec cx
	jmp @@move_collision_test
@@do_rotate:
	rotate_piece
	jmp @@move_collision_test
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
	pusha
	xor di, di
@next:
	push ax
	mov ax, dx
	and ax, 0x0f
	shl ax, cl
piece_mode:
	and ax, word [si]
	or di, ax
	pop ax
	lodsw
	shr dx, 4
	jnz @next
	or di, di
	popa
	mov byte [bx], 0x23 ; Enter test-mode
	ret

pieces:
	dw 0x2222 ; I (2)
	dw 0x4460 ; L (4)
	dw 0x2260 ; J (4)
	dw 0x06C0 ; S (2)
	dw 0x0C60 ; Z (2)
	dw 0x0660 ; O (0)
	dw 0x04E0 ; T (4)

clobber_mem:

%ifdef BOOT_SECTOR
times 510-$+$$ db 0xCC
db 0x55, 0xAA
%endif

