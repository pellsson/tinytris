;
; bp = board
; bl = piece-x
; si = piece_ptr into board
; dx = piece
;
; ax, cx, di clobber
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
	push 0xb800
	pop es
	pusha
	mov si, bp
	xor di, di
	mov bh, BOARD_HEIGHT
@@next_row:
	mov cx, 0x1000
@@next_col
	test cx, word [si]
	mov ah, 0x44
	jnz @@draw
	mov ah, 0x77
@@draw:
	stosw
	stosw
	shr cx, 1
	jnz @@next_col
	add di, ((SCREEN_WIDTH * 2) - ((BOARD_WIDTH + 1) * 4))
	lodsw ; add si, 2
	dec bh
	jnz @@next_row
	popa
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
	dec si
	dec si
@@more:
	std
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
	mov bl, 4
	mov si, bp
%endmacro

; 09 14 - or word [si], dx
; 31 14 - xor word [si], dx
; 85 14 - test word [si], dx
%macro piece_operation_merge 0
	mov byte [piece_mode], 0x09
	call piece_operation
%endmacro

%macro piece_operation_remove 0
	mov byte [piece_mode], 0x31
	call piece_operation
%endmacro

%macro piece_operation_test 0
	call piece_operation
%endmacro

%if 1
start:
	cld
	init_board

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
	piece_operation_merge
	draw_board
	popf
	jnz get_next_piece
	piece_operation_remove
	lodsw
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
	mov di, bx ; new X-position
	xor ah, ah
	int 0x16
	sub al, 'j'
	jz @@move_left
	dec al
	jz short rotate
	dec al
	jnz short redraw
	dec bx
	jmp @@move_collision_test
@@move_left:
	inc bx
@@move_collision_test:
	piece_operation_test
	jz short redraw
	mov bx, di ; undo move
	jmp short redraw
%endif

;
; ABCD	MIEA	PONM	DHLP
; EFGH	NJFB	LKJI	BGKO
; IJKL	OKGC	HGFE	CFJN
; MNOP	PLHD	DBCA	AEIM
;
rotate:
%if 1
	pusha

	xor bp, bp
	mov si, 0x8888
	mov ch, 12
	mov cl, 3
@@next:
	push dx
	and dx, si
	shr dx, cl
	mov di, dx

	push cx
	mov cl, 15
@@solve:
	mov bx, dx
	shl bx, cl
	or di, bx
	sub cl, 5
	jnz @@solve

	pop cx

	shr di, 12
	xchg ch, cl
	shl di, cl
	sub cl, 4
	xchg ch, cl

	or bp, di ; bp = rotated

	pop dx
	shr si, 1
	dec cl
	jnb @@next

	pop cx
	push bp ; change di on in pusha
	popa

	xchg dx, di
	piece_operation_test
	jz @@can_rotate
	xchg dx, di
@@can_rotate:
%endif
	jmp redraw


piece_operation: ; cant touch di
	pusha
	xor ax, ax
	mov cl, 12
__next:
	pusha
	shr dx, cl
	and dx, 0x0f
	mov cl, bl
	shl dx, cl
	; 09 14 - or word [si], dx
	; 31 14 - xor word [si], dx
	; 85 14 - test word [si], dx
piece_mode:
	test word [si], dx
	popa
	jz @@z
	inc ax
@@z:
	inc si ; cant lodsw here or al is nuked...
	inc si
	sub cl, 4
	jns __next
	or ax, ax
	popa
	mov byte [piece_mode], 0x85 ; Enter test-mode
	ret

pieces:
	dw 0x2222 ; I (2)
	dw 0x4460 ; L (4)
	dw 0x2260 ; J (4)
	dw 0x06C0 ; S (2)
	dw 0x0C60 ; Z (2)
	dw 0x0660 ; O (0)
	dw 0x04E0 ; T (4)

%ifdef BOOT_SECTOR
times 510-$+$$ db 0xCC
db 0x55, 0xAA
%endif

