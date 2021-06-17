
;*******************************************************
;
;	Floppy16.inc
;		Floppy drive interface routines
;
;	OS Development Series
;*******************************************************

%ifndef __FLOPPY16_INC_67343546FDCC56AAB872_INCLUDED__
%define __FLOPPY16_INC_67343546FDCC56AAB872_INCLUDED__

bits	16

OEMLabel		db "MIKEBOOT"	; Disk label
BytesPerSector		dw 512		; Bytes per sector
SectorsPerCluster	db 1		; Sectors per cluster
ReservedForBoot		dw 2		; Reserved sectors for boot record
NumberOfFats		db 2		; Number of copies of the FAT
RootDirEntries		dw 224		; Number of entries in root dir
					; (224 * 32 = 7168 = 14 sectors to read)
LogicalSectors		dw 5000		; Number of logical sectors
MediumByte		db 0F0h		; Medium descriptor byte
SectorsPerFat		dw 9		; Sectors per FAT
SectorsPerTrack		dw 18		; Sectors per track (36/cylinder)
Sides			dw 2		; Number of sides/heads
HiddenSectors		dd 0		; Number of hidden sectors
LargeSectors		dd 0		; Number of LBA sectors
DriveNo			dw 0		; Drive No: 0
Signature		db 0x29		; Drive signature: 41 for floppy
VolumeID		dd 00000000h	; Volume ID: any number
VolumeLabel		db "MIKEOS     "; Volume Label: any 11 chars
FileSystem		db "FAT12   "	; File system type: don't change!

datasector  dw 0x0000
cluster     dw 0x0000

absoluteSector db 0x00
absoluteHead   db 0x00
absoluteTrack  db 0x00

bootdev     db 0x00

disk_error	db "Floppy error! Press any key...", 0

;************************************************;
; Read drive parameters
; DL = drive number
; ES:DI = xxxxxxxxxxx ;DO NOT USE
;
; DH <= heads-1
; CX <= cylinders-1
; AL <= sectors per tracks
;************************************************;

ReadParams:
	push 	ax
	mov 	ax, 0
	mov	es, ax
	pop 	ax

	mov	ah, 0x8
	mov 	di, 0

	int 	0x13

	pusha
	jc 	fatal_disk_error
	popa

	;extacrt cylinders
	push 	cx		;save value of cx
	shl 	cx, 5		;shift bits left by 5
	shr 	cx, 5		;shift bits right by 5
				;same as deleting top 5 bits

	;extact sector per track
	pop 	ax
	shr 	ax, 10		;shift bits right by 10
				;equal to the top 5 bits

	ret


;************************************************;
; write drive parameters to table
; DH = heads-1
; CX = cylinders-1
; AL = sectors per tracks
;************************************************;
SaveParams:
	mov 	dh, dl
	xor	dh, dh
	inc 	dx
	mov	WORD [Sides], dx

	dec	ax
	mov 	WORD [SectorsPerTrack], ax


;************************************************;
; Convert CHS to LBA
; LBA = (cluster - 2) * sectors per cluster
;************************************************;

ClusterLBA:
          sub     ax, 0x0002                          ; zero base cluster number
          xor     cx, cx
          mov     cl, 1				      ; convert byte to word
          mul     cx
          add     ax, WORD [datasector]               ; base data sector
          ret
     
;************************************************;
; Convert LBA to CHS
; AX=>LBA Address to convert
;
; absolute sector = (logical sector / sectors per track) + 1
; absolute head   = (logical sector / sectors per track) MOD number of heads
; absolute track  = logical sector / (sectors per track * number of heads)
;
;************************************************;

LBACHS:
          xor     dx, dx                              ; prepare dx:ax for operation
          div     WORD [SectorsPerTrack]	      ; calculate
          inc     dl                                  ; adjust for sector 0
          mov     BYTE [absoluteSector], dl
          xor     dx, dx                              ; prepare dx:ax for operation
          div     WORD [Sides]	  		      ; calculate
          mov     BYTE [absoluteHead], dl
          mov     BYTE [absoluteTrack], al
          ret
;***********************************************;

fatal_disk_error:
	mov 	ah, 0x1
	mov	dl, [bootdev]
	int 	0x13
	mov 	ch, ah
	push	cx

	cmp	ch, 0
	je	.exit

	mov si, disk_error		; If not, print error message and reboot
	call print_string

	pop cx

	xor	bx, bx		; A faster method of clearing BX to 0
	mov	ah, 0x0e
	mov 	al, ch
	add 	al, 49		
	int	0x10

	jmp reboot			; Fatal double error
.exit:
	ret

;***********************************************;

reset_drive:
	mov		ah, 0		; reset floppy disk function
	mov		dl, [bootdev]	; drive 0 is floppy drive
	int		0x13		; call BIOS
	;jc		.check		; If Carry Flag (CF) is set, there was an error. Try resetting again
	ret
.check:
	mov 	ah, 0x1
	mov	dl, [bootdev]
	int 	0x13
	mov 	ch, ah
	push	cx

	cmp	ch, 0
	je	.exit
	jmp 	fatal_disk_error
.exit:
	ret

;***********************************************;

read_sector:
	;ch --> track/cylinder
	;dh --> head
	;cl --> sector
	;will be written in es:bx

	mov 	al, 1	       ; al = amount of sectors to read
	mov 	ah, 2          ; ah = 2: read from drive
	mov 	dl, [bootdev]  ;drive number
	int 	0x13   		   ; => ah = status, al = amount read
	
	jc	.check_fatal
	jmp 	.done

.check_fatal:
	call fatal_disk_error
.done:
	ret

;***********************************************;

read_LBA:
	;ax --> sector
	;will be written in es:bx
	call LBACHS	;convert to CHS

	mov 	ch, byte [absoluteTrack] 
	mov 	dh, byte [absoluteHead]
	mov 	cl, byte [absoluteSector]
	
	call read_sector

	ret

;************************************************;
; Reads a series of sectors
; CX=>Number of sectors to read
; AX=>Starting sector
; ES:EBX=>Buffer to read to
;************************************************;

ReadSectors:
   .loop_read:
	pusha
	call	read_LBA
	popa

	add 	bx, [BytesPerSector]	;buffer for next sector
	inc	ax

	dec	cx
	jnz 	.loop_read

	ret

;************************************************;	

reboot:
	mov ax, 0
	int 16h				; Wait for keystroke
	mov ax, 0
	int 19h				; Reboot the system

;************************************************;

print_string:				; Output string in SI to screen
	pusha

	mov ah, 0Eh			; int 10h teletype function

.repeat:
	lodsb				; Get char from string
	cmp al, 0
	je .done			; If char is zero, end of string
	int 10h				; Otherwise, print it
	jmp short .repeat

.done:
	popa
	ret

%endif		;__FLOPPY16_INC_67343546FDCC56AAB872_INCLUDED__
