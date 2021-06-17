; ==================================================================
; The Mike Operating System bootloader
; Copyright (C) 2006 - 2019 MikeOS Developers -- see doc/LICENSE.TXT
;
; Based on a free boot loader by E Dehling. It scans the FAT12
; floppy for KERNEL.BIN (the MikeOS kernel), loads it and executes it.
; This must grow no larger than 512 bytes (one sector), with the final
; two bytes being the boot signature (AA55h). Note that in FAT12,
; a cluster is the same as a sector: 512 bytes.
; ==================================================================


	BITS 16

	jmp bootloader_start	; Jump past disk description section


; ------------------------------------------------------------------
; Disk description table, to make it a valid floppy
; Note: some of these values are hard-coded in the source!
; Values are those used by IBM for 1.44 MB, 3.5" diskette

OEMLabel		db "MIKEBOOT"	; Disk label
BytesPerSector		dw 512		; Bytes per sector
SectorsPerCluster	db 1		; Sectors per cluster
ReservedForBoot		dw 2		; Reserved sectors for boot record
NumberOfFats		db 2		; Number of copies of the FAT
RootDirEntries		dw 224		; Number of entries in root dir
					; (224 * 32 = 7168 = 14 sectors to read)
LogicalSectors		dw 2880		; Number of logical sectors
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

read_sector:
	;ch --> track/cylinder
	;dh --> head
	;cl --> sector
	;will be written in es:bx

	mov 	al, 1	       ; al = amount of sectors to read
	;mov 	ch, 0          ; cylinder/track = 0
	;mov 	dh, 0          ; head           = 0
	;mov 	cl, 2          ; sector         = 2
	mov 	ah, 2          ; ah = 2: read from drive
	mov 	dl, [bootdev]  ;drive number
	int 	0x13   		   ; => ah = status, al = amount read
	
	jc	.check_fatal
	jmp 	.done

.check_fatal:
	call fatal_disk_error
.done:
	ret

read_LBA:
	;ax --> sector
	;will be written in es:bx
	call LBACHS	;convert to CHS

	mov 	ch, byte [absoluteTrack] 
	mov 	dh, byte [absoluteHead]
	mov 	cl, byte [absoluteSector]
	
	call read_sector

	ret

; ------------------------------------------------------------------
; Main bootloader code

bootloader_start:
	cli
        mov     ax, 0x0000				; set the stack
        mov     ss, ax
        mov     sp, 0xFFFF

        mov     ax, 0x07C0				; setup registers to point to our segment
        mov     ds, ax
        mov     es, ax
        mov     fs, ax
        mov     gs, ax
	sti

	; NOTE: A few early BIOSes are reported to improperly set DL

	cmp dl, 0
	je no_change
	mov [bootdev], dl		; Save boot device number
	mov ah, 8			; Get drive parameters
	int 13h
	jc fatal_disk_error
	and cx, 3Fh			; Maximum sector number
	mov [SectorsPerTrack], cx	; Sector numbers start at 1
	movzx dx, dh			; Maximum head number
	add dx, 1			; Head numbers start at 0 - add 1 for total
	mov [Sides], dx

no_change:
	mov eax, 0			; Needed for some older BIOSes

	;find end of stage one to place there the rest
	mov ax, rest_of_file
	add ax, 0x7c00
	push ax		;store ax

	mov	si, msgLoading 	
	call 	print_string

	call 	reset_drive

	mov 	si, msgDone
	call 	print_string

	pop 	bx

	;call 	read
	mov 	ax, 1
	mov	bx, 0x200	;	0x7c00:0x200
	call 	read_LBA

	mov 	si, msgDone
	call 	print_string

	jmp 	rest_of_file

	cli
	hlt


; ------------------------------------------------------------------
; BOOTLOADER SUBROUTINES

reboot:
	mov ax, 0
	int 16h				; Wait for keystroke
	mov ax, 0
	int 19h				; Reboot the system


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

fatal_stage2_not_found:
	mov 	si, stage2_error
	call 	print_string

	jmp reboot

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

read:
	mov al, 1 	   ; al = amount of sectors to read
	mov ch, 0          ; cylinder/track = 0
	mov dh, 0          ; head           = 0
	mov cl, 2          ; sector         = 2
	mov ah, 2          ; ah = 2: read from drive
	int 0x13   		   ; => ah = status, al = amount read

	ret

; ------------------------------------------------------------------
; STRINGS AND VARIABLES

	

     	absoluteSector db 0x00
     	absoluteHead   db 0x00
     	absoluteTrack  db 0x00

	kern_filename	db "KERNEL  BIN"	; MikeOS kernel filename
	msgProgress 	db ".", 0
	msgLoading 	db "Loading", 0
	msgDone		db "Done", 0
	msgLBA		db "LBA", 0
	msgRead		db "Read", 0

	disk_error	db "Floppy error! Press any key...", 0

	bootdev		db 0 	; Boot device number


; ------------------------------------------------------------------
; END OF BOOT SECTOR AND BUFFER START

	times 510-($-$$) db 0	; Pad remainder of boot sector with zeros
	dw 0AA55h		; Boot signature (DO NOT CHANGE!)

; ==================================================================

rest_of_file:
LOAD_ROOT:
	;get start sector of root
	
	xor 	ax, ax 			;clear ax
	xor 	dx, dx			;clear dx
	mov 	al, [NumberOfFats]	;number of fats * size of fat = size of fats
	mul	WORD [SectorsPerFat]	;now ax stores the size of both fats
	add 	ax, WORD [ReservedForBoot]	;add reserved sectors so now ax is the locations of root

	;we need to save where our data starts
	mov 	WORD [datasector], ax
	add 	WORD [datasector], 14	;add size of root

	;loop and read root into 0x7c00:0400 (0x8000)
	;right after the code
	mov bx, 0x0400
	mov cx, 14	;root dir is always 14 sectors

.LOOP_read:
	pusha
	call 	read_LBA
	popa

	add	bx, 0x200 	;location for next sector
	inc	ax		;nex sector
	dec 	cx		;loop if not all read
	jnz	.LOOP_read
	
	; now we got out root in 0x0000:8000 we need to find our stage2

     	; browse root directory for binary image
        mov     cx, WORD [RootDirEntries]             ; load loop counter
        mov     di, 0x0200	;remember that we load the root at 0x07c0:0x0200                            ; locate first root entry

.LOOP_search:
        push    cx
        mov     cx, 0x000B                            ; eleven character name
        mov     si, ImageName                         ; image name to find
        push    di
    rep  cmpsb                                         ; test for entry match
        pop     di
        je      LOAD_FAT
        pop     cx
        add     di, 0x0020                            ; queue next directory entry
        loop    .LOOP_search
        jmp     fatal_stage2_not_found

LOAD_FAT:
	;save first cluster
        mov     si, msgCRLF
        call    print_string
        mov     dx, WORD [di + 0x001A]
        mov     WORD [cluster], dx                  ; file's first cluster
	mov 	ax, WORD [cluster]         
 
	;find size of FATs
	xor 	ax, ax
	mov 	al, BYTE [NumberOfFats]
	mul 	WORD [SectorsPerFat]
	mov 	cx, ax		;now cx = sectors used by both FATs

	;find location of FATs
	mov 	ax, WORD [ReservedForBoot]	;ax = location of FATs first sector

	;read Fats into memory
	mov	bx, 0x0400	;write it after the code

.FAT_read:
	pusha
	call 	read_LBA
	popa

	add	bx, 0x200 	;location for next sector
	inc	ax		;next sector
	dec 	cx		;loop if not all read
	jnz	.FAT_read
	
	; now we got our Fats in memory
	; read image into 0x0000:0x0500
	mov     si, msgCRLF
        call    print_string

	mov 	ax, 0x0050
	mov 	es, ax
	mov 	bx, 0x0000	;destination for image (0x0050:0x0000)
	push 	bx

	;load stage2
LOAD_IMAGE:
	mov	ax, WORD [cluster]
	call	ClusterLBA
	pop 	bx	
	xor 	cx, cx
	mov 	cl, BYTE [SectorsPerCluster]

.image_read:
	pusha
	call 	read_LBA
	popa

	add	bx, 0x200 	;location for next sector
	inc	ax		;next sector
	dec 	cx		;loop if not all read
	jnz	.image_read

	;cluster was read
	push	bx

	;compute next cluster
	mov 	ax, WORD [cluster]
	mov 	cx, ax
	mov	dx, ax
        shr     dx, 0x0001                          ; divide by two
        add     cx, dx                              ; sum for (3/2)
        mov     bx, 0x0400                          ; location of FAT in memory
        add     bx, cx                              ; index into FAT
        mov     dx, WORD [bx]                       ; read two bytes from FAT
        test    ax, 0x0001
        jnz     .ODD_CLUSTER
          
.EVEN_CLUSTER:
     
        and     dx, 0000111111111111b               ; take low twelve bits
        jmp     .DONE
         
.ODD_CLUSTER:
     
        shr     dx, 0x0004                          ; take high twelve bits
          
.DONE:
     
        mov     WORD [cluster], dx                  ; store new cluster
        cmp     dx, 0x0FF0                          ; test for end of file
        jb      LOAD_IMAGE
          
DONE:
     	;now all stage 2 should be loaded at 0x0050:0x0000 (0x0500)
        mov     si, msgCRLF
        call    print_string
	mov	dl, [bootdev]
        push    WORD 0x0050
        push    WORD 0x0000
        retf

        mov     si, ImageName   	
        call    print_string

	cli
	hlt


; ------------------------------------------------------------------
; STRINGS AND VARIABLES

	stage2_error	db "Stage2 not found! Press any key...", 0
	ImageName   	db "KRNLDR  SYS"
     	msgCRLF     	db ".", 0x0D, 0x0A, 0x00

	cluster		dw 0x0000
     	datasector  	dw 0x0000

	succses		db "YAY!", 0

times 1024-($-$$) db 0	; Pad remainder of boot sector with zeros














