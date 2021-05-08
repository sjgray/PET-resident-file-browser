; PET/CBM EDIT ROM - File Browser and Utility  - Started March 25, 2021
; ===========================================  - Updated: 2021-04-04
;
; A File Browser Utility that can be included in Editor ROM or Option ROM.
;
; ITEMS TODO:
; -Directory loading to RAM
; -Directory display
; -Print at function and direct write to screen

;-------------- Includes
; includes from editrom project. Need to fool it, so define a few things first.
; This will all be eliminated when merged with Editrom Project.

COLUMNS		= 80
CODEBASE	= 0
ESCCODES	= 0
BACKARROW	= 0
BACKACTION	= 0

!SOURCE "membasic4.asm"				; BASIC calls
!SOURCE "memchips.asm"				; IO Chips
!SOURCE "memkernal.asm"				; KERNAL calls
!SOURCE "memlow.asm"				; Low Memory locations
!SOURCE "memzeropage.asm"			; Zero Page locations

;-------------- Constants

SCREENPAGES     = 8				; 2K size for 80 col
DIRWIDTH        = 16				; Width of directory window
DIRHEIGHT	= 20				; Height of directory window
DIRROW		= 2				; Row for first directory line
DIRCOL		= 2				; Column for directory listing
IDFLAG		= 0				; Flag to add ID string
MYDRIVE		= 8				; Default Drive Number, ie: 8
DRIVEUNIT	= 0				; Default Unit Numberm ie: 0 or 1

;-------------- Zero Page Locations
; $B1-$C3 used for TAPE. Safe to use here for now?

SCNWID		= $B1				; Current Screen Width 
SCNSAVE1	= $B2				; Cursor Pointer LO
SCNSAVE2	= $B3				; Cursor Pointer HI
SCNSAVE3	= $B4				; Cursor Column/offset
SCNSAVE4	= $B5				; Cursor Row
SCNMEM		= $B6				; Screen Save Buffer Pointer
CMDMEM		= $B7				; LO Command String Buffer Pointer
;		= $B8				; HI

;						Details for LEFT Directory

LDIRMEM		= $B9				; LEFT Directory Buffer Pointer
LDIRTOP		= $BB				; LEFT Index of Top Entry
LDIRSEL		= $BC				; LEFT Index of selected entry
LDIREND		= $BD				; LEFT Index of Last Entry

;						Details for Directory being Browsed

DTOP		= $BE				; Index of Top entry
DSEL		= $BF				; Index of Selected Entry
DEND		= $C0				; Index of Last Entry
DCOUNT		= $C1				; Counter for directory display
DTMP		= $C2				; Temp counter

; $ED-$F7 not used in 80-column machines. Safe to use here for now?

ZP1		= $EE				; Source Pointer
ZP2		= $F0				; Destination Pointer

;-------------- Memory Locations

TAPEBUFFER      = $027A				; Tape buffer

;======================================================================================
; START OF PROGRAM CODE
;======================================================================================

!TO "sb.bin",plain				; Output filename without Load Address
*=$A000						; Put this in the $A Option ROM

Start:
		JSR SaveCursorPos		; Save cursor position
		JSR FindScnWidth		; Determine width of screen		
		JSR InitStuff			; Do some intialization stuff		
		JSR SaveScreen			; Save the screen to memory
		JSR DrawUI			; Draw the interface
		JSR GetDiskStatus		; Get Disk Status 
		JSR LoadLeftDir			; Load the directory
		JSR GetDiskStatus		; Get Disk Status
		JSR AnyKey			; Wait for a key
		JSR RestoreScreen		; Restore the screen
		JSR RestoreCursorPos		; Restore cursor position
BrowseDone:	RTS

;===========================================================================================
; Subroutines - Work in Progress
;===========================================================================================
;======================================================================================
; Init Stuff
;======================================================================================
; Here we look at the BASIC pointers to figure out where we can store our data
; STREND  pointer is the End of Array pointer which is the first address that is free
; FREETOP pointer is the End of RAM and is the last address we can use.

InitStuff:
		LDX #0				; Make our data start on page boundry
		STX SCNMEM			; LO Screen Save Address
		STX LDIRMEM			; LO Directory Storage Address

		LDX STREND+1			; HI End of Arrays
		INX				; Start on boundty of next page
		STX SCNMEM+1			; HI Screen Save address
		TXA
		CLC
		ADC #8				; Reserve 8 pages for 80-col screen
		STA LDIRMEM+1			; HI Directory Start Address
		
		LDA #8				; Default Drive=8
		STA MYDRIVE			; Set it
		RTS

;======================================================================================
; DRAW UI
;======================================================================================
; Draw Titlebar, box for the filenames and key help line

DrawUI:		LDA #<TitleBar				
		LDY #>TitleBar
		JSR STROUTZ			; Print the titlebar

		LDA #<DirBox1
		LDY #>DirBox1
		JSR STROUTZ			; Print top line

		LDA #DIRHEIGHT			; Height of Dir Box
		STA ZP1				; Save it

DUILoop:	LDA #<DirBox2
		LDY #>DirBox2
		JSR STROUTZ			; Print middle line
		DEC ZP1				; Decrement Counter
		BNE DUILoop			; Go back for more

DUIBottom:	LDA #<DirBox3
		LDY #>DirBox3
		JSR STROUTZ			; Print bottom line

DUIBar:		LDA #<KeyBar				
		LDY #>KeyBar
		JSR STROUTZ			; Print key bar line
		RTS

;======================================================================================
; GET USER INPUT
;======================================================================================
; Wait for any key to be pressed 

AnyKey:		JSR GETIN			; Get keystroke to .A
		BEQ AnyKey			; None, so loop back
		RTS

;======================================================================================
; DEBUG STRINGS
;======================================================================================

ShowProg:	LDA #<ProgStart				
		LDY #>ProgStart
		JSR STROUTZ			; Print it
		RTS

ShowMarker:	LDA #<ProgMarker				
		LDY #>ProgMarker
		JSR STROUTZ			; Print it
		RTS

;======================================================================================
; PRINT ROUTINES
;======================================================================================
; Set ZP1 to point to string. ZP2 will be updated print address
; PrintAt: Set .X=Col (0-max), .Y=Row (0-24)
; PrintIt: Prints from current position
; Requires
PrintAt:
		
PrintIt:

;======================================================================================
; DISK ROUTINES
;======================================================================================
;======================================================================================
; SEND DOS COMMAND
;======================================================================================
; Send a string to the command channel.
; ZP1 points to a null terminated string.

send_cmd	LDA MYDRIVE			; was CHRGETX ; Current Device#
		STA FA				; Set Device Number
		LDA #$6f			; DATA SA 15 (command channel)
		STA SA				; Set Secondary Address
		JSR LISTN			; LISTEN 
		LDA SA				; Reload Secondary Address
		JSR SECND			; Set Secondary Address

		LDA CMDMEM			; Buffer start
		STA ZP1				; Pointer
		LDA CMDMEM+1
		STA ZP1+1			; Pointer

SENDCMD2	LDY #0				; always index 0
		LDA (ZP1),Y			; get the character
		BEQ SENDCMDDONE			; if zero then done
		JSR CIOUT			; otherwise, send char to IEEE
		INC ZP1				; point to next character
						; if buffer might cross page this will fail!
		CLV
		BVC SENDCMD2			; loop back for more

SENDCMDDONE	JSR UNLSN			; un-listen
		CLC

;======================================================================================
; GET DISK STATUS
;======================================================================================
; Read the status string: ##,String,TT,SS

GetDiskStatus:	LDA #<GDS1
		LDY #>GDS1
		JSR STROUTZ			; debug

		LDA MYDRIVE			; Current Device#
		STA FA				; Set device number
		JSR TALK			; TALK
		LDA #$6f			; Secondary Address=15
		STA SA				; Store it
		JSR SECND			; Send secondary address

GS_NEXTCHAR	jsr ACPTR			; Read byte from IEEE bus
		cmp #$0D			; Is byte = CR?		
		beq GS_DONE			; yes, jump out
		jsr SCROUT			; no, write char to screen
		jmp GS_NEXTCHAR			; go back for more

GS_DONE		jsr SCROUT			; write <CR> to screen
		jsr UNTLK			; UNTALK
		RTS				; jmp READY ; Back to BASIC

GDS1:		!PET "drive status: ",0

;======================================================================================
; LOAD LEFT/RIGHT DIRECTORY
;======================================================================================
; Sets up pointers for LEFT/RIGHT directory
; !!!! For now they will use the same memory

LoadLeftDir:
		LDA #0				; Counter
		STA LDIRTOP			; Start at Top of List
		STA LDIRSEL			; Selected at top

		LDA LDIRMEM			; LO Directory Storage Address
		STA ZP1				; LO Pointer
		LDA LDIRMEM+1			; HI Directory Storage Address
		STA ZP1+1			; HI Pointer
		JSR LoadDirectory		; Read directory to memory, count entries
		LDA DTMP			; Load # of entries in directory
		STA LDIREND			; Save it for LEFT				; TODO: save the end address here
		RTS

;======================================================================================
; LOAD DIRECTORY
;======================================================================================
; Reads the directory into RAM pointed to by ZP1.
; Counts # of directory entries to DTMP.

;-------------- Prep
LoadDirectory:
		LDA #<FNDirectory		; LO Pointer to Filename ("$0")
		STA FNADR			; LO Filename Address pointer
		LDA #>FNDirectory		; HI
		STA FNADR+1			; HI 
		LDY #2				; Length=2 "$0"
		STY FNLEN			; Save it

		LDA #0				; Clear the status byte
		STA STATUS
		STA DTMP			; Zero the entry counter

		LDA #$60			; DATA SA 0
		STA SA
		JSR OPENI			; open file
		JSR TALK			; talk
		LDA SA				; Secondary Address
		JSR SECND			; Set Secondary Address

SkipLA:		JSR ACPTR			; Read two bytes Load Address
		JSR ACPTR			; and discard

;-------------- Read and Parse the Header

LoadHeader:	LDX STATUS			; Did it read ok?
		BNE StopListing			; No, finish

		JSR DDGetStor			; Discard2/Read/Store - LO Drive#
		JSR GetStor			; Read/Store          - HI Drive#
		JSR GetStor			; Get <RVS>
		JSR GetStor			; Get <QUOTE>

SkipToQ:	JSR GetStor			; Read/Store title character
		CMP #34				; Is it a <QUOTE>?
		BNE SkipToQ			; No, loop back
		JSR DGetStor			; Discard <SPACE> Get first ID
		JMP SkipToEnd			; Discard rest of line

;-------------- Read and parse File Entry or Blocks Free

LoadEntry:	LDX STATUS			; Did it read ok?
		BNE StopListing			; No, finish

		JSR DDGetStor			; Discard 2, Get Blocks LO
		JSR GetStor			; Get Blocks HI

LoadELoop	JSR ACPTR			; Get a byte
		CMP #32				; Is it <SPACE>
		BEQ LoadELoop			; Yes, ignore, then go back for more
		JSR StorIt			; No, must be <QUOTE> or <B>
		CMP #66				; Is it a <B>?
		BEQ BlocksFree			; Yes, parse blocks free
						; No, must be <Quote> to continue
;-------------- Read and parse Filename

LoadFName:	JSR GetStor			; Read/Store
		CMP #34				; Is it a quote?
		BNE LoadFName			; No, go back for more

GetSpaces:	JSR GetStor			; Store it
		CMP #32				; Is it a <SPACE>?
		BEQ GetSpaces			; Yes, get more

SkipToEnd:	JSR ACPTR			; Read 
		CMP #0				; Is it <NULL>?
		BNE SkipToEnd
		JSR StorIt			; Add null to END
		JMP Entry_done			; jump to done

;-------------- Read and parse Blocks Free line

BlocksFree:	JSR GetStor			; Get and store text
		CMP #0				; Is it <NULL>
		BNE BlocksFree			; No, get more
		jmp StopListing		

;-------------- Line is complete

Entry_done:	LDA DTMP			; Get Counter
		CMP #255			; Is it 255?
		BEQ StopListing			; Yes, Maximum entries, so exit
		INC DTMP			; Counter=Counter+1
		JMP LoadEntry			; No, Jump back for more lines

;-------------- Listing is complete

StopListing:	JSR CLSEI			; close file with $E0, unlisten
		LDA #<TEMP2			; debug "Reading:"
		LDY #>TEMP2
		JSR STROUTZ			; debug print it
		RTS

;-------------- Directory Read and Store Routine / ZP1 store
; Four entry points:
; 1: Read two Directory bytes and discard (DD)
; 2: Read one Directory byte  and discard (D)
; 3: Read one Directory byte to .A
; 4: Store .A to ZP1 pointer
; 5: Increment ZP1 Pointer and handle page crossing

DDGetStor:	JSR ACPTR			; Get and ignore
DGetStor:	JSR ACPTR			; Get and ignore
GetStor:	JSR ACPTR			; Get a byte
StorIt:		LDY #0				; Ensure offset is 0
		STA (ZP1),Y			; Store it
IncZP1:		INC ZP1				; Increment pointer LO
		BNE IncDone
		INC ZP1+1			; Increment pointer HI
IncDone:	RTS		

;======================================================================================
; LOAD / RUN
;======================================================================================
; Load a file. Run it too?

loadrun:	lda #0				; Clear status byte
		sta STATUS
		sta VERCK			; LOAD=0, VERIFY=1
		jsr LOADOP			; LOAD without pointer change

		lda STATUS			; Did it load?
		and #$10
		bne loaderr			; No, exit out

		lda EAL+1			; end of program MSB
		sta VARTAB+1			; start of basic variables MSB
		lda EAL				; end of program LSB
		sta VARTAB			; start of basic variables LSB

		jsr CRLF
		jsr RSTXCLR			; reset TXTPTR and perform CLR
		jsr LINKPRG			; rebuild chaining of BASIC lines

		lda SAVELA
		cmp #$2f			; if '/' then load only, omit RUN
		bne startprg			; '^' --> RUN
		jmp READY			; load only, exit with BASIC warm start

startprg	jsr STXTPT			; reset TXTPTR
		jmp NEWSTT			; RUN

loaderr		jmp FILENOTFOUND		; FILE NOT FOUND, return to basic









;**************************************************************************************
; Routines that are working
;**************************************************************************************
;======================================================================================
; FIND SCREEN WIDTH
;======================================================================================
; This routine prints <HOME><HOME><DOWN> using the standard PET print routines.
; It does not disturb the screen. <HOME><HOME> cancels any windowing.
; This will place the cursor on the second line. The screen line pointer will be set
; and the LO byte will be 40 or 80.
;>>>>>>> Will linked lines affect result?

FindScnWidth:	LDA #<HomeHome			; Address of string to print
		LDY #>HomeHome
		JSR STROUTZ			; Use standard system print routine
		LDA ScrPtr			; LO of Start of Line Address
		STA SCNWID			; Save it
		RTS

;======================================================================================
; SAVE AND RESTORE SCREEN
;======================================================================================
; Save/Restore the screen to/from RAM pointed at by ZPSCREEN

SaveScreen:	LDA #<SCREEN_RAM		; LO Screen address
		STA ZP1				; LO Source pointer
		LDA #>SCREEN_RAM		; HI Screen address
		STA ZP1+1			; HI Source pointer
		LDA SCNMEM			; LO Destination address
		STA ZP2				; LO Destination pointer
		LDA SCNMEM+1			; HI Destination address
		STA ZP2+1			; HI Destination pointer
		JMP CopyScreen

RestoreScreen:	LDA SCNMEM			; LO Source address
		STA ZP1				; LO Source pointer
		LDA SCNMEM+1			; HI Source address
		STA ZP1+1			; HI Source pointer
		LDA #<SCREEN_RAM		; LO Destination address
		STA ZP2				; LO Destination pointer
		LDA #>SCREEN_RAM		; HI Destination address
		STA ZP2+1			; HI Destination pointer

;======================================================================================
; COPY SCREEN
;======================================================================================
; ZP1 and ZP2 are set, so set .X and fall into copy routine below

CopyScreen:	LDX #SCREENPAGES		; How many pages to copy? 8=2K

;-------------- BLOCK COPY
; Copies .X pages from source ZP1 to destination ZP2

CopyMem:	LDY #0				; Start at offset 0

CopyLoop:	LDA (ZP1),Y			; Read byte
		STA (ZP2),Y			; Save it
		INY				; Next byte
		BNE CopyLoop			; Finished a page? No, go back for more
		INC ZP1 + 1			; Next Page
		INC ZP2 + 1			; Next page
		DEX				; Count the page
		BNE CopyLoop			; Back for more		
		RTS

;======================================================================================
; SAVE/RESTORE CURSOR
;======================================================================================
; Saves or Restores the cursor position

SaveCursorPos:	LDA ScrPtr			; $C4/C5 - pointer to screen line
		STA SCNSAVE1
		LDA ScrPtr+1
		STA SCNSAVE2
		LDA CursorCol			; $C6 - position of cursor on line
		STA SCNSAVE3
		LDA CursorRow			; $D8 - line where cursor lived
		STA SCNSAVE4
		RTS

RestoreCursorPos:
		LDA SCNSAVE1
		STA ScrPtr
		LDA SCNSAVE2
		STA ScrPtr+1
		LDA SCNSAVE3
		STA CursorCol
		LDA SCNSAVE4
		STA CursorRow
		RTS

;======================================================================================
; DATA TABLES
;======================================================================================

;-------------- User Interface

TitleBar:	!PET 147,18,"stevebrowse 2021-04-05                  ",13,0	; Top Line Titlebar
DirBox1:	!BYTE $B0					; Top Left Corner
		!BYTE $C0,$C0,$C0,$C0,$C0,$C0,$C0,$C0,$C0	; Hor Line
		!BYTE $C0,$C0,$C0,$C0,$C0,$C0,$C0,$C0,$C0	; Hor Line
		!BYTE $AE,13,0					; Top Right Corner, CR
DirBox2:	!PET  $DD,"                  ",$DD,13,0		; Centre line
DirBox3:	!BYTE $AD					; Bottom Left Corner
		!BYTE $C0,$C0,$C0,$C0,$C0,$C0,$C0,$C0,$C0	; Hor Line
		!BYTE $C0,$C0,$C0,$C0,$C0,$C0,$C0,$C0,$C0	; Hor Line
		!BYTE $BD,13,0					; Bottom Right Corner, CR
KeyBar:		!PET "up/dn=sel,return=run/cd, r=root",19,13,0	; Keys <home> - NO CR

;-------------- 
HomeHome:	!BYTE 19,19,17,0				; WAS: 147,13,46,0				; CLS, CR, PERIOD
FNDirectory:	!PET "$1",0					; Directory string
AreUSure:	!PET "are you sure (y/n)?",0			; Confirm

;======================================================================================
; SCREEN LINE ADDRESS TABLE
;======================================================================================
; This is a table of screen line addresses, representing the address of the start of each line.
; There are 50 entries for both HI and LO bytes. Each pair is offset 40 bytes so when
; calculating start lines for 80 column you must multiply ROW by 2.
;
;SLALO:		!byte $00,$28,$50,$78,$a0,$c8,$f0,$18,$40,$68
;		!byte $90,$b8,$e0,$08,$30,$58,$80,$a8,$d0,$f8
;		!byte $20,$48,$70,$98,$c0,$e8,$10,$38,$60,$88
;		!byte $B0,$d8,$00,$28,$50,$78,$a0,$c8,$f0,$18
;		!byte $40,$68,$90,$b8,$e0,$08,$30,$58,$80,$a8
;		!byte $d0
;
;SLAHI:		!byte $80,$80,$80,$80,$80,$80,$80,$81,$81,$81
;		!byte $81,$81,$81,$82,$82,$82,$82,$82,$82,$82
;		!byte $83,$83,$83,$83,$83,$83,$84,$84,$84,$84
;		!byte $84,$84,$85,$85,$85,$85,$85,$85,$85,$86
;		!byte $86,$86,$86,$86,$86,$87,$87,$87,$87,$87
;		!byte $87

;======================================================================================
; Init Stuff
;======================================================================================

;-------------- Keyboard Command Bytes

Keylist:	!byte $91,$11,$9D,$1D,$0D,$20,$03,$83		;<UP><DOWN><LEFT><RIGHT><ENTER><SPACE><STOP><RUN> Basic Browsing Keys
		!byte $43,$51					;<C>opy <Q>uit - Commands

KeyJumpLO:	!byte <KeyUp,<KeyDown,<KeyLeft,<KeyRight,<KeyEnter,<KeySpace,<KeyStop,<KeyRun
		!byte <KeyCopy,<KeyQuit

KeyJumpHI:	!byte >KeyUp,>KeyDown,>KeyLeft,>KeyRight,>KeyEnter,>KeySpace,>KeyStop,>KeyRun
		!byte >KeyCopy,>KeyQuit

;-------------- Key Routines Placeholder (Routines not yet written)

KeyUp:
KeyDown:
KeyLeft:
KeyRight:
KeyEnter:
KeySpace:
KeyStop:
KeyRun:
KeyCopy:
KeyQuit:
		RTS

;-------------- Misc Strings

ProgStart	!PET ">>> ",0					; DEBUG Start of Sub 
ProgMarker	!PET "!",0					; DEBUG Marker

;-------------- DEBUG Strings

TEMP1:		!PET 13,"reading: ",0
TEMP2:		!PET 13,"directory loaded",13,0

;======================================================================================
; PADD to 4K to be able to load into VICE
;======================================================================================

!FILL $B000-*,0
