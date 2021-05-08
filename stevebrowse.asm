; PET/CBM EDIT ROM - File Browser and Utility  (C)2021 Steve J. Gray
; ===========================================  
; - Started March 25, 2021
; - Updated: 2021-04-07
;
; A File Browser Utility that can be included in Editor ROM or Option ROM.
;
;-------------- Includes
; includes from editrom project. Need to fool it, so define a few things first.
; This will all be eliminated when merged with Editrom Project.

COLUMNS		= 80
ROWS		= 25
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
DIRHEIGHT	= 18				; Height of directory window
DIRROW		= 2				; Row for first directory line
DIRCOL		= 2				; Column for directory listing
IDFLAG		= 0				; Flag to add ID string
MYDRIVE		= 8				; Default Drive Number, ie: 8
DRIVEUNIT	= 0				; Default Unit Numberm ie: 0 or 1
DRECLEN		= 23				; Record length for ram directory
STATUSROW	= 23				; Location of status row

;======================================================================================
; PROGRAM STORAGE LOCATIONS
;======================================================================================
; $B1-$C3 used for TAPE. Safe to use here for now?

;ScrPtr		= $c4				; LO Pointer to character screen line
;		= $c5				; HI Pointer to character screen line
;CursorCol	= $c6				; Cursor COL - Position of cursor on above line
;CursorRow	= $d8				; Cursor ROW


SCNMEM		= $B1				; LO Screen Save Buffer Pointer
;		= $B2				; HI
CMDMEM		= $B3				; LO Command String Buffer Pointer
;		= $B4				; HI

SCNWID		= $B7				; Current Screen Width 
CLMARGIN	= $BA				; Cursor LEFT Margin

;-- Details for Directory being Browsed
DMEM		= $BB				; LO Directory Buffer Pointer
;		= $BC				; HI
DTOP		= $BD				; Index of Top entry
DSEL		= $BE				; Index of Selected Entry
DEND		= $BF				; Index of Last Entry
DCOUNT		= $C0				; Counter for directory display
DENTRY		= $C1				; Index of current Entry
;last free	= $C2				; Last free

;-- $ED-$F7 not used in 80-column machines. Safe to use here for now?

ZP1		= $EE				; Srce Pointer - Scn Cpy, String Print
ZP2		= $F0				; Dest Pointer - Scn Cpy
ZP3		= $F2				; Work Pointer - Directory display

;-------------- Memory Locations

TAPEBUFFER      = $027A				; Tape buffer
SCNSAVE1	= TAPEBUFFER+1			; Cursor Pointer LO
SCNSAVE2	= TAPEBUFFER+2			; Cursor Pointer HI
SCNSAVE3	= TAPEBUFFER+3			; Cursor Column/offset
SCNSAVE4	= TAPEBUFFER+4			; Cursor Row

;-- Details for LEFT Directory
LDIRMEM		= TAPEBUFFER+5			; LO LEFT Directory Buffer Pointer
;		= TAPEBUFFER+6			; HI
LDIRTOP		= TAPEBUFFER+7			; LEFT Index of Top Entry
LDIRSEL		= TAPEBUFFER+8			; LEFT Index of selected entry
LDIREND		= TAPEBUFFER+9			; LEFT Index of Last Entry

;-- Details for RIGHT Directory
RDIRMEM		= TAPEBUFFER+10			; LO LEFT Directory Buffer Pointer
;		= TAPEBUFFER+11			; HI
RDIRTOP		= TAPEBUFFER+12			; LEFT Index of Top Entry
RDIRSEL		= TAPEBUFFER+13			; LEFT Index of selected entry
RDIREND		= TAPEBUFFER+14			; LEFT Index of Last Entry


;======================================================================================
; START OF PROGRAM CODE
;======================================================================================

!TO "sb.bin",plain				; Output filename without Load Address
*=$A000						; Put this in the $A Option ROM

Start:
		JSR InitStuff			; Do some intialization stuff
		JSR SaveCursorPos		; Save cursor position
		JSR SaveScreen			; Save the screen to memory
		JSR FindScnWidth		; Determine width of screen
		
		JSR DrawUI			; Draw the interface
		JSR LoadLeftDir			; Load the directory
		JSR ShowLeftDir			; Show the directory
		JSR GetDiskStatus		; Get Disk Status

		JSR AnyKey			; Wait for a key
		JSR RestoreScreen		; Restore the screen
		JSR RestoreCursorPos		; Restore cursor position
BrowseDone:	RTS

TestMsg1:	!PET "[start1]",13,0
TestMsg2:	!PET "[start2]",13,0
TestMsg3:	!PET "[initdone]",13,0
TestMsg4:	!PET "[copyscreen]",13,0

;======================================================================================
; Init Stuff
;======================================================================================
; Here we look at the BASIC pointers to figure out where we can store our data.
; STREND  pointer is the End of Array pointer which is the first address that is free
; FREETOP pointer is the End of RAM and is the last address we can use.
; ** Testing phase - ignore FREETOP, max 255 entries which uses ~6K for directory.

InitStuff:	LDX #0				; Make our data start on page boundry
		STX SCNMEM			; LO Screen Save Address
		STX LDIRMEM			; LO Directory Storage Address
		STX CLMARGIN			; Cursor Left Margin
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
; SHOW DIRECTORY
;======================================================================================
; Displays the directory in the proper location

ShowLeftDir:
						; DEBUG************
		LDY LDIRMEM			; LO Pointer to LEFT Directory Buffer
		STY DMEM			; LO Pointer for Dir Memory
		INY				; Add 2 to skip block bytes
		INY
		STY ZP3				; LO Work Pointer
		LDY LDIRMEM+1			; HI Pointer to LEFT Directory Buffer
		STY DMEM+1			; HI Pointer for Dir Memory
		STY ZP3+1			; HI Work Pointer

		LDY DTOP			; Top Entry
		STY DENTRY			; Make it Current Entry
		LDY LDIREND
		STY DEND

		LDY #0				; Count=1 (first entry displayed)
		STY DCOUNT			; Entry Counter

SDLoop:
		INC DCOUNT
		LDA DCOUNT			
		STA SCREEN_RAM			; DEBUG
		
;-------------- Position Cursor
		LDA #DIRROW			; Cursor Position for Top Left
		CLC
		ADC DCOUNT			; Add Entry Count
		LDY #DIRCOL
		JSR CursorAt			; Move cursor to top of directory

;-------------- Print the entry
		ldy #0
		lda #'!'
		sta (ZP3),Y
		LDA ZP3				; LO Work Pointer
		LDY ZP3+1			; HI Work Pointer
		JSR STROUTZ			; Print it, starting at <QUOTE>

SDnext:		LDA ZP3
		CLC
		ADC #DRECLEN			; Add record len to pointer
		STA ZP3				; 
		BNE SDNext			; Did it cross page? No skip ahead
		INC ZP3+1			; Yes, inc page.

SDNext:		LDA DCOUNT			; Get it again
		CMP #DIRHEIGHT			; Is it at end of box?
		BEQ SDexit
		CMP DEND			; Is it at the end of the directory
		BNE SDLoop			; No, Go back for more

		JSR ClearStatus
	
SDexit:		RTS


;======================================================================================
; PRINT ROUTINES
;======================================================================================
; PrintAt:	Set .A=LO, .Y=HI address of string
;		String:	First byte is ROW, second is COL, the rest is printed.
; CursorAt:	Set .A=ROW, .Y=COL	- Positions cursor.
; ClearStatus:	NONE			- Clears Status line with SPACES
; ClearRow:	Set .A=ROW, 		- Clears the entire ROW with SPACE.
; FillRow:	Set .A=ROW, .Y=CHR	- Clears the entire ROW with CHR.

PrintAt:
		STA ZP1					; Set LO
		STY ZP1+1				; Set HI		
		LDY #0					; Index at zero
		LDA (ZP1),Y				; Get print ROW
		STA CursorRow				; Set Cursor ROW
		JSR IncZP1				; Advance pointer
		LDA (ZP1),Y				; Get print COL
		STA CursorCol				; Set Cursor COL
		JSR IncZP1				; ZP1 now points to TEXT
		JSR SetCursor				; Calculate cursor position

		LDA ZP1					; LO
		LDY ZP1+1				; HI
		JMP STROUTZ				; Print it!

CursorAt:	STA CursorRow				; Set ROW
		STY CursorCol				; Set COL
		JSR SetCursor				; Position Cursor
		RTS

SetStatus:	PHA					; Save .A and .Y
		TYA					; onto stack
		PHA
		JSR ClearStatus				; Clear the Status Line
		LDA #STATUSROW				;
		LDY #0
		JSR CursorAt				; Position cursor
		PLA					; Bring back pointer to string
		TAY
		PLA
		JMP STROUTZ				; Print the string

ClearStatus:	LDA #STATUSROW				; Row where status messages printed		
ClearRow:	LDY #32					; <SPACE>
FillRow:	JSR CursorAt
		LDA CursorCol				; Put CHR to A
		LDY #0					; Counter
ClearRloop	STA (ScrPtr),Y
		INY
		CPY SCNWID
		BNE ClearRloop
		RTS

;======================================================================================
; SET CURSOR
;======================================================================================
; Uses CursorRow to set Screen line pointer ScrPtr for printing.
; CursorCol is used as offset
; Messes up all registers
; We should be able to replace this routine when integrating with Editrom!

SetCursor:
		LDY CursorRow				; Get ROW as index
		LDA SCNWID				; Check Screen Width
		CMP #80					; Is it 80?
		BEQ SetC80				; Yes, skip over

		LDA SLA40_Lo,Y				; Get LO from Table
		LDX SLA40_Hi,Y				; Get HI from Table
		BNE SetCAX

SetC80:		LDA SLA80_Lo,Y				; Get LO from Table
		LDX SLA80_Hi,Y				; Get HI from Table

SetCAX:		STA ScrPtr				; Store in ScrPrt LO
		STX ScrPtr+1				; Store in ScrPrt HI
SetCDone:	RTS


;======================================================================================
; GET ANY KEY
;======================================================================================
; Wait for any key to be pressed 

AnyKey:		JSR GETIN			; Get keystroke to .A
		BEQ AnyKey			; None, so loop back
		RTS






;**************************************************************************************
; DISK ROUTINES
;**************************************************************************************
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
		RTS

;======================================================================================
; GET DISK STATUS
;======================================================================================
; Read the status string: ##,String,TT,SS

GetDiskStatus:	LDA MYDRIVE			; Current Device#
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

;======================================================================================
; LOAD LEFT/RIGHT DIRECTORY
;======================================================================================
; Sets up pointers for LEFT/RIGHT directory
; Initialize pointers for TOP,SEL,END,COUNT

LoadLeftDir:
		LDA LDIRMEM			; LO Pointer to Directory Storage Address
		STA ZP1				; LO
		LDA LDIRMEM+1			; HI Pointer to Directory Storage Address
		STA ZP1+1			; HI Pointer

		JSR LoadDirectory		; Read directory to memory, count entries

		LDA DTOP			; Now we copy results back to LEFT DIR
		STA LDIRTOP			; Start at Top of List
		LDA DSEL
		STA LDIRSEL			; Selected at top
		LDA DEND
		STA LDIREND			; End of directory
		LDA DCOUNT
		STA LDIREND			; Number of entries in directory			; TODO: save the end address here
		RTS

;======================================================================================
; LOAD DIRECTORY
;======================================================================================
; Reads the directory into RAM pointed to by ZP1.
; Counts # of directory entries to DTMP.

;-------------- Prep
LoadDirectory:
		LDA #0				; Clear pointers
		STA DTOP			; Index of Top entry
		STA DSEL			; Index of Selected Entry
		STA DEND			; Index of Last Entry
		STA DCOUNT			; Counter for directory display
		
		LDA #<ReadingDir		; Print "Reading.."
		LDY #>ReadingDir
		JSR SetStatus

		LDA #<FNDirectory		; LO Pointer to Filename ("$0")
		STA FNADR			; LO Filename Address pointer
		LDA #>FNDirectory		; HI
		STA FNADR+1			; HI 
		LDY #2				; Length=2 "$0"
		STY FNLEN			; Save it

		LDA #0				; Clear the status byte
		STA STATUS
		STA DCOUNT			; Zero the entry counter

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

Entry_done:	LDA DCOUNT			; Get Counter
		CMP #255			; Is it 255?
		BEQ StopListing			; Yes, Maximum entries, so exit
		INC DCOUNT			; Counter=Counter+1
		JMP LoadEntry			; No, Jump back for more lines

;-------------- Listing is complete

StopListing:	JSR CLSEI			; close file with $E0, unlisten
		LDA #<DirLoaded
		LDY #>DirLoaded
		JMP SetStatus


;-------------- Directory Read / Discard / Store, and Pointer Update ZP1
; Five entry points:
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

;-------------- Increment ZP1 Pointer

IncZP1:		INC ZP1				; Increment pointer LO
		BNE IncZP1X
		INC ZP1+1			; Increment pointer HI
IncZP1X:	RTS		


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
; This routine prints "<HOME><HOME><CLS><DOWN>" using the standard PET print routines.
; The <HOME><HOME> cancels any windowing, <CLS> Clears the screen and 40-col line links.
; The <DOWN> moves to the second line and the screen line pointer will be set, with the
; LO byte equal to 40 or 80 (points to $8028 or $8050).
; *** We can probably eliminate this when integrated into Editrom!

FindScnWidth:
		LDA #<HomeHome			; Address of string to print
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

CopyScreen:
		LDA #<TestMsg4
		LDY #>TestMsg4
		JSR STROUTZ

		LDX #SCREENPAGES		; How many pages to copy? 8=2K

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

SaveCursorPos:	LDA ScrPtr			; Pointer to screen line
		STA SCNSAVE1
		LDA ScrPtr+1
		STA SCNSAVE2
		LDA CursorCol			; Position of cursor on line
		STA SCNSAVE3
		LDA CursorRow			; Line where cursor lives
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
		JSR PrintAt			; Print key bar line

		LDA #24				; Last ROW
		LDY #45				; RVS Space
		JSR FillRow			; Clear the ROW
		RTS

;======================================================================================
; DATA TABLES
;======================================================================================

;-------------- User Interface

TitleBar:	!PET 147,18,"stevebrowse 2021-04-07                  ",13,0	; Top Line Titlebar
DirBox1:	!BYTE $B0					; Top Left Corner
		!BYTE $C0,$C0,$C0,$C0,$C0,$C0,$C0,$C0,$C0,$C0	; Hor Line
		!BYTE $C0,$C0,$C0,$C0,$C0,$C0,$C0,$C0,$C0,$C0	; Hor Line
		!BYTE $AE,13,0					; Top Right Corner, CR
DirBox2:	!PET  $DD,"                    ",$DD,13,0	; Centre line
DirBox3:	!BYTE $AD					; Bottom Left Corner
		!BYTE $C0,$C0,$C0,$C0,$C0,$C0,$C0,$C0,$C0,$C0	; Hor Line
		!BYTE $C0,$C0,$C0,$C0,$C0,$C0,$C0,$C0,$C0,$C0	; Hor Line
		!BYTE $BD,13,0					; Bottom Right Corner, CR
KeyBar:		!BYTE 5,30 ;ROW/COL
		!PET "up/dn=sel,return=run/cd, r=root",19,13,0	; Keys <home> - NO CR

;-------------- 
HomeHome:	!BYTE 19,19,147,17,0				; <HOME><HOME><CLS><DOWN>
FNDirectory:	!PET "$0",0					; Directory string

AreUSure:	!PET "are you sure (y/n)?",0
ReadingDir:	!PET "reading directory...",0		
DirLoaded:	!PET "directory loaded: ",0
Copying		!PET "copying...",0
Renaming:	!PET "renaming...",0

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

;======================================================================================
; SCREEN LINE ADDRESS TABLE
;======================================================================================
; These are tables representing the address of the start of each screen line for both
; 40 and 80 column wide screens. Screen width is determined at the start of the program
; and stored in SCNWID.
;
;---------- 40 characters wide
 
SLA40_Lo	!byte $00,$28,$50,$78,$a0,$c8,$f0,$18,$40,$68,$90,$b8,$e0
		!byte $08,$30,$58,$80,$a8,$d0,$f8,$20,$48,$70,$98,$c0

SLA40_Hi	!byte $80,$80,$80,$80,$80,$80,$80,$81,$81,$81,$81,$81,$81
		!byte $82,$82,$82,$82,$82,$82,$82,$83,$83,$83,$83,$83

;---------- 80 characters wide 

SLA80_Lo	!byte $00,$50,$a0,$f0,$40,$90,$e0,$30,$80,$d0,$20,$70,$c0
		!byte $10,$60,$b0,$00,$50,$a0,$f0,$40,$90,$e0,$30,$80

SLA80_Hi	!byte $80,$80,$80,$80,$81,$81,$81,$82,$82,$82,$83,$83,$83
		!byte $84,$84,$84,$85,$85,$85,$85,$86,$86,$86,$87,$87




;======================================================================================
; PADD to 4K to be able to load into VICE
;======================================================================================

!FILL $B000-*,0
