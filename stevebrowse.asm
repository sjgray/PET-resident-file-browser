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
PRINT		= $FFD2				; Print a character routine

;-------------- Constants

SCREENPAGES     = 8				; 2K size for 80 col
DIRWIDTH        = 16				; Width of directory window
DIRHEIGHT	= 20				; Height of directory window
DIRROW		= 0				; Row for first directory line
DIRCOL0		= 0				; Column for LEFT directory
DIRCOL1		= 39				; Column for LEFT directory
IDFLAG		= 0				; Flag to add ID string
MYDRIVE		= 8				; Default Drive Number, ie: 8
DRIVEUNIT	= 0				; Default Unit Numberm ie: 0 or 1
DRECLEN		= 23				; Record length for ram directory

DSROW		= 22				; Location of Disk Status ROW
MSGROW		= 23				; Location of Info Status ROW
HELPROW		= 24				; Location of HELP keys   ROW

RVS		= 18				; <RVS>
ROFF		= 146				; <OFF>

;======================================================================================
; PROGRAM STORAGE LOCATIONS
;======================================================================================
; $B1-$C3 used for TAPE. Safe to use here for now?

;ScrPtr		= $c4				; LO Pointer to character screen line
;		= $c5				; HI Pointer to character screen line
;CursorCol	= $c6				; Cursor COL - Position of cursor on above line
;CursorRow	= $d8				; Cursor ROW
;STROUTZ	= $bb1d				; A=LSB, Y=MSB			; BASIC4 STROUTZ is broken!
;STROUT		= $bb24				; X=len, STRADR=ptr


ACTIVEDIR	= $B1				; LO Screen Save Buffer Pointer
;		= $B2				; HI
SCNWID		= $B5				; Current Screen Width 
CLMARGIN	= $B6				; Cursor LEFT Margin
SELMEM		= $B7				; Selected Item Memory Pointer
;		= $B8				; FREE

;-- Details for Directory being Browsed

DMEM		= $B9				; LO Directory Buffer Pointer
;		= $BA				; HI
DTOPMEM		= $BB				; LO Pointer to Top Entry Address
;		= $BC				; HI
DTOP		= $BD				; Index of Top entry
DSEL		= $BE				; Index of Selected Entry
DBOT		= $BF				; Index of Bottom Entry
DEND		= $C0				; Index of Last Entry
DCOUNT		= $C1				; Counter for directory display
DENTRY		= $C2				; Index of current Entry
DCOL		= $C3				; Left column for directory listing

;-- $ED-$F7 not used in 80-column machines. Safe to use here for now?

ZP1		= $EE				; Srce Pointer - Scn Cpy, String Print
ZP2		= $F0				; Dest Pointer - Scn Cpy
ZP3		= $F2				; Work Pointer - Directory display

;-------------- Memory Locations
; These locations should eventually move to BASIC program space so that ML code
; that uses the tape buffer will work.
; TODO: instead of copying individual bytes we should copy the entire block for
;       LEFT or RIGHT to/from ZP locations for current DIR memory.

TAPEBUFFER      = $027A				; 634   Tape buffer

SCNSAVE1	= TAPEBUFFER			; $027B Cursor Pointer LO
SCNSAVE2	= TAPEBUFFER+1			; $027C Cursor Pointer HI
SCNSAVE3	= TAPEBUFFER+2			; $027D Cursor Column/offset
SCNSAVE4	= TAPEBUFFER+3			; $027E Cursor Row
SCNMEM		= TAPEBUFFER+4			; $027F LO Pointer to Screen Buffer
;		= TAPEBUFFER+5			; $0280 HI Pointer to Screen Buffer
CMDMEM		= TAPEBUFFER+6			; $0281 LO Command String Buffer Pointer
;		= TAPEBUFFER+7			; $0282 HI

;-- Details for LEFT/RIGHT Directory
; These locations must match the ones in Zero Page (starting from DMEM)

LDIRMEM		= TAPEBUFFER+10			; $0284 LO LEFT Directory Buffer
;		= TAPEBUFFER+11			; $0285 HI LEFT Directory Buffer
LDIRTOPMEM	= TAPEBUFFER+12			; $0286 LO LEFT Top Memory Pointer
;		= TAPEBUFFER+13			; $0287 HI LEFT Top Memory Pointer
LDIRTOP		= TAPEBUFFER+14			; $0288 LEFT Index of Top Entry
LDIRSEL		= TAPEBUFFER+15			; $0289 LEFT Index of Selected entry
LDIRBOT		= TAPEBUFFER+16			; $028A LEFT Index of Bottomd entry
LDIREND		= TAPEBUFFER+17			; $028B LEFT Index of Last Entry

RDIRMEM		= TAPEBUFFER+20			; $028E LO RIGHT Directory Buffer
;		= TAPEBUFFER+21			; $028F HI RIGHT Directory Buffer
RDIRTOPMEM	= TAPEBUFFER+22			; $0286 LO RIGHT Top Memory Pointer
;		= TAPEBUFFER+23			; $0287 HI RIGHT Top Memory Pointer
RDIRTOP		= TAPEBUFFER+24			; $0290 RIGHT Index of Top Entry
RDIRSEL		= TAPEBUFFER+25			; $0291 RIGHT Index of Selected entry
RDIRBOT		= TAPEBUFFER+26			; $0292 RIGHT Index of Bottom entry
RDIREND		= TAPEBUFFER+27			; $0293 RIGHT Index of Last Entry


;======================================================================================
; START OF PROGRAM CODE
;======================================================================================

!TO "sb.bin",plain	; Output filename without Load Address
*=$A000			; Put this in the $A Option ROM - SYS40960

Start:
		JSR InitStuff			; Do some intialization stuff
		JSR SaveCursorPos		; Save cursor position
		JSR SaveScreen			; Save the screen to memory
		JSR FindScnWidth		; Determine width of screen
		
		JSR DrawUI			; Draw the interface

		JSR LoadLeftDir			; Load the LEFT directory		
		JSR ShowLeftDir			; Show the LEFT directory

;		JSR LoadRightDir		; Load the RIGHT directory
;		JSR ShowRightDir		; Show the RIGHT directory

		JSR GetDiskStatus		; Get Disk Status

		JSR Interact			; The Main Code

;		JSR AnyKey			; Wait for a key
		JSR RestoreScreen		; Restore the screen
		JSR RestoreCursorPos		; Restore cursor position
BrowseDone:	RTS

;TestMsg1:	!PET "[test1]",13,0
;TestMsg2:	!PET "[test2]",13,0
;TestMsg3:	!PET "[test3]",13,0

;======================================================================================
; Interactive 
;======================================================================================
; This is the main code that runs to interact with the program features. It reads
; keystrokes and acts on them, looping around until user exits.

Interact:
		INC SCREEN_RAM+30		; DEBUG!!!!!!!!!!!!!!!!!!!!!

		JSR GETIN			; Get keystroke to .A
		BEQ Interact			; No press, go back

		STA SCREEN_RAM + 32		; DEBUG!!!!!!!!!!!!!!!!!!

		CMP #88				; Is it <X>?
		BEQ IsExit			; Yes, Exit!
		CMP #17				; Is it <DOWN>?
		BEQ IsDown			;
		CMP #145			; Is it <UP>?
		BEQ IsUp			;
		CMP #19				; Is it <HOME>?
		BEQ IsHome			;
		CMP #32				; Is it <SPACE>?
		BEQ IsSpace
		CMP #62				; Is it ">"?
		BEQ IsPgDn
		CMP #60				; Is it ">"?
		BEQ IsPgUp

		JMP Interact			; Loop back for more

IRefresh:	JSR ShowDirectory		; Re-draw Directory
ILoop:		JMP Interact			; Loop back for more
		

IsExit:		RTS				; Exit!

;-------------- Perform UP

IsUp:		LDY DSEL			; SELECTED Entry
		CPY DTOP			; Is it at TOP?
		BEQ YesTop			; Yes, try to scroll up. skip ahead
		DEC DSEL			; No, it's safe to move to prev entry
		JMP IRefresh			; Redraw directory

YesTop:		LDY DTOP			; Get TOP
		BEQ ILoop			; Is it Zero? no moving up. skip

		DEC DTOP			; Move TOP Up
		DEC DSEL			; Move SELECTED Up
		JSR DScrollDOWN			; Do Scroll DOWN
NoUp:		JMP IRefresh			; Refresh
		

;-------------- Perform DOWN		

IsDown:		LDY DSEL			; SELECTED entry
		CPY DEND			; Is is at the END?
		BEQ ILoop			; Yes, can't go down. abort.

		INC SCREEN_RAM+120		; DEBUG

		CPY DBOT			; Is it at BOTTOM
		BEQ IsDownOK			; Yes, need to scroll
		INC DSEL			; No, just inc SELECTED
		BNE IRefresh		

IsDownOK	INC DSEL			; Move SELECTED down
		INC DTOP
		JSR DScrollUP			; Do Scroll UP
		JMP IRefresh

;-------------- Perform HOME

IsHome:		LDY #0				; First Entry
		STY DTOP			; Set TOP
		INY
		STY DSEL			; Set SELECTED
		JSR DScrollTOP			; Scroll to TOP
		JSR DrawUI
		JMP IRefresh

;-------------- Perform SPACE

IsSpace:	LDA DSEL			; Is SELECTED zero? (header)
		BEQ ILoop			; Yes, abort

		LDY #18				; Index for MARK character
		LDX #32				; Assume we need to de-Select
		LDA (SELMEM),Y			; Get the bye
		CMP #95				; Is it <BACKARROW>?
		BEQ IsSpaceX			; Yes, deselect it
		LDX #95				; No, make it <BACKARROW>
IsSpaceX:	TXA
		STA (SELMEM),Y			; Write <BACKARROW> to entry
		JMP IRefresh

IsPgDn:		LDA ZP3				; LO Work Pointer
		CLC
		ADC #$CC			; DRECLEN*DIRHEIGHT=460= $1CC
		STA ZP3
		LDA ZP3+1			; HI Work Pointer
		ADC #1
		STA ZP3+1			; HI Work Pointer
; todo: check for past END!
		LDA DTOP
		ADC #DIRHEIGHT
		STA DTOP			; Directory TOP
		STA DSEL			; SELECTED entry
		JMP IRefresh

IsPgUp:
		
;-------------- We are lost

		JMP IRefresh

;-------------- Directory Scrolling
; Takes the current DTOPMEM pointer and loads into ZP3 pointer.
; Scrolling up or down adds or subtracts DRECLEN constant to update ZP3.

DScrollUP:	JSR DTOP2ZP3			; Load the Work Pointers
		ADC #DRECLEN			; Subtract entry length
		STA ZP3				; Store it
		STA DTOPMEM
		BCC DScrollX			; Did it cross the page?
		INC ZP3+1			; Page cross
		INC DTOPMEM+1
		RTS

DScrollDOWN:	JSR DTOP2ZP3			; Load the Work Pointers
		SBC #DRECLEN			; Add entry length
		STA ZP3				; Store it		
		STA DTOPMEM			
		BCC DScrollX			; Did it cross the page?
		INC ZP3+1
		INC DTOPMEM+1
		RTS

;-------------- 
DScrollTOP:	LDY DMEM			; LO DMEM points to start
		INY
		INY
		STY DTOPMEM			; LO Work Pointer
		STY ZP3

		LDA DMEM+1			; HI
		STA DTOPMEM+1			; HI
		STA ZP3+1
		RTS 

;-------------- Load the DTOPMEM pointer and copy to ZP3

DTOP2ZP3:	LDA DTOPMEM+1			; HI DTOP Memory Address
		STA ZP3+1			; HI Store in Work Pointer
		LDA DTOPMEM			; LO DTOP Memory Address
		STA ZP3				; LO Store in Work Pointer
		CLC				; Prep for Add or Subtract
DScrollX	RTS				; Return with LO in .A

;======================================================================================
; Init Stuff
;======================================================================================
; Here we look at the BASIC pointers to figure out where we can store our data.
; STREND  pointer is the End of Array pointer which is the first address that is free
; FREETOP pointer is the End of RAM and is the last address we can use.
; ** Testing phase - ignore FREETOP, max 255 entries which uses ~6K for directory.

; Buffers with no BASIC program loaded: LEFT  at $0D00, RIGHT at $2500

InitStuff:	LDX #0				; Make our data start on page boundry
		STX SCNMEM			; LO Screen Save Address
		STX LDIRMEM			; LO LEFT  Directory Buffer
		STX RDIRMEM			; LO RIGHT Directory Buffer
		STX CLMARGIN			; Cursor Left Margin
		LDX STREND+1			; HI End of Arrays
		INX				; Start on boundty of next page
		STX SCNMEM+1			; HI Screen Save Buffer
		TXA
		CLC
		ADC #8				; Reserve 8 pages for 80-col screen
		STA LDIRMEM+1			; HI LEFT Directory Buffer
		ADC #24				; Reserve 24 pages (6K) for LEFT Dir
		STA RDIRMEM+1			; HI RIGHT Directory Buffer
		
		LDA #8				; Default Drive=8
		STA MYDRIVE			; Set it
		RTS


;======================================================================================
; SETUP FOR LEFT/RIGHT DIRECTORY
;======================================================================================
; Displays the directory in the proper location

ShowLeftDir:	LDY LDIRMEM			; LO Pointer to LEFT Directory Buffer
		LDA LDIRMEM+1			; HI Pointer to LEFT Directory Buffer
		JSR SetDirMem			; Set Memory Pointers
		LDA #DIRCOL0			; LEFT directory column
		LDY LDIREND			; Index of LEFT last entry
		JMP ShowTest

ShowRightDir:	LDY RDIRMEM			; LO Pointer to LEFT Directory Buffer
		LDA RDIRMEM+1			; HI Pointer to LEFT Directory Buffer
		JSR SetDirMem			; Set Memory Pointers
		LDA #DIRCOL1			; LEFT directory column
		LDY RDIREND			; Index of LEFT last entry

ShowTest:	STA DCOL			; Directory Column
		STY DEND			; Index of last entry
		CPY #0				; Is DEND zero?
		BNE ShowDirectory		; No, good to go
		RTS				; Yes, nothing to show!

SetDirMem:	STY DMEM			; LO Pointer for Dir Memory
		INY				; Add 2 to skip block bytes
		INY
		STY DTOPMEM			; LO Top Index Memory Pointer
		STY ZP3				; LO Work Pointer

		STA DMEM+1			; HI Pointer for Dir Memory
		STA DTOPMEM+1			; HI Top Index Memory Pointer
		STA ZP3+1			; HI Work Pointer
		RTS

DIRDEBUG:	LDY DTOPMEM
		STY SCREEN_RAM+40
		LDY DTOPMEM+1
		STY SCREEN_RAM+41

		LDY DTOP
		STY SCREEN_RAM+43
		LDY DSEL
		STY SCREEN_RAM+44
		LDY DBOT
		STY SCREEN_RAM+45
		LDY DEND
		STY SCREEN_RAM+46
		RTS

;======================================================================================
; SHOW DIRECTORY
;======================================================================================
; ZP3 pointer used to walk through directory memory
; Copy ZP3 to DTOPMEM so we can Scroll relative to this.
; Update DTOP. Increment DENTRY as we go. Count displayed lines in DCOUNT.
; When bottom is reached save to DBOT.
; DSEL is the currently selected entry - it must be marked (somehow!!!!!)

ShowDirectory:
		JSR DIRDEBUG
		LDY DTOPMEM			; LO DTOP Memory Address
		STY ZP3				; LO Work Pointer
		LDY DTOPMEM+1			; HI DTOP Memory Address
		STY ZP3+1			; HI Work Pointer
		LDY DTOP			; Top Entry
		STY DENTRY			; Make it Current Entry

		LDY #0				; Count=1 (first entry displayed)
		STY DCOUNT			; Entry Counter
		STY DEND			; END Index

;-------------- Position Cursor

SDLoop:		LDA #DIRROW			; Cursor Position for Top Left
		CLC
		ADC DCOUNT			; Add Entry Count
		LDY DCOL			; Column for selected directory
		JSR CursorAt			; Move cursor to top of directory

		LDY #0				; Index for print loop
		STY ReverseFlag			; also, set <RVS> off

SDSelection:
		LDA #32				; Assume <SPACE> character
		LDY DCOUNT			; What is the entry#?
		CPY DSEL			; Is it SELECTED?
		BNE SDpoint			; No, skip ahead

;-------------- Selected Item - Save Pointer
		LDA ZP3				; Save pointer to SELECTED
		STA SELMEM			; LO SELECTED memory pointer
		STA SCREEN_RAM+ 36		; DEBUG!!!!!!!!!!!!!!!!!!!!!!!!!!
		LDA ZP3+1			; HI Work Pointer
		STA SELMEM+1			; HI SELECTED memory pointer
		STA SCREEN_RAM+ 37		; DEBUG!!!!!!!!!!!!!!!!!!!!!!!!!!

		LDA #62				; Yes, use ">" character
SDpoint:	JSR $FFD2			; Print It!

;-------------- Check if Entry is Marked

		LDY #18				; Offset for in entry for Marker
		LDA (ZP3),Y			; Get the byte
		CMP #95				; Is it <BACKARROW>?
		BNE NotMarked			; No, skip ahead
		LDA #18				; Yes, print <RVS>
		JSR PRINT			; Print It!

;-------------- Print the entry

NotMarked:	LDY #0				; Start index at zero

SDprint:	LDA (ZP3),Y			; Get character at pointer
		CMP #0				; Is it <NULL>?
		BEQ SDpos			; Yes, we are done
		JSR PRINT			; No, print it!
		INY				; next character
		BNE SDprint			; Jump back for more

;-------------- Move Work Pointer to next entry

SDpos:		LDA ZP3				; LO Work Pointer
		CLC				; prep for Add
		ADC #DRECLEN			; Add record len to pointer
		STA ZP3				; LO Work Pointer
		BCC SDnext			; Did it cross page? No skip ahead
		INC ZP3+1			; Yes, inc page.

SDnext:		INC DCOUNT			; Next Entry#
		LDA DCOUNT			; Get it again		
		STA DBOT			; Set as BOTTOM (temporarily)		
		CMP #DIRHEIGHT			; Is it at end of box?
		BEQ SDexit			; Yes, exit		
		CMP DEND			; Is it at the end of the directory?
		BNE SDLoop			; No, Go back for another entry
SDexit:		DEC DBOT
		RTS


;======================================================================================
; PRINT ROUTINES
;======================================================================================
; PrintAt:	Set .A=LO, .Y=HI address of string
;		String:	First byte is ROW, second is COL, the rest is printed.
; CursorAt:	Set .A=ROW, .Y=COL	- Positions cursor.
; ClearMsg:	NONE			- Clears Status line with SPACES
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

SetMsg:	PHA					; Save .A and .Y
		TYA					; onto stack
		PHA
		JSR ClearMsg				; Clear the Status Line
		LDA #MSGROW				;
		LDY #0
		JSR CursorAt				; Position cursor
		PLA					; Bring back pointer to string
		TAY
		PLA
		JMP STROUTZ				; Print the string

ClearMsg:	LDA #MSGROW				; Message ROW
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

GetDiskStatus:
		LDA #DSROW			; Disk Status ROW
		LDY #0
		JSR CursorAt
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

GS_DONE		jsr UNTLK			; UNTALK
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

		LDA #<FNDir0			; LO Pointer to Filename ("$0")
		STA FNADR			; LO Filename Address pointer
		LDA #>FNDir0			; HI
		STA FNADR+1			; HI 

		JSR LoadDirectory		; Read directory to memory, count entries

		LDA DTOP			; Now we copy results back to LEFT DIR
		STA LDIRTOP			; Start at Top of List
		LDA DSEL
		STA LDIRSEL			; Selected at top
		LDA DCOUNT
		STA LDIREND			; End of directory
		RTS

LoadRightDir:
		LDA RDIRMEM			; LO Pointer to Directory Storage Address
		STA ZP1				; LO
		LDA RDIRMEM+1			; HI Pointer to Directory Storage Address
		STA ZP1+1			; HI Pointer

		LDA #<FNDir1			; LO Pointer to Filename ("$1")
		STA FNADR			; LO Filename Address pointer
		LDA #>FNDir1			; HI
		STA FNADR+1			; HI 

		JSR LoadDirectory		; Read directory to memory, count entries

		LDA DTOP			; Now we copy results back to LEFT DIR
		STA RDIRTOP			; Start at Top of List
		LDA DSEL
		STA RDIRSEL			; Selected at top
		LDA DCOUNT
		STA RDIREND			; Number of entries in directory			; TODO: save the end address here
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
		STA DEND			; Index of Last Entry
		STA DCOUNT			; Counter for directory display
		LDA #1				; Make entry after header SELECTED
		STA DSEL			; Index of Selected Entry
  
		LDA #<ReadingDir		; Print "Reading.."
		LDY #>ReadingDir
		JSR SetMsg

		LDY #2				; Length=2 "$0" or "$1"
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
		JMP StopListing		

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
		JSR SetMsg
		LDX DCOUNT			; LO # of entries = DCOUNT
		STX DEND			; save as END
		LDA #0				; HI # of entries = 0
		JSR INTOUT			; write #blocks to screen
		RTS


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
; Draw Program Info and key help line

DrawUI:		JSR $E015			; Clear Screen
		LDA #<ProgInfo				
		LDY #>ProgInfo
		JSR PrintAt			; Print Program Info

		LDA #<KeyBar				
		LDY #>KeyBar
		JSR PrintAt			; Print Key Bar
		RTS

;======================================================================================
; DATA TABLES
;======================================================================================

;-------------- User Interface

ProgInfo:	!BYTE MSGROW-1,0				; Message ROW
		!PET  "stevebrowse 2021-04-11",0 		; Title text
KeyBar:		!BYTE HELPROW,0					; HELP ROW
		!PET  RVS,"crsr",  ROFF,"sel "			; cursor
		!PET  RVS,"home",  ROFF,"top "			; home
		!PET  RVS,"space",  ROFF,"mark "		; space
		!PET  RVS,"return",ROFF,"run/cd "		; return		
		!BYTE RVS,94,      ROFF				; UPARROW
		!PET  "root",0					; 

;-------------- 
HomeHome:	!BYTE 19,19,147,17,0				; <HOME><HOME><CLS><DOWN>
FNDir0:		!PET "$0",0					; Directory string
FNDir1:		!PET "$1",0					; Directory string

AreUSure:	!PET "are you sure (y/n)?",0
ReadingDir:	!PET "reading...",0		
DirLoaded:	!PET 146,"# of entries:",0
Copying		!PET "copying...",0
Renaming:	!PET "renaming...",0


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
