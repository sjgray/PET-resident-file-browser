; PET/CBM EDIT ROM - File Browser and Utility  (C)2021 Steve J. Gray, March 25/2021
; ===========================================  Updated: 2021-04-11
;
; A File Browser Utility for Option ROM or Editor ROM.
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

!SOURCE "membasic4.asm"			; BASIC calls
!SOURCE "memchips.asm"			; IO Chips
!SOURCE "memkernal.asm"			; KERNAL calls
!SOURCE "memlow.asm"			; Low Memory locations
!SOURCE "memzeropage.asm"		; Zero Page locations

PRINT		= $FFD2			; Print a character routine
CLEARSCREEN	= $E015			; Clear the screen

;-------------- Program Options

SCREENSAVE	= 1			; Include Screen Save Options

;-------------- Constants

SCREENPAGES     = 8			; 2K size for 80 col
DIRWIDTH        = 21			; Width  of directory windows
DIRHEIGHT	= 18			; Height of directory windows + 1 for header
DIRROW0		= 2			; LEFT  Directory Top-Left position
DIRCOL0		= 0			; LEFT  
DIRROW1		= 2			; RIGHT Directory Top-Left position
DIRCOL1		= 26			; RIGHT
IDFLAG		= 0			; Flag to add ID string
MYDRIVE		= 8			; Default Drive Number, ie: 8
DRIVEUNIT	= 0			; Default Unit Numberm ie: 0 or 1
DRECLEN		= 23			; Record length for ram directory

DEBUGRAM	= SCREEN_RAM+23*80+60	; Address  of Debug       ROW

PROGROW		= 0			; Location of Program Info ROW
DSROW		= 21			; Location of DiskST/File  ROW
MSGROW		= 22			; Location of Info Status  ROW
KEYROW		= 24			; Location of HELP keys    ROW

RVS		= 18			; <RVS>
ROFF		= 146			; <OFF>

;======================================================================================
; PROGRAM STORAGE LOCATIONS
;======================================================================================
; $B1-$C3 used for TAPE. Safe to use here for now?

;ScrPtr		= $c4			; LO Pointer to character screen line
;		= $c5			; HI Pointer to character screen line
;CursorCol	= $c6			; Cursor COL - Position of cursor on above line
;CursorRow	= $d8			; Cursor ROW
;STROUTZ	= $bb1d			; A=LSB, Y=MSB			; BASIC4 STROUTZ is broken!
;STROUT		= $bb24			; X=len, STRADR=ptr

;-------------- General

ACTIVE		= $B1			; Active Directory# 0 or 1
SCNWID		= $B2			; Current Screen Width 
TEMPMSG		= $B3			; Temp Message# Storage

;-------------- Details for Directory being Browsed
; The order of these 10 locations must match the LEFT and RIGHT details!

DMEMHI		= $B4			; +0 HI Directory Buffer Pointer
DDEV		= $B5			; +1 Device# (8-15)
DUNIT		= $B6			; +2 Drive Unit# (0 or 1) for dual drives
DTOP		= $B7			; +3 Index of Top entry
DSEL		= $B8			; +4 Index of Selected Entry
DBOT		= $B9			; +5 Index of Bottom Entry
DEND		= $BA			; +6 Index of Last Entry
DROW		= $BB			; +7 Row for Top-Left of Directory 
DCOL		= $BC			; +8 Col for Top-Left of Directory
DSELMEM		= $BD			; +8 LO Selected Item Memory Pointer
;		= $BE			; +9 HI Selected Item Memory Pointer

DENTRY		= $BF			; Index of current Entry
DCOUNT		= $C0			; Counter for directory display
CMDKEY		= $C1			; Command Keypress
CMDJUMP		= $C2			; LO Command Jump
;		= $C3			; HI Command Jump

;-------------- $ED-$F7 not used in 80-column machines. Safe to use here for now?

ZP1		= $EE			; Src  Pointer - Scn Cpy, String Print
ZP2		= $F0			; Dest Pointer - Scn Cpy
ZP3		= $F2			; Work Pointer - Directory display
;		= $F4			; FREE
;		= $F6			; FREE

;-------------- Memory Locations
; These locations should eventually move to BASIC program space so that ML code
; that uses the tape buffer will work.
; TODO: instead of copying individual bytes we should copy the entire block for
;       LEFT or RIGHT to/from ZP locations for current DIR memory.

TAPEBUFFER      = $0381			; End of Tapebuffer2 (was: TapeBuffer#1=$027A)

SCREENMEM	= TAPEBUFFER
LEFTMEM		= TAPEBUFFER+10		; Start of LEFT  Memory Structure
RIGHTMEM	= TAPEBUFFER+30		; Start of RIGHT Memory Structure

;-------------- Screen Save

SCNSAVE1	= SCREENMEM		; Cursor Pointer LO
SCNSAVE2	= SCREENMEM+1		; Cursor Pointer HI
SCNSAVE3	= SCREENMEM+2		; Cursor Column/offset
SCNSAVE4	= SCREENMEM+3		; Cursor Row
SCNMEMHI	= SCREENMEM+4		; LO Pointer to Screen Buffer

;-------------- Command Buffer

CMDMEM		= SCREENMEM+6		; LO Command String Buffer Pointer
;		= SCREENMEM+7		; HI Command String Buffer Pointer

;-------------- Details for LEFT/RIGHT Directory
; These locations must match the Following:
; DMEMHI	= $B4			; +0 HI Directory Buffer Pointer
; DDEV		= $B5			; +1 Device# (8-15)
; DUNIT		= $B6			; +2 Drive Unit# (0 or 1) for dual drives
; DTOP		= $B7			; +3 Index of Top entry
; DSEL		= $B8			; +4 Index of Selected Entry
; DBOT		= $B9			; +5 Index of Bottom Entry
; DEND		= $BA			; +6 Index of Last Entry
; DROW		= $BB			; +7 Row for Top-Left of Directory 
; DCOL		= $BC			; +8 Col for Top-Left of Directory
; DSELMEM	= $BD			; +9 LO Selected Item Memory Pointer
;		= $BE			; +10 HI Selected Item Memory Pointer

LDMEMHI		= LEFTMEM+0		; LEFT HI Directory Buffer Page
LDDEV		= LEFTMEM+1		; LEFT Device# (8-15)
LDUNIT		= LEFTMEM+2		; LEFT Drive Unit# (0 or 1) for dual drives
LDTOP		= LEFTMEM+3		; LEFT Index of Top Entry
LDSEL		= LEFTMEM+4		; LEFT Index of Selected entry
LDBOT		= LEFTMEM+5		; LEFT Index of Bottom entry
LDEND		= LEFTMEM+6		; LEFT Index of Last Entry
LDROW		= LEFTMEM+7		; LEFT Row for Top-Left of Directory 
LDCOL		= LEFTMEM+8		; LEFT Col for Top-Left of Directory
LDSELMEM	= LEFTMEM+9		; LEFT LO Selected Item Memory 
;		= LEFTMEM+10		; LEFT HI Selected Item Memory 

RDMEMHI		= RIGHTMEM+0		; RIGHT HI Directory Buffer Page
RDDEV		= RIGHTMEM+1		; RIGHT Device# (8-15)
RDUNIT		= RIGHTMEM+2		; RIGHT Drive Unit# (0 or 1) for dual drives
RDTOP		= RIGHTMEM+3		; RIGHT Index of Top Entry
RDSEL		= RIGHTMEM+4		; RIGHT Index of Selected entry
RDBOT		= RIGHTMEM+5		; RIGHT Index of Bottom entry
RDEND		= RIGHTMEM+6		; RIGHT Index of Last Entry
RDROW		= RIGHTMEM+7		; RIGHT Row for Top-Left of Directory 
RDCOL		= RIGHTMEM+8		; RIGHT Col for Top-Left of Directory
RDSELMEM	= RIGHTMEM+9		; RIGHT LO Selected Item Memory 
;		= RIGHTMEM+10		; RIGHT HI Selected Item Memory 


;======================================================================================
; START OF PROGRAM CODE
;======================================================================================

!TO "sb.bin",plain	; Output filename without Load Address
*=$A000			; Put this in the $A Option ROM - SYS40960

Start:
		JSR InitStuff			; Do some intialization stuff
!IF SCREENSAVE=1 {
		JSR SaveCursorPos		; Save cursor position
		JSR SaveScreen }		; Save the screen to memory
		JSR FindScnWidth		; Determine width of screen
		JSR DrawUI			; Draw the interface
		JSR PrepDir			; Load the ACTIVE directory
		JSR IRestart			; Interactive Loop until exit. Start with Refresh
BrowseDone:	RTS				; Exit Program


;======================================================================================
; DEBUG TO SCREEN !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;======================================================================================
; Visible display of certain memory locations on screen

ShowDEBUG:	INC DEBUGRAM
		LDY ACTIVE
		STY DEBUGRAM+1
		LDY DTOP
		STY DEBUGRAM+4
		LDY DSEL
		STY DEBUGRAM+5
		LDY DBOT
		STY DEBUGRAM+6
		LDY DEND
		STY DEBUGRAM+7
		LDY DENTRY
		STY DEBUGRAM+8
		RTS

;======================================================================================
; CLEAR ACTIVE DIRECTORY AREA
;======================================================================================
; Clears current directory area in preparation for new (shorter) directory

ClearDArea:	LDX DROW			; Top ROW of directory
		STX DENTRY			;
		LDX #255				; Count
		STX DCOUNT			;

ClearDAloop:	LDY #19				; No Entry string
		LDX DCOUNT			; Get the Count
		CPX #DIRHEIGHT			; Is it at the end?		
		BEQ ClearDAexit
		LDX DENTRY
		LDA DCOL			; Directory column
		JSR PrintAtNM			; Print Msg#=.Y at Row=.X, Col=.Y
		INC DENTRY			; Next Row
		INC DCOUNT			; Next Count
		JMP ClearDAloop
ClearDAexit:	RTS	


;======================================================================================
; INTERACTIVE
;======================================================================================
; This is the main code that runs to interact with the program features. It reads
; keystrokes and acts on them, looping around until user exits.

;-------------- Command Key Table and Address Lo/HI Tables
;       	KEY:   Q     , DOWN  , UP  , LEFT  , RIGHT  , HOME  , SPACE  , [     , ]     , /      , CLS  , RETURN  ,NULL
CMDTABLE:	!BYTE  81    , 17    , 145 ,  157  , 29     , 19    , 32     , 91    , 93    , 47     , 147  , 13      ,0
CMDLO:		!BYTE <IsQuit,<IsDown,<IsUp,<IsLeft,<IsRight,<IsHome,<IsSpace,<IsPgUp,<IsPgDn,<IsSlash,<IsCLS,<IsRETURN
CMDHI:		!BYTE >IsQuit,>IsDown,>IsUp,>IsLeft,>IsRight,>IsHome,>IsSpace,>IsPgUp,>IsPgDn,>IsSlash,>IsCLS,>IsRETURN

;-------------- Interactive Dispatch
Interact:
		JSR ShowDEBUG			; DEBUG!!!!!!!!!!!!!!!!!!!!!
		JSR GETIN			; Get keystroke to .A
		BEQ Interact			; No press, go back

		STA CMDKEY			; Save Key

		LDY #0				; Set Index to start of table
KeyLookup:	LDA CMDTABLE,Y			; Get a Key from the table
		BEQ Interact			; If zero we are at end of list
		INY				; Index for next key
		CMP CMDKEY			; Is is a key from the table?
		BNE KeyLookup			; No, check next in list

KeyFound:	DEY				; Yes, index to previous key
		LDA CMDLO,Y			; Get Target Address LO
		STA CMDJUMP			; Store it
		LDA CMDHI,Y			; Get Target Address LO
		STA CMDJUMP+1			; Store it
		JMP (CMDJUMP)			; Go to the Target Address

;-------------- Post-command actions

IRestart:	JSR DrawUI			; Re-draw GUI
IRefreshStat:	JSR ShowKeyBar			; Show Key Help line
IRefresh:	JSR ShowDirectory		; Re-draw Directory
		JMP Interact

ILoopStat:	JSR ShowKeyBar			; Show Key Help line
ILoop:		JMP Interact

ShowPET:
!IF SCREENSAVE=1{
		JSR RestScreen			; Restore the screen
		JSR RestCursorPos		; Restore cursor position
} ELSE {
		JSR CLEARSCREEN			; Clear the screen
}
		RTS

;======================================================================================
; Key Actions
;======================================================================================

;-------------- Exit Program

IsQuit:		JSR AskQuit			; Prompt to Exit
		CMP #89				; Is it "Y"?
		BNE ILoopStat			; No, continue
		JMP ShowPET			; Yes, Restore screen, cursor, then Exit!

;-------------- Perform UP

IsUp:		LDY DSEL			; SELECTED Entry
		CPY #1				; Is it first entry?
		BEQ NoUp			; Yes, no scrolling up
		CPY DTOP			; Is it at TOP?
		BNE YesUp			; No, ok to move selected up. skip ahead
YesTop:		DEC DTOP			; Move entire list up
YesUp:		DEC DSEL			; Move SELECTED Up
NoUp:		JMP IRefresh			; Refresh		

;-------------- Perform DOWN		

IsDown:		LDY DSEL			; SELECTED entry
		CPY DEND			; Is is at END of directory?
		BEQ ILoop			; Yes, can't go down. abort.

		CPY DBOT			; Is it at BOTTOM of displayed?
		BEQ IsDownOK			; Yes, need to scroll
		INC DSEL			; No, just move SELECTED down
		BNE IsDexit

IsDownOK:	INC DSEL			; Move SELECTED down
		INC DTOP			; Move TOP down

IsDexit:	JMP IRefresh

;-------------- Perform LEFT/RIGHT - Select Directory

IsLeft:		LDA ACTIVE			; From RIGHT to LEFT.
		BNE ISLRdo			; Is RIGHT active? Yes, do it
		BEQ ILoop			; No, LEFT - Ignore

IsRight:	LDA ACTIVE			; From LEFT to RIGHT
		BNE ILoop			; Is RIGHT active? Yes, Ignore

ISLRdo:		JSR SwapActive
		JMP IRefreshStat

;-------------- Perform HOME

IsHome:		LDY #1				; First Entry
		STY DTOP			; Set TOP
		STY DSEL			; Set SELECTED
		JSR DrawUI			; Re-draw the Interface
		JMP IRefresh

;-------------- Perform SPACE

IsSpace:	LDA DSEL			; Is SELECTED zero? (header)
		BEQ ILoop			; Yes, abort

		LDY #18				; Index for MARK character
		LDX #32				; Assume we need to de-Select
		LDA (DSELMEM),Y			; Get the bye
		CMP #95				; Is it <BACKARROW>?
		BEQ IsSpaceX			; Yes, deselect it
		LDX #95				; No, make it <BACKARROW>
IsSpaceX:	TXA
		STA (DSELMEM),Y			; Write <BACKARROW> to entry
		JMP IRefresh

;-------------- Perform Page DOWN

IsPgDn:
		CLC				;-- Check if END is less than PgDn
		LDA DBOT			; Directory END
		ADC #DIRHEIGHT-1		; Is it more than height of display?
		CMP DBOT
		BPL OkDown			; Yes, do it

NoDown:		CLC				;-- Set TOP to be one page from BOT
		LDA DEND			; No, get END
		SBC #DIRHEIGHT			; Subtract height of display
		STA DTOP			; Use it for TOP
		STA DSEL			; Put selected at TOP too
		JMP IRefresh

OkDown:		LDA DBOT			; Get the BOTTOM visible entry
		STA DTOP			; Make it the top
		STA DSEL			; and SELECTED
		JMP IRefresh

;-------------- Perform Page UP

IsPgUp:		CLC
		LDA DTOP			; Directory TOP
		CMP #DIRHEIGHT			; Height of display box
		BPL IPUfull			; Is it more? Yes, jump full page
		LDA #1				; No, just go to FIRST entry
		STA DTOP			; Set it
		JMP IRefresh

IPUfull:	SBC #DIRHEIGHT			; Subtract height of display box
		STA DTOP			; Set DTOP
		LDA DSEL			; SELECTED
		SBC #DIRHEIGHT			; Subtract height of display box
		STA DSEL			; Save it
		JMP IRefresh			; Re-draw list

;-------------- Perform Load new Drive/Unit

IsSlash:
IsCLS:		JSR AskDevice			; Ask and input Device#. Returns 8-15 in .A
		BEQ CLSabort
		STA DDEV			; Store Device#
		JSR AskUnit			; Ask and input Unit#. Returns 0 or 1
		STA DUNIT			; Store Unit#
		JSR LoadDirectory		; Get the new directory
		JMP IRefreshStat
CLSabort:	JMP ILoopStat			; Return and clear status line


;-------------- Perform RETURN
; This will handle various actions based on filetype
; P=PRG,D=DIR,C=CBM
IsRETURN:	LDY #19				; Index to FILETYPE Character
		LDA (DSELMEM),Y			; Get the FILETYPE		
		CMP #80				; "P"? - PRG file
		BEQ LoadPRG			; Yes, Load the file
		CMP #67				; "D"? - DIR file
		BEQ ChangeDIR			; Yes, Change Directory
		CMP #66				; "C"? - CBM file (1581 and?)
		BEQ SelPart			; Tes, Select Partition
		JMP IRefreshStat

;-------------- Confirm File Load

LoadPRG:	LDY #9				; "Load PRG?"	
		JSR ClearKeyNM			; Print It
		JSR AskSure			; Are you sure?
		CMP #89				; Is it "Y"?
		BEQ LPgo			; Yes
		JMP ILoopStat

;-------------- Do File Load
; Clear screen and print the load command, then stuff <CR> into keyboard to invoke

LPgo:		LDY #10				; Pre-filename string <CLS>dL<QUOTE>
		JSR PrintNM			; Print the string
		LDA #34				; <QUOTE>
		JSR PRINT			; Print it
 	
		LDY #1				; First character after quote
LPloop:		LDA (DSELMEM),Y			; Get character
		INY				; move ahead
		JSR PRINT			; Print it
		CMP #34				; Is it <QUOTE>?		
		BNE LPloop			; No, loop back for more

LPpost:		LDY #11				; Drive string ",D"
		JSR PrintNM			; Print the string

		LDX DUNIT			; Device# (0 or 1)		
		JSR PrintX			; Write INT to screen

		LDY #12				; Unit string ",U"
		JSR PrintNM

		LDX DDEV			; Unit#
		JSR PrintX			; Write INT to screen


;-------------- Stuff <CR> into Keyboard buffer

		LDA #19				; Home
		STA $026F			; First byte of keyboard buffer
		LDA #13				; <CR>
		STA $0270			; Second byte of keyboard buffer
		LDA #2				
		STA $9E				; # of chrs in keyboard buffer
		RTS				; Exit program

;-------------- Print post-filename parts of DLOAD sting

PComma:		LDA #44				; Comma
		JMP PRINT			; Print it and return
PrintU:		LDA #68				; "U"
		JMP PRINT			; Print it and return

PrintX:		LDA #0
		JSR INTOUT			; write INTEGER to screen
		RTS
 	
;-------------- TODO! Change Dir or Select Partition
SelPart:
ChangeDIR:	JMP IRefresh


;======================================================================================
; DIRECTORY MEMORY TRANSFER ROUTINES
;======================================================================================

;-- Swap Active

SwapActive:	JSR SaveActive
		LDA ACTIVE
		EOR #1				; Toggle Active Bit
		STA ACTIVE
		JSR LoadActive
		RTS				; Return to / routine!

;-- Save and Load Active

SaveActive:	JSR GetCurrent			; Get Current Directory ZP mem to .A and .X
		JSR SetSource
		JSR GetActive			; Get Active Mem pointer to .A and .X
		JSR SetDest
		JSR XferIt			; Do the Transfter
		RTS

LoadActive:	JSR GetCurrent			; Get Current Directory ZP mem to .A and .X
		JSR SetDest
		JSR GetActive			; Get Active Mem pointer to .A and .X
		JSR SetSource
		JSR XferIt
		RTS

;-- Get LEFT/RIGHT/CURRENT Memory Pointers

GetCurrent:	LDA #<DMEMHI
		LDX #0
		RTS

GetActive:	LDA ACTIVE
		BNE GetRight

GetLeft:	LDA #<LEFTMEM			; HI Location of LEFT storage memory
		LDX #>LEFTMEM			; LO (never zero)
		RTS

GetRight:	LDA #<RIGHTMEM			; HI Location of LEFT storage memory
		LDX #>RIGHTMEM			; LO
		RTS

;-- Set Source or Destination

SetSource:	STA ZP1
		STX ZP1+1
		RTS

SetDest:	STA ZP2
		STX ZP2+1
		RTS

;-- Transfer

XferIt:		LDY #0				; Number of bytes to transfer
XferLloop:	LDA (ZP1),Y			; Read it
		STA (ZP2),Y			; Write it
		INY				; Next byte
		CPY #11				; Are we at the end?
		BNE XferLloop			; No, go back for more
		RTS


;======================================================================================
; SET DIRECTORY MEMORY POINTER 
;======================================================================================
; Sets ZP3 pointer to start of active Directory buffer + 2
; (to skip drive and blocks used bytes to make traversing easier

SetDirPtr:	LDA DMEMHI			; HI Top Index Memory Pointer
		STA ZP3+1			; HI Work Pointer
		LDA #2				; LO starts at 2
		STA ZP3				; LO Work Pointer
		RTS


;======================================================================================
; SHOW DIRECTORY
;======================================================================================
; ZP3 pointer used to walk through directory memory. 
; Requires DTOP, DEND, DSEL to be set. When bottom is reached it is saved to DBOT.
; DTOP is the entry at the top of the list, just below the HEADER. DTOP>0!
; DENTRY points to current entry being looked at.
; DSEL is the enry that is SELECTED. 

ShowDirectory:	LDA DDEV			; Check if drive selected
		BEQ ShowIsOff			; No, forget about it
		LDA DEND			; Check END
		BNE ShowDstart			; Ok, good to go

ShowIsOff:	LDX DROW
		LDA DCOL
		LDY #18				; "No Dir loaded"
		JSR PrintAtNM			; Print it and end
		LDX DROW
		INX
		LDA DCOL
		LDY #16
		JMP PrintAtNM

ShowDstart:	JSR SetDirPtr			; Always start at HEADER!		
		LDY #0				; Top Entry (HEADER)
		STY DENTRY			; Point to Header
		STY DBOT			; BOTTOM Index
		STY DCOUNT			; Entry Counter (0 to end)

;-------------- TOP of Loop to Draw Entries
; Position Cursor to top of directory Area

SDloop:		LDA DROW			; Cursor Position for Top Left
		CLC
		ADC DCOUNT			; Add Entry Count
		TAX
		LDA DCOL			; Column for selected directory
		JSR CursorAt			; Move cursor to top of directory
		LDY #0				; Index for print loop
		STY ReverseFlag			; also, set <RVS> off

SDSelection:	LDA #32				; Assume <SPACE> character
		LDY DENTRY			; What is the entry#?
		CPY DSEL			; Is it SELECTED?
		BNE SDpoint			; No, skip ahead

;-------------- Selected Item - Save Pointer

		LDA ZP3				; Save pointer to SELECTED
		STA DSELMEM			; LO SELECTED memory pointer
		LDA ZP3+1			; HI Work Pointer
		STA DSELMEM+1			; HI SELECTED memory pointer

		LDA #62				; Yes, use ">" character
SDpoint:	JSR PRINT			; Print either SPACE or ">"

;-------------- Check if Entry is Marked

		LDY #18				; Offset in record for Marker
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

;-------------- Finished Entry. Check if HEADER entry

SDpos:		LDA DCOUNT			; Entry. Is it zero (HEADER)?
		BNE SDpos2			; No, skip ahead to next entry

;-------------- Calculate pointer for first normal file entry
; The HEADER is the first entry in the buffer, when we scoll down we want this to
; always be visible. We may have to skip over some entries so that the DTOP entry
; is directly below the HEADER.

		LDY DTOP			; Yes, set Top Entry
		STY DENTRY			; Make it Current Entry
		JSR SetDZ			; Set ZP3 pointer based on DTOP
		jmp SDnext2			; skip past DENTRY increment

;-------------- Move Work Pointer to next entry

SDpos2:		LDA ZP3				; LO Work Pointer
		CLC				; prep for Add
		ADC #DRECLEN			; Add record len to pointer
		STA ZP3				; LO Work Pointer
		BCC SDnext			; Did it cross page? No skip ahead
		INC ZP3+1			; Yes, inc page.

SDnext:		INC DENTRY			; Next Entry
SDnext2:	LDA DCOUNT			; Get it again		
		CMP #DIRHEIGHT			; Is it at end of allocated area?
		BEQ SDexit			; Yes, exit		
		CMP DEND			; Is it at the end of the directory?
		BEQ SDexit			; Yes, Exit

		INC DCOUNT			; Increment Count
		JMP SDloop			; Go back for another entry

SDexit:		LDA DENTRY			; Last ENTRY 
		STA DBOT			; save it for DBOT
		DEC DBOT			; adjust
		RTS


;======================================================================================
; SET CURSOR
;======================================================================================
; Uses CursorRow to set Screen line pointer ScrPtr for printing.
; CursorCol is used as offset. Messes up .A and .X registers
; ****We should be able to replace this routine when integrating with Editrom!

SetCursorAX:	STA CursorCol			; Set COL
		STX CursorRow			; Set ROW

SetCursor:	TYA
		PHA
		LDY CursorRow			; Get ROW as index
		LDA SCNWID			; Check Screen Width
		CMP #80				; Is it 80?
		BEQ SetC80			; Yes, skip over

		LDA SLA40_Lo,Y			; Get LO from Table
		LDX SLA40_Hi,Y			; Get HI from Table
		BNE SetCAX

SetC80:		LDA SLA80_Lo,Y			; Get LO from Table
		LDX SLA80_Hi,Y			; Get HI from Table

SetCAX:		STA ScrPtr			; Store in ScrPrt LO
		STX ScrPtr+1			; Store in ScrPrt HI
SetCDone:	PLA
		TAY
		RTS


;**************************************************************************************
; DISK ROUTINES
;**************************************************************************************
;======================================================================================
; SEND DOS COMMAND
;======================================================================================
; Send a string to the command channel.
; ZP1 points to a null terminated string.

send_cmd	LDA DDEV			; Device# for ACTIVE directory
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
; Print to STATUS line

GetDiskStatus:	LDY #17				; Disk Status ROW
		JSR ClearMsgNM			; Set the cursor to start of Row

		LDA DDEV			; Current Device#
		STA FA				; Set device number
		JSR TALK			; TALK
		LDA #$6f			; Secondary Address=15
		STA SA				; Store it
		JSR SECND			; Send secondary address

GS_NEXTCHAR	JSR ACPTR			; Read byte from IEEE bus
		CMP #$0D			; Is byte = CR?		
		BEQ GS_DONE			; yes, jump out
		JSR SCROUT			; no, write char to screen
		JMP GS_NEXTCHAR			; go back for more

GS_DONE		JSR UNTLK			; UNTALK
		RTS

;======================================================================================
; PREP ACTIVE DIRECTORY
;======================================================================================
; Set up Active directory. Set ACTIVE to 0 (LEFT) or 1 (RIGHT)

PrepDir:
		LDY ACTIVE			; Get Active directory
		BNE Prep1

Prep0:		LDA LDMEMHI			; HI Pointer to Directory Storage Address
		BNE PrepCommon

Prep1:		LDA RDMEMHI			; HI Pointer to Directory Storage Address

PrepCommon:	STA DMEMHI
		BNE LoadDirectory		; If Device>0 then ok

;-------------- Device not set

		LDY #16				; no device! tell them
		JSR ClearMsgNM			; print it
		RTS

;======================================================================================
; LOAD DIRECTORY
;======================================================================================
; Reads the directory into RAM pointed to by ZP1.
; FNADR pointer must point to directory string
; FNLEN must be set to length of string.-----------> To be fixed
; Counts # of directory entries to DCOUNT.

LoadDirectory:
		JSR ClearDArea
		LDA DUNIT			; Get the UNIT
		BNE SetD1			; If not zero, skip ahead

SetD0:		LDA #<FNDir0			; LO Pointer to Filename ("$0")		
		LDY #>FNDir0			; HI
		BNE PrepDev

SetD1:		LDA #<FNDir1			; LO Pointer to Filename ("$1")
		LDY #>FNDir1			; HI

PrepDev:	STA FNADR			; LO Filename Address pointer
		STY FNADR+1			; HI 

		LDY #2				; Directory string Length=2 "$0" or "$1"
		STY FNLEN			; Save it

		LDA DDEV			; LEFT Device#
		STA FA				; Set Device Number

		LDA DMEMHI			; HI Start of Directory buffer
		STA ZP1+1
		LDA #0				; Clear pointers
		STA ZP1				; LO Work Pointer

		STA DTOP			; Index of TOP entry
		STA DBOT			; Index of BOTTOMentry		
		STA DEND			; Index of END Entry
		STA DCOUNT			; Counter for directory display
  
		LDY #6				; Print "Reading.."
		JSR ClearKeyNM

		LDA #0				; 
		STA STATUS			; Clear Disk Status
		STA DCOUNT			; Zero the entry counter

		LDA #$60			; DATA SA 0
		STA SA				; Set Secondary Address
		JSR OPENI			; open file
		JSR TALK			; talk
		LDA SA				; Secondary Address
		JSR SECND			; Set Secondary Address
		LDX STATUS
		BNE LDError

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
		INC DEND			; End=End+1
		JMP LoadEntry			; No, Jump back for more lines

;-------------- Disk Error

LDError:	JSR CLSEI			; close file with $E0, unlisten
		LDA #0
		STA DCOUNT
		STA DEND
		RTS

;-------------- Listing is complete


StopListing:	JSR CLSEI			; close file with $E0, unlisten
		DEC DEND			; don't count "blocks free" entry
		LDY #5				; "entries:"
		JSR ClearMsgNM			; Print to Message line
		LDA #0				; HI # of entries = 0
		LDY DCOUNT			; LO count
		DEY				; Adjust for blocks free line
		JSR INTOUT			; write #blocks to screen
		LDY #1				; First filename (ie: skip header)
		STY DTOP			; Top of listing
		STY DSEL			; Select it

		JSR GetDiskStatus		; Show Disk Status

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


;**************************************************************************************
; Routines that are working
;**************************************************************************************
;======================================================================================
; Init Stuff
;======================================================================================
; Here we look at the BASIC pointers to figure out where we can store our data.
; STREND  pointer is the End of Array pointer which is the first address that is free
; FREETOP pointer is the End of RAM and is the last address we can use.
; All Buffers start on PAGE boundries to reduce code size.
;
; ** Testing phase - ignore FREETOP, max 255 entries which uses ~6K for directory.
; ** Buffers with no BASIC program loaded: LEFT  at $0D00, RIGHT at $2500
; ** Test on VICE with 8032 Model.
;
; Set LEFT  to Device 8, unit 0.
; Set RIGHT to Device 8, unit 1
InitStuff:
		LDA #<TAPEBUFFER
		STA ZP1
		LDA #>TAPEBUFFER
		STA ZP1+1

;-------------- Initialize Our Memory storage area

		LDY #190			; length of tape buffer
		LDA #0				; zero
InitMyMem:	STA (ZP1),Y			; write it
		DEY				; decrement index
		BNE InitMyMem			; Is it zero? no, go back for more

;-------------- Initialize Storage Pointers

InitDirMem:	LDX STREND+1			; HI End of Arrays
		INX				; Start on boundry of next page
		TXA
		CLC

!IF SCREENSAVE=1 {
		STX SCNMEMHI			; HI Screen Save Buffer
		ADC #SCREENPAGES		; Reserve 8 pages for 80-col screen
}
;-------------- Set LEFT and RIGHT buffer areas
; We reserve 24 pages each. This needs 6K per directory and a max of 255 entries

		STA LDMEMHI			; HI LEFT Directory Buffer
		ADC #24				; Reserve 24 pages (6K) for LEFT Dir
		STA RDMEMHI			; HI RIGHT Directory Buffer

;-------------- Set up Default LEFT (Active) Directory
;
; Since our memory is initialized to all zeros, we only need to set a few memory locations
; Since the Active directory defaults to the LEFT when we switch to the RIGHT these
; will be copied to the LEFT storage area

		LDA #8	;$FA			; Last device accessed
		STA DDEV			; LEFT  Device#
		LDA #DIRROW0			; Set up LEFT ROW,COL for Listing
		STA DROW			; LEFT Row
		LDA #DIRCOL0
		STA DCOL			; LEFT Col

;-------------- Set up RIGHT Directory
;
; We only need a pointer to the start of the directory buffer (set above) and the
; ROW/COL where it will be displayed. The device and unit# must be supplied by
; the user when they switch to the RIGHT.

		LDA #DIRROW1			; Set up RIGHT ROW,COL for Listing
		STA RDROW
		LDA #DIRCOL1
		STA RDCOL
		RTS


;======================================================================================
; SetDZ: Set ZP3 Pointer Calculaion using DTOP
;======================================================================================
; DTOP is the INDEX to the top of the ACTIVE directory list. All display is relative
; to this point. Rather than fiddle with adding or subtacting as you move up or down
; the listing we will just re-calculate the ZP3 pointer each time.
; DTOP is limited to one byte, so 256 maximum entries.

SetDZ:		
		LDA DMEMHI			; HI Page for Directory Buffer
		STA ZP3+1			; HI Work Pointer
		LDA #2				; LO = $00 + 2
		STA ZP3				; LO Work Pointer
		LDY DTOP			; Validate DTOP
		BNE SetDZTop			; >0 is ok
		INC DTOP			; If not, INCREMENT

SetDZTop:	LDY #0				; Counter for add loop
SetDZloop:	CPY DTOP			; Is counter at DTOP?
		BEQ SetDZX			; Yes, done
		CLC
		ADC #DRECLEN			; Directory Record Length 
		STA ZP3				; LO Work Pointer
		BCC SDZskip			; no page crossing
		INC ZP3+1			; HI Work Pointer
SDZskip:	INY				; increment counter
		BNE SetDZloop			; always Loop back
SetDZX:		RTS


;======================================================================================
; CONFIRM (Y/N) + Quit + AnyKey 
;======================================================================================
; Display confirmation then wait for ANY KEY - No validation done
; Answer returned in .A

AskQuit:	LDY #4					; "Quit?"
		JSR ClearKeyNM				; Clear KEY Row, Print the Message
AskSure:	LDY #5					; "Are you sure"
		JSR PrintNM				; Print the Message
AnyKey:		JSR GETIN				; Get keystroke to .A
		BEQ AnyKey				; None, so loop back
		RTS

;======================================================================================
; PROMPT FOR DRIVE DEVICE NUMBER
;======================================================================================
; Prompt for Drive Device Number. Accepts 0-9 or STOP=Abort. 0-7 are treated as 10-17.

AskDevice:	LDY #20					; Point to Prompt
		JSR ClearKeyNM				; Print the message
AskDloop	JSR AnyKey				; Get ANY Key
		CMP #3					; Is it Stop
		BEQ AskAbort				; Yes, abort
		CMP #48					; Is it "0"?
		BMI AskDloop				; less, not valid
		CMP #58					; "9"?
		BPL AskDloop				; greater, not valid
		CLC
		CMP #55					; Is it "7"?
		BPL NoAdj				; more, skip ahead
		ADC #11					; less, Add 10 (0 to 7 -> 10 to 17)
NoAdj:		SBC #48					; Subtract 48 - Device# in .A
		RTS 

;======================================================================================
; PROMPT FOR DRIVE UNIT NUMBER
;======================================================================================
; Prompt for Drive Unit Number. Accepts 0-1 or STOP=Abort

AskUnit:	LDY #21					; "Unit (0/1)?" Prompt
		JSR ClearKeyNM				; Print the message
AskUloop:	JSR AnyKey				; Get ANY Key
		STA SCREEN_RAM+73
		CMP #3					; Is it Stop?
		BEQ AskAbort				; Yes, abort
		CMP #48					; Is it "0"?
		BEQ AskUok				; less, not valid
		CMP #49					; "1"?
		BNE AskUloop				; greater, not valid

AskUok:		CLC
		SBC #47					; Subtract 48 - Device# in .A
		STA SCREEN_RAM+74
		RTS

AskAbort:	LDA #0					; Return a NULL
		RTS

;======================================================================================
; FIND SCREEN WIDTH
;======================================================================================
; This routine prints "<HOME><HOME><CLS><DOWN>" using the standard PET print routines.
; The <HOME><HOME> cancels any windowing, <CLS> Clears the screen and 40-col line links.
; The <DOWN> moves to the second line and the screen line pointer will be set, with the
; LO byte equal to 40 or 80 (points to $8028 or $8050).
; *** We can probably eliminate this when integrated into Editrom!

FindScnWidth:	LDY #1
		JSR PrintNM			; Use standard system print routine
		LDA ScrPtr			; LO of Start of Line Address
		STA SCNWID			; Save it
		RTS

!IF SCREENSAVE=1 {
;======================================================================================
; SAVE AND RESTORE SCREEN
;======================================================================================
; Save/Restore the screen to/from RAM pointed at by ZPSCREEN

SaveScreen:	LDA #>SCREEN_RAM		; HI Screen address
		LDY SCNMEMHI			; HI Screen Save Buffer
		BNE CopyScreen			; Setup for copy, then do it

RestScreen:	LDY #>SCREEN_RAM		; HI Screen address
		LDA SCNMEMHI			; HI Screen Save Buffer
		;JMP CopyScreen			; falls into the routine

;======================================================================================
; COPY SCREEN
;======================================================================================
; ZP1 Pointer HI byte in .A
; ZP2 Pointer HI byte in .Y - Pointer LO bytes are assumed zero.
; For now we assume 80 col screen and copy 8 pages (2K) of RAM.

CopyScreen:	STA ZP1+1			; HI Source Pointer
		STY ZP2+1			; HI Destination Pointer
		LDA #0				; Zero LO pointers
		STA ZP1				; LO Source Pointer
		STA ZP2				; LO Destination Pointer

		LDX #SCREENPAGES		; How many pages to copy? 8=2K

;======================================================================================
; MEMORY BLOCK COPY
;======================================================================================
; Copies .X pages from source ZP1 to destination ZP2

CopyMem:	LDY #0				; Start at offset 0

CopyMemLoop:	LDA (ZP1),Y			; Read byte
		STA (ZP2),Y			; Save it
		INY				; Next byte
		BNE CopyMemLoop			; Finished a page? No, go back for more
		INC ZP1 + 1			; Next Page
		INC ZP2 + 1			; Next page
		DEX				; Count the page
		BNE CopyMemLoop			; Back for more		
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

RestCursorPos:	LDA SCNSAVE1
		STA ScrPtr
		LDA SCNSAVE2
		STA ScrPtr+1
		LDA SCNSAVE3
		STA CursorCol
		LDA SCNSAVE4
		STA CursorRow
		RTS
}

;======================================================================================
; DRAW UI
;======================================================================================
; Draw Program Info and key help line

DrawUI:		JSR CLEARSCREEN			; Clear Screen

		LDX #PROGROW			; Program Info ROW
		LDY #2				; Message#
		JSR PrintRowNM			; Print Program Info

		LDX #1				; Row 1
		LDA #64				; Horizontal line chr
		JSR FillRow			; Fill the row with 

		LDX #KEYROW-1			; Row 23
		LDA #64				; Horizontal line chr
		JSR FillRow			; Fill the row with 

ShowKeyBar:	LDY #3				; Key Command Bar
		JSR ClearKeyNM			; Print Key Bar		
		RTS

;======================================================================================
; DATA TABLES
;======================================================================================

FNDir0:		!PET "$0",0			; Directory string
FNDir1:		!PET "$1",0			; Directory string


;======================================================================================
; PRINT ROUTINES
;======================================================================================
; CursorR:	.X=ROW, .A=0			Position to Start of ROW
; CursorAt:	.X=ROW, .A=COL			Position Cursor
; ClearRow:	.X=ROW, CHR=32			Clears the Entire ROW with SPACE.
; FillRow:	.X=ROW, .A=CHR			Clears the Entire ROW with CHR.
;-------------- NM: Numbered Message Print Routines
; ClearKeyNM:	.Y=Msg#, Row=KEYROW, Col=0	Clear then Print Msg# on KEYROW
; ClearMsgNM:	.Y=Msg#, Row=MSGROW, Col=0	Clear then Print Msg# on MSGROW
; PrintMsgNM:   .Y=Msg#, Row=MSGROW, Col=0	Print Msg# on MSGROW
; PrintRowNM:	.Y=Msg#, .X=Row,     Col=0	Print Msg# at Row,0
; PrintAtNM:	.Y=Msg#, .X=Row,     .A=Col	Print Msg# at Row,Col
; PrintNM:	.Y=Msg#, at Cursor Position	Print Msg#

;-------------- Move Cursor to specified ROW and COL
;		.X=Row, .A=Col

CursorR:	LDA #0				; COL=0
CursorAt:	JSR SetCursorAX			; Position Cursor to ROW=.X, COL=.A , .Y preserved
		RTS

;-------------- Clear or Fill specified ROW
; Clear:	.X=Row, CHR=32
; Fill:		.X=Row, .A=CHR (screencode)

ClearRow:	LDA #32				; <SPACE>
FillRow:	PHA				; Push CHR to Fill with in .A pushed to stack
		JSR CursorR			; Set the cursor (.A lost
		PLA				; Pull CHR from stack .A

;-------------- Clear the ROW

ClearR:		LDY #0				; Start Counter at cursor pos
ClearRloop:	STA (ScrPtr),Y			; Write character to screen
		INY				; Next position
		CPY SCNWID			; Is is end of line? 
		BNE ClearRloop			; No, go back for more
		RTS

ClearKeyNM:	LDX #KEYROW			; Clear Key Bar Row then print Msg#
		JMP PrintClear

ClearMsgNM:	LDX #MSGROW			; Clear Message Row then print Msg#

PrintClear:	STY TEMPMSG			; Save Msg#		
		JSR ClearRow			; Clear IT
		LDY TEMPMSG			; Re-load Msg#
		JMP PrintNM

;-------------- Print At ROW,0 or ROW,COL

PrintRowNM:	LDA #0				; Col=0
PrintAtNM:	JSR SetCursorAX			; Position Cursor. .Y preserved

;-------------- Print Numbered Message
		
PrintNM:	LDA NMAdLo,Y			; LO Address of Start of Message
		STA ZP2				; LO Pointer
		LDA NMAdHi,Y			; HI Address of Start of Message
		STA ZP2+1			; HI Pointer
		LDY #0
PNMloop		LDA (ZP2),Y			; Get the character to print
		BEQ PNMexit			; If zero then done
		JSR PRINT			; No, print it
		INY				; next character
		BNE PNMloop			; loop for more
PNMexit:	RTS


;======================================================================================
; Numbered Message Strings
;======================================================================================

NMAdLo:	!BYTE <NM0,<NM1,<NM2,<NM3,<NM4,<NM5,<NM6,<NM7		; 0-7
	!BYTE <NM8,<NM9,<NM10,<NM11,<NM12,<NM13,<NM14,<NM15	; 8-15
	!BYTE <NM16,<NM17,<NM18,<NM19,<NM20,<NM21		; 16-

NMAdHi:	!BYTE >NM0,>NM1,>NM2,>NM3,>NM4,>NM5,>NM6,>NM7		; 0-7
	!BYTE >NM8,>NM9,>NM10,>NM11,>NM12,>NM13,>NM14,>NM15	; 8-15
	!BYTE >NM16,>NM17,>NM18,>NM19,>NM20,>NM21		; 16-

NM0:	!BYTE 0						; For print@ 
NM1:	!BYTE 19,19,147,17,0				; <HOME><HOME><CLS><DOWN>
NM2:	!PET "stevebrowse 2021-04-20",0 		; Title text

NM3:	!PET RVS,"/"	,ROFF,"drive "			; Select Drive	
	!PET RVS,"home" ,ROFF,"top "			; Home
	!PET RVS,"[]"   ,ROFF,"page "			; Page Up/Down
	!PET RVS,"spc"  ,ROFF,"mark "			; Space
	!PET RVS,"rtn"  ,ROFF,"run/cd "			; Return
	!PET RVS,"q"    ,ROFF,"quit",0			; Quit

NM4:	!PET "quit: ",0					
NM5:	!PET "are you sure (y/n)?",0			; Are you Sure?
NM6:	!PET "reading...",0				; Reading...
NM7:	!PET 18,"[no directory      ]",146,0		; No Directory
NM8:	!PET "entries:",0				; Dir info
NM9:	!PET "load prg: ",0				; Load prompt
NM10:	!PET 147,"dL",0					; <CLS>dL	- Pre-filename
NM11:	!PET ",d",0					; Drive#  D0/D1
NM12:	!PET ",u",0					; Device# U8-U17
NM13:	!PET "copying...",0				; Copying...
NM14:	!PET "renaming...",13,0				; Rename...
NM15:   !PET "dev:xx unit:x ",0				; Device# and Unit# display
NM16:	!PET "hit '/' to select!",0			;
NM17:	!PET "ds: ",0					; Disk Status
NM18:	!PET "x",0					; unused
NM19:	!PET "                     ",0			; Blank dir entry

NM20:	!PET "device# ("				; Device selection prompt
	!PET RVS,"8",ROFF," "
	!PET RVS,"9",ROFF," 1"
	!PET RVS,"0",ROFF," 1"
	!PET RVS,"1",ROFF," 1"
	!PET RVS,"2",ROFF,")?",0
NM21:	!PET "unit (0,1)?",0				; Unit selection prompt

;======================================================================================
; SCREEN LINE ADDRESS TABLE
;======================================================================================
; These are tables representing the address of the start of each screen line for both
; 40 and 80 column wide screens. Screen width is determined at the start of the program
; and stored in SCNWID.
;
;---------- 40 characters wide
 
SLA40_Lo:	!byte $00,$28,$50,$78,$a0,$c8,$f0,$18,$40,$68,$90,$b8,$e0
		!byte $08,$30,$58,$80,$a8,$d0,$f8,$20,$48,$70,$98,$c0

SLA40_Hi:	!byte $80,$80,$80,$80,$80,$80,$80,$81,$81,$81,$81,$81,$81
		!byte $82,$82,$82,$82,$82,$82,$82,$83,$83,$83,$83,$83

;---------- 80 characters wide 

SLA80_Lo:	!byte $00,$50,$a0,$f0,$40,$90,$e0,$30,$80,$d0,$20,$70,$c0
		!byte $10,$60,$b0,$00,$50,$a0,$f0,$40,$90,$e0,$30,$80

SLA80_Hi:	!byte $80,$80,$80,$80,$81,$81,$81,$82,$82,$82,$83,$83,$83
		!byte $84,$84,$84,$85,$85,$85,$85,$86,$86,$86,$87,$87


;======================================================================================
; PADD to 4K to be able to load into VICE
;======================================================================================

!FILL $B000-*,0
