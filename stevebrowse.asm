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

SCREENSAVE	= 0			; Include Screen Save Options

;-------------- Constants

SCREENPAGES     = 8			; 2K size for 80 col
DIRWIDTH        = 21			; Width  of directory windows
DIRHEIGHT	= 18			; Height of directory windows
DIRROW0		= 2			; LEFT  Directory Top-Left position
DIRCOL0		= 0			; LEFT  
DIRROW1		= 2			; RIGHT Directory Top-Left position
DIRCOL1		= 26			; RIGHT
IDFLAG		= 0			; Flag to add ID string
MYDRIVE		= 8			; Default Drive Number, ie: 8
DRIVEUNIT	= 0			; Default Unit Numberm ie: 0 or 1
DRECLEN		= 23			; Record length for ram directory

DEBUGRAM	= SCREEN_RAM+24*80+70	; Address  of Debug       ROW

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

ZP1		= $EE			; Srce Pointer - Scn Cpy, String Print
ZP2		= $F0			; Dest Pointer - Scn Cpy
ZP3		= $F2			; Work Pointer - Directory display
;		= $F4			; FREE
;		= $F6			; FREE

;-------------- Memory Locations
; These locations should eventually move to BASIC program space so that ML code
; that uses the tape buffer will work.
; TODO: instead of copying individual bytes we should copy the entire block for
;       LEFT or RIGHT to/from ZP locations for current DIR memory.

TAPEBUFFER      = $027A			; 634   Tape buffer

SCREENMEM	= TAPEBUFFER
LEFTMEM		= TAPEBUFFER+10		; Start of LEFT  Memory Structure
RIGHTMEM	= TAPEBUFFER+20		; Start of RIGHT Memory Structure

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
		JSR SaveScreen			; Save the screen to memory
}
		JSR FindScnWidth		; Determine width of screen
		
		JSR DrawUI			; Draw the interface
		JSR PrepDir			; Load the ACTIVE directory		
		JSR ShowActiveDir		; Show the RIGHT directory

		JSR ILoopStat			; Interactive Loop with Stat update - Loops until Exit
BrowseDone:	RTS				; Exit Program

;TestMsg1:	!PET "[test1]",13,0
;TestMsg2:	!PET "[test2]",13,0
;TestMsg3:	!PET "[test3]",13,0

;======================================================================================
; Interactive 
;======================================================================================
; This is the main code that runs to interact with the program features. It reads
; keystrokes and acts on them, looping around until user exits.

;-------------- Command Key Table and Address Lo/HI Tables
;       	KEY:   Q     , DOWN  , UP  , HOME  , SPACE  , <     , >     , /      , CLS  , RETURN  ,NULL
CMDTABLE:	!BYTE  81    , 17    , 145 , 19    , 32     , 62    , 60    , 47     , 147  , 13      ,0
CMDLO:		!BYTE <IsQuit,<IsDown,<IsUp,<IsHome,<IsSpace,<IsPgDn,<IsPgUp,<IsSlash,<IsCLS,<IsRETURN
CMDHI:		!BYTE >IsQuit,>IsDown,>IsUp,>IsHome,>IsSpace,>IsPgDn,>IsPgUp,>IsSlash,>IsCLS,>IsRETURN

;-------------- Interactive Dispatch
Interact:
		INC DEBUGRAM			; DEBUG!!!!!!!!!!!!!!!!!!!!!

		JSR GETIN			; Get keystroke to .A
		BEQ Interact			; No press, go back

		STA DEBUGRAM + 1		; DEBUG!!!!!!!!!!!!!!!!!!

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
IRefresh:	JSR ShowDirectory		; Re-draw Directory
		JMP Interact
ILoopStat:	JSR ShowKeyBar			; Show Key Help line
ILoop:		JMP Interact			; Loop back for more

ShowPET:
!IF SCREENSAVE=1{
		JSR RestoreScreen		; Restore the screen
		JSR RestoreCursorPos		; Restore cursor position
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
		CPY DTOP			; Is it at TOP?
		BEQ YesTop			; Yes, try to scroll up. skip ahead
		DEC DSEL			; No, it's safe to move to prev entry
		JMP IRefresh			; Redraw directory

YesTop:		LDY DTOP			; Get TOP
		BEQ ILoop			; Is it Zero? no moving up. skip

		DEC DTOP			; Move TOP Up
		DEC DSEL			; Move SELECTED Up
NoUp:		JMP IRefresh			; Refresh
		

;-------------- Perform DOWN		

IsDown:		LDY DSEL			; SELECTED entry
		CPY DEND			; Is is at the END?
		BEQ ILoop			; Yes, can't go down. abort.

		INC DEBUGRAM+2			; DEBUG!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		CPY DBOT			; Is it at BOTTOM
		BEQ IsDownOK			; Yes, need to scroll
		INC DSEL			; No, just inc SELECTED
		BNE IRefresh		

IsDownOK	INC DSEL			; Move SELECTED down
		INC DTOP
		JMP IRefresh

;-------------- Perform HOME

IsHome:		LDY #0				; First Entry
		STY DTOP			; Set TOP
		INY
		STY DSEL			; Set SELECTED
		JSR DrawUI
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

IsPgDn:		CLC				;-- Check if END is less than PgDn
		LDA DEND			; Directory END
		SBC DBOT			; Subtract BOTTOM
		CMP #DIRHEIGHT			; Is it more than height of display?
		BPL OkDown			; Yes, do it

NoDown:		CLC				;-- Set TOP to be one page from BOT
		LDA DEND			; No, get END
		SBC #DIRHEIGHT			; Subtract height of display
		STA DTOP			; Use it for TOP
		STA DSEL			; Put selected at TOP too
		JMP IRefresh

OkDown:		LDA DTOP			;-- Set TOP to be PgDn
		ADC #DIRHEIGHT			; Add height of display
		STA DTOP			; Set it
		LDA DSEL			; Get SELECTED
		ADC #DIRHEIGHT			; Add height of display
		STA DSEL			; Set it
		JMP IRefresh

;-------------- Perform Page UP

IsPgUp:		CLC
		LDA DTOP			; Directory TOP
		CMP #DIRHEIGHT			; Height of display box
		BPL IPUfull			; Is it more? Yes, jump full page
		LDA #0				; No, just go to FIRST entry
		STA DTOP			; Set it
		JMP IRefresh

IPUfull:	SBC #DIRHEIGHT			; Subtract height of display box
		STA DTOP			; Set DTOP
		LDA DSEL			; SELECTED
		SBC #DIRHEIGHT			; Subtract height of display box
		STA DSEL			; Save it
		JMP IRefresh			; Re-draw list

;-------------- Perform Select Directory LEFT/RIGHT

IsSlash:
;		JMP ILoop
;		LDA ACTIVE			; Active Directory (0=LEFT,1=RIGHT)
;		CMP #0				; Left Side?
;		BNE RightAct			; No, Right is active

LeftAct:	;backup current to left
RightAct:	;backup current to right


;-------------- Perform Load new Drive/Unit

IsCLS:		JSR AskDevice			; Ask and input Device#. Returns 8-15 in .A
		BEQ CLSabort
		STA DDEV			; Store Device#
		JSR AskUnit			; Ask and input Unit#. Returns 0 or 1
		STA DUNIT			; Store Unit#
		JSR PrepCommon			; Get the new directory
CLSabort:	JMP ILoopStat			; Return and clear status line


;-------------- Perform RETURN

IsRETURN:
		LDY #19				; Index to FILETYPE Character
		LDA (DSELMEM),Y			; Get the FILETYPE		
		CMP #80				; "P"? - PRG file
		BEQ LoadPRG			; Yes, Load the file
		CMP #67				; "D"? - DIR file
		BEQ ChangeDIR			; Yes, Change Directory
		JMP IRefresh

;-------------- Do File Load

LoadPRG:	LDY #9				; "Load PRG?"	
		JSR ClearKeyNM			; Print It
		JSR AskSure			; Are you sure?
		CMP #89				; Is it "Y"?
		BEQ LPgo			; Yes
		JMP ILoopStat

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

LPpost:		LDY #11				; Post-filename string <QUOTE>,D0 ON U8<CR><CR>
		JSR PrintNM			; Print the string

		LDA #19				; Home
		STA $026F			; First byte of keyboard buffer
		LDA #13				; <CR>
		STA $0270			; Second byte of keyboard buffer
		LDA #2				
		STA $9E				; # of chrs in keyboard buffer
		RTS				; Exit program
 	
;-------------- Do Change Dir

ChangeDIR:
		JMP IRefresh

;======================================================================================
; DEBUG TO SCREEN !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;======================================================================================
; Visible display of certain memory locations on screen

DIRDEBUG:	LDY DTOP
		STY DEBUGRAM+4
		LDY DSEL
		STY DEBUGRAM+5
		LDY DBOT
		STY DEBUGRAM+6
		LDY DEND
		STY DEBUGRAM+7
		RTS

;======================================================================================
; SetDZ: Set ZP3 Pointer Calculaion using DTOP
;======================================================================================
; DTOP is the INDEX to the top of the ACTIVE directory list. All display is relative
; to this point. Rather than fiddle with adding or subtacting as you move up or down
; the listing we will just re-calculate the ZP3 pointer each time.
; DTOP is limited to one byte, so 256 maximum entries.

SetDZ:		LDY DTOP			; Directory TOP. Counter for add loop
		LDA DMEMHI			; HI Page for Directory Buffer
		STA ZP3+1			; HI Work Pointer
		LDA #2				; LO = $00 + 2
		STA ZP3				; LO Work Pointer
SetDZloop:	CPY #0				; Are we at top?
		BEQ SetDZX			; Yes, done
		CLC
		ADC #DRECLEN			; Directory Record Length 
		STA ZP3				; LO Work Pointer
		BCC SDZskip			; no page crossing
		INC ZP3+1			; HI Work Pointer
SDZskip:	DEY				; 
		BNE SetDZloop			; Loop back
SetDZX:		RTS
		

;======================================================================================
; PROMPT FOR DRIVE DEVICE NUMBER
;======================================================================================
; Prompt for Drive Device Number. Accepts 0-9 or STOP=Abort. 0-7 are treated as 10-17.

AskDevice:	LDY #3					; Point to Prompt
		JSR ClearKeyNM				; Print the message
AskDloop	JSR AnyKey				; Get ANY Key
		STA SCREEN_RAM+70			; DEBUG!!!!!!!!!!!!!!!!!!!		
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
		STA SCREEN_RAM+71			; DEBUG!!!!!!!!!!!!!!!!!!!
		RTS 

;======================================================================================
; PROMPT FOR DRIVE UNIT NUMBER
;======================================================================================
; Prompt for Drive Unit Number. Accepts 0-1 or STOP=Abort

AskUnit:	LDY #4					; "Unit (0/1)?" Prompt
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
; CONFIRM (Y/N) + AnyKey 
;======================================================================================
; Display confirmation then wait for ANY KEY - No validation done
; Answer returned in .A

AskQuit:	LDY #2					; "Quit?"
		JSR ClearKeyNM				; Clear KEY Row, Print the Message
AskSure:	LDY #13					; "Are you sure"
		JSR PrintNM				; Print the Message
AnyKey:		JSR GETIN				; Get keystroke to .A
		BEQ AnyKey				; None, so loop back
		RTS

;======================================================================================
; SETUP FOR LEFT/RIGHT DIRECTORY
;======================================================================================
; Displays the directory in the proper location

ShowActiveDir:
		LDA DDEV			; Check if drive selected
		BNE ShowDirectory		; No, ok
		RTS				; Yes, nothing to show

;ShowLeftDir:	LDA LDMEMHI			; HI Pointer to LEFT Directory Buffer
;		JSR SetDirMem			; Set Memory Pointers
;		LDA #DIRCOL0			; LEFT directory column
;		LDY LDEND			; Index of LEFT last entry
;		JMP ShowTest
;
;ShowRightDir:	LDA RDMEMHI			; HI Pointer to LEFT Directory Buffer
;		JSR SetDirMem			; Set Memory Pointers
;		LDA #DIRCOL1			; LEFT directory column
;		LDY RDEND			; Index of LEFT last entry
;
;ShowTest:	STA DCOL			; Directory Column
;		STY DEND			; Index of last entry
;		CPY #0				; Is DEND zero?
;		BNE ShowDirectory		; No, good to go
;		RTS				; Yes, nothing to show!
;
;SetDirMem:	STA DMEMHI			; HI Top Index Memory Pointer
;		STA ZP3+1			; HI Work Pointer
;		LDA #2				; LO starts at 2
;		STA ZP3				; LO Work Pointer
;		RTS


;======================================================================================
; SHOW DIRECTORY
;======================================================================================
; ZP3 pointer used to walk through directory memory. 
; Requires DTOP, DEND, DSEL to be set. When bottom is reached it is saved to DBOT. 

ShowDirectory:	JSR SetDZ			; Set ZP3 pointer based on DTOP
		JSR DIRDEBUG			; DEBUG!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		LDY DTOP			; Top Entry
		STY DENTRY			; Make it Current Entry
		STY DBOT			; BOTTOM Index
		LDY #0				; Count=1 (first entry displayed)
		STY DCOUNT			; Entry Counter
;		STY DEND			; END Index

;-------------- Position Cursor

SDLoop:		LDA DROW			; Cursor Position for Top Left
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
SDpoint:	JSR PRINT			; Print It!

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

SDnext:		INC DENTRY			; Next Entry
		INC DBOT			; Update BOTTOM
		INC DCOUNT			; Increment Count
		LDA DCOUNT			; Get it again		
		CMP #DIRHEIGHT			; Is it at end of box?
		BEQ SDexit			; Yes, exit		
		CMP DEND			; Is it at the end of the directory?
		BNE SDLoop			; No, Go back for another entry
SDexit:		DEC DBOT			; adjust
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

GetDiskStatus:
		LDX #DSROW			; Disk Status ROW
		JSR CursorR			; Set the cursor to start of Row

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
; Set up Active directory
; Set ACTIVE to 0 (LEFT) or 1 (RIGHT)

PrepDir:	LDY ACTIVE			; Get Active directory
		BNE Prep1

Prep0:
		LDA LDMEMHI			; HI Pointer to Directory Storage Address
		STA DMEMHI			;
;		LDA LDDEV			; Device#
;		STA DDEV
;		LDA LDUNIT			; Unit#
;		STA DUNIT				

		JMP PrepCommon

Prep1:		LDA RDMEMHI			; HI Pointer to Directory Storage Address
		STA DMEMHI			; HI
		LDA RDDEV			; Device#
		STA DDEV
		LDA RDUNIT			; Unit#
		STA DUNIT				

PrepCommon:	LDA DUNIT			; Get the UNIT
		BNE SetU1			; If not zero, skip ahead

SetU0:		LDA #<FNDir0			; LO Pointer to Filename ("$0")
		STA FNADR			; LO Filename Address pointer
		LDA #>FNDir0			; HI
		STA FNADR+1			; HI 
		JMP PrepDev

SetU1:		LDA #<FNDir1			; LO Pointer to Filename ("$1")
		STA FNADR			; LO Filename Address pointer
		LDA #>FNDir1			; HI
		STA FNADR+1			; HI 

PrepDev:	LDA DDEV			; LEFT Device#
		STA FA				; Set Device Number
CheckValDir:	BNE LoadDirectory		; If Device>0 then ok

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

;-------------- Prep
LoadDirectory:
		LDA DMEMHI			; HI Start of Directory buffer
		STA ZP1+1
		LDA #0				; Clear pointers
		STA ZP1				; LO Work Pointer
		STA DTOP			; Index of TOP entry
		STA DBOT			; Index of BOTTOMentry		
		STA DEND			; Index of END Entry
		STA DCOUNT			; Counter for directory display
		LDA #1				; Make entry after header SELECTED
		STA DSEL			; Index of Selected Entry
  
		LDY #6				; Print "Reading.."
		JSR ClearKeyNM

		LDY #2				; Length=2 "$0" or "$1" DEBUG!!!!!!!!!!!!!
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
		JSR $E015			; Clear screen
		JSR GetDiskStatus		; Show error
		LDA #0
		STA DCOUNT
		STA DEND
		JSR AnyKey
		RTS

;-------------- Listing is complete


StopListing:	JSR CLSEI			; close file with $E0, unlisten
		LDY #5				; "entries:"
		JSR ClearMsgNM			; Print to Message line
		LDA #0				; HI # of entries = 0
		LDY DCOUNT			; LO count
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

RestoreScreen:	LDY #>SCREEN_RAM		; HI Screen address
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
}

;======================================================================================
; DRAW UI
;======================================================================================
; Draw Program Info and key help line

DrawUI:		JSR CLEARSCREEN			; Clear Screen

		LDX #PROGROW			; Program Info ROW
		LDY #14				; Message#
		JSR PrintRowNM			; Print Program Info

		LDX #1				; Row 1
		LDA #64				; Horizontal line chr
		JSR FillRow			; Fill the row with 

		LDX #KEYROW-1			; Row 23
		LDA #64				; Horizontal line chr
		JSR FillRow			; Fill the row with 

ShowKeyBar:	LDY #12				; Key Command Bar
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

;--------------
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
	!BYTE <NM16						; 16-

NMAdHi:	!BYTE >NM0,>NM1,>NM2,>NM3,>NM4,>NM5,>NM6,>NM7		; 0-7
	!BYTE >NM8,>NM9,>NM10,>NM11,>NM12,>NM13,>NM14,>NM15	; 8-15
	!BYTE >NM16						; 16-

NM0:	!BYTE 0						; For print@ 
NM1:	!BYTE 19,19,147,17,0				; <HOME><HOME><CLS><DOWN>
NM2:	!PET "quit: ",0					
NM3:	!PET "device# ("				; Device selection prompt
	!PET RVS,"8",ROFF," "
	!PET RVS,"9",ROFF," 1"
	!PET RVS,"0",ROFF," 1"
	!PET RVS,"1",ROFF," 1"
	!PET RVS,"2",ROFF,")?",0

NM4:	!PET "unit (0,1)?",0				; Unit selection prompt
NM5:	!PET "entries:",0				; Dir info
NM6:	!PET "reading...",0				; Reading...
NM7:	!PET "copying...",0				; Copying...
NM8:	!PET "renaming...",13,0				; Rename...
NM9:	!PET "load prg: ",0				; Load prompt
NM10:	!PET 147,"dL",0					; <CLS>dL<QUOTE> - Pre-filename
NM11:	!PET ",d0,u8",0					; d0,u8          - Post-filename

NM12:	!PET RVS,"cls"   ,ROFF,"drive "			; Select Drive
	!PET RVS,"/"     ,ROFF,"switch "		; Switch Sides
	!PET RVS,"crsr"  ,ROFF,"sel "			; Cursor
	!PET RVS,"home"  ,ROFF,"top "			; Home
	!PET RVS,"<>"    ,ROFF,"page "			; Page Up/Down
	!PET RVS,"spc"   ,ROFF,"mark "			; Space
	!PET RVS,"rtn"   ,ROFF,"run/cd "		; Return
	!PET RVS,"q"     ,ROFF,"quit",0	; Quit

NM13:	!PET "are you sure (y/n)?",0			; Are you Sure?
NM14:	!PET "stevebrowse 2021-04-17",0 		; Title text
NM15:   !PET "dev:xx unit:x ",0				; Device# and Unit# display
NM16:	!PET "no drive selected. press <cls>!",0	;

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
