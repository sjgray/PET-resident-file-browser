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

!SOURCE "membasic4.asm"				; BASIC calls
!SOURCE "memchips.asm"				; IO Chips
!SOURCE "memkernal.asm"				; KERNAL calls
!SOURCE "memlow.asm"				; Low Memory locations
!SOURCE "memzeropage.asm"			; Zero Page locations

PRINT		= $FFD2				; Print a character routine
CLEARSCREEN	= $E015				; Clear the screen

;-------------- Program Options

SCREENSAVE	= 0				; Include Screen Save Options

;-------------- Constants

SCREENPAGES     = 8				; 2K size for 80 col
DIRWIDTH        = 16				; Width of directory window
DIRHEIGHT	= 20				; Height of directory window
DIRROW		= 0				; Row for first directory line
DIRCOL0		= 0				; Column for LEFT directory
DIRCOL1		= 26				; Column for LEFT directory
IDFLAG		= 0				; Flag to add ID string
MYDRIVE		= 8				; Default Drive Number, ie: 8
DRIVEUNIT	= 0				; Default Unit Numberm ie: 0 or 1
DRECLEN		= 23				; Record length for ram directory

DEBUGRAM	= SCREEN_RAM+24*80+70		; Address  of Debug       ROW

PROGROW		= 20				; Location of Progress    ROW
DSROW		= 21				; Location of DiskST/File ROW
MSGROW		= 22				; Location of Info Status ROW
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


ACTIVE		= $B1				; Active Directory# 0 or 1
SCNWID		= $B2				; Current Screen Width 
;CLMARGIN	= $B3				; Cursor LEFT Margin

;-- Details for Directory being Browsed
; The order of these 7 locations must match the LEFT and RIGHT details!

DMEMHI		= $B4				; +0 HI Directory Buffer Pointer
DTOP		= $B5				; +1 Index of Top entry
DSEL		= $B6				; +2 Index of Selected Entry
DBOT		= $B7				; +3 Index of Bottom Entry
DEND		= $B8				; +4 Index of Last Entry
DCOL		= $B9				; +5 Left column for directory listing
DSELMEM		= $BA				; +6 LO Selected Item Memory Pointer
;		= $BB				; +7 HI Selected Item Memory Pointer

DCOUNT		= $BC				; Counter for directory display
DENTRY		= $BD				; Index of current Entry
CMDKEY		= $BE				; Command Keypress
CMDJUMP		= $BF				; LO Command Jump
;		= $C0				; HI Command Jump
;		= $C1				; FREE
;		= $C2				; FREE
;		= $C3				; FREE

;-- $ED-$F7 not used in 80-column machines. Safe to use here for now?

ZP1		= $EE				; Srce Pointer - Scn Cpy, String Print
ZP2		= $F0				; Dest Pointer - Scn Cpy
ZP3		= $F2				; Work Pointer - Directory display
;		= $F4				; FREE
;		= $F6				; FREE

;-------------- Memory Locations
; These locations should eventually move to BASIC program space so that ML code
; that uses the tape buffer will work.
; TODO: instead of copying individual bytes we should copy the entire block for
;       LEFT or RIGHT to/from ZP locations for current DIR memory.

TAPEBUFFER      = $027A				; 634   Tape buffer

SCREENMEM	= TAPEBUFFER
LEFTMEM		= TAPEBUFFER+10			; Start of LEFT  Memory Structure
RIGHTMEM	= TAPEBUFFER+20			; Start of RIGHT Memory Structure

SCNSAVE1	= SCREENMEM			; Cursor Pointer LO
SCNSAVE2	= SCREENMEM+1			; Cursor Pointer HI
SCNSAVE3	= SCREENMEM+2			; Cursor Column/offset
SCNSAVE4	= SCREENMEM+3			; Cursor Row
SCNMEMHI	= SCREENMEM+4			; LO Pointer to Screen Buffer
;
CMDMEM		= SCREENMEM+6			; LO Command String Buffer Pointer
;		= SCREENMEM+7			; HI Command String Buffer Pointer

;-- Details for LEFT/RIGHT Directory
; These locations must match the ones in Zero Page (starting from DMEM)

LDIRACTIVE	= LEFTMEM			; LEFT Active Status
LDIRMEMHI	= LEFTMEM+1			; LEFT HI Directory Buffer Page
LDIRTOP		= LEFTMEM+2			; LEFT Index of Top Entry
LDIRSEL		= LEFTMEM+3			; LEFT Index of Selected entry
LDIRBOT		= LEFTMEM+4			; LEFT Index of Bottom entry
LDIREND		= LEFTMEM+5			; LEFT Index of Last Entry
LSELMEM		= LEFTMEM+6			; LEFT LO Selected Item Memory 
;		= LEFTMEM+7			; LEFT HI Selected Item Memory 

RDIRACTIVE	= RIGHTMEM			; RIGHT Active Status
RDIRMEMHI	= RIGHTMEM+1			; RIGHT HI Directory Buffer Page
RDIRTOP		= RIGHTMEM+2			; RIGHT Index of Top Entry
RDIRSEL		= RIGHTMEM+3			; RIGHT Index of Selected entry
RDIRBOT		= RIGHTMEM+4			; RIGHT Index of Bottom entry
RDIREND		= RIGHTMEM+5			; RIGHT Index of Last Entry
RSELMEM		= RIGHTMEM+6			; RIGHT LO Selected Item Memory 
;		= RIGHTMEM+7			; RIGHT HI Selected Item Memory 


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

		JSR LoadLeftDir			; Load the LEFT directory		
		JSR LoadRightDir		; Load the RIGHT directory
		JSR ShowRightDir		; Show the RIGHT directory
		JSR ShowLeftDir			; Show the LEFT directory

		JSR GetDiskStatus		; Get Disk Status

		JSR Interact			; The Main Code. Loops until Exit.
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
		INY				; Index for next key
		CMP #0				; End of Key List?
		BEQ Interact			; Yes, key not found. Get another KEY
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
ILoop:		JMP Interact			; Loop back for more

ILoopStat:	JSR ClearMsg			; Clear Status Line
		JMP Interact

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

IsQuit:		JSR AskSure			; Prompt to Exit
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
		JMP ILoop
		LDA ACTIVE			; Active Directory (0=LEFT,1=RIGHT)
		CMP #0				; Left Side?
		BNE RightAct			; No, Right is active
LeftAct:
		;backup current to left

RightAct:
		;backup current to right


;-------------- Perform Load new Drive/Unit

IsCLS:
		JSR AskDevice			; Ask and input Device#. Returns 8-15 in .A
		BEQ CLSabort
		JSR AskUnit			; Ask and input Unit#. Returns 0 or 1
CLSabort:	JMP ILoopStat			; Return and clear status line


;-------------- Perform RETURN

IsRETURN:
		LDY #19				; Index to FILETYPE Character
		LDA (DSELMEM),Y			; Get the FILETYPE
		STA DEBUGRAM+9			; DEBUG!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		CMP #80				; "P"? - PRG file
		BEQ LoadPRG			; Yes, Load the file
		CMP #67				; "D"? - DIR file
		BEQ ChangeDIR			; Yes, Change Directory
		JMP IRefresh

;-------------- Do File Load

LoadPRG:	LDY #1		
		JSR PrintNM			; Are you sure?
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
; SetDZ: Set ZP3 Pointer Calculaion using DTOP
;======================================================================================
; DTOP is the INDEX to the top of the visible directory list. All display is relative
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
		JSR PrintMsgNM				; Print the message
AskDloop	JSR AnyKey				; Get ANY Key		
		CMP #3					; Is it Stop
		BEQ AskAbort				; Yes, abort
		CMP #48					; Is it "0"?
		BMI AskDloop				; less, not valid
		CMP #57					; "9"?
		BPL AskDloop				; greater, not valid
		CLC
		CMP #56					; Is it "7"?
		BPL NoAdj				; more, skip ahead
		ADC #10					; less, Add 10 (0 to 7 -> 10 to 17)
NoAdj:		SBC #48					; Subtract 48 - Device# in .A
		RTS 

;======================================================================================
; PROMPT FOR DRIVE UNIT NUMBER
;======================================================================================
; Prompt for Drive Unit Number. Accepts 0-1 or STOP=Abort

AskUnit:	LDY #4					; Point to Prompt
		JSR PrintNM				; Print the message
AskUloop:	JSR AnyKey				; Get ANY Key		
		CMP #3					; Is it Stop
		BEQ AskAbort				; Yes, abort
		CMP #48					; Is it "0"?
		BMI AskUloop				; less, not valid
		CMP #49					; "1"?
		BPL AskUloop				; greater, not valid
		CLC
		SBC #48					; Subtract 48 - Device# in .A
		RTS

AskAbort:	LDA #0					; Return a NULL
		RTS

;======================================================================================
; CONFIRM (Y/N) + AnyKey 
;======================================================================================
; Display confirmation then wait for ANY KEY - No validation done
; Answer returned in .A

AskSure:	JSR ClearMsg
		LDY #2					; "Quit?"
		JSR PrintMsgNM				; Print the Message
AnyKey:		JSR GETIN				; Get keystroke to .A
		BEQ AnyKey				; None, so loop back
		RTS

;======================================================================================
; SETUP FOR LEFT/RIGHT DIRECTORY
;======================================================================================
; Displays the directory in the proper location

ShowLeftDir:	LDA LDIRMEMHI			; HI Pointer to LEFT Directory Buffer
		JSR SetDirMem			; Set Memory Pointers
		LDA #DIRCOL0			; LEFT directory column
		LDY LDIREND			; Index of LEFT last entry
		JMP ShowTest

ShowRightDir:	LDA RDIRMEMHI			; HI Pointer to LEFT Directory Buffer
		JSR SetDirMem			; Set Memory Pointers
		LDA #DIRCOL1			; LEFT directory column
		LDY RDIREND			; Index of LEFT last entry

ShowTest:	STA DCOL			; Directory Column
		STY DEND			; Index of last entry
		CPY #0				; Is DEND zero?
		BNE ShowDirectory		; No, good to go
		RTS				; Yes, nothing to show!

SetDirMem:	STA DMEMHI			; HI Top Index Memory Pointer
		STA ZP3+1			; HI Work Pointer
		LDA #2				; LO starts at 2
		STA ZP3				; LO Work Pointer
		RTS

;======================================================================================
; DEBUG TO SCREEN
;======================================================================================

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
; SHOW DIRECTORY
;======================================================================================
; ZP3 pointer used to walk through directory memory.
; Count displayed lines in DCOUNT.
; When bottom is reached save to DBOT.
; DSEL is the currently selected entry.

ShowDirectory:	JSR SetDZ			; Set ZP3 pointer based on DTOP
		JSR DIRDEBUG			; DEBUG!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		LDY DTOP			; Top Entry
		STY DENTRY			; Make it Current Entry
		STY DBOT			; BOTTOM Index
		LDY #0				; Count=1 (first entry displayed)
		STY DCOUNT			; Entry Counter
;		STY DEND			; END Index

;-------------- Position Cursor

SDLoop:		LDA #DIRROW			; Cursor Position for Top Left
		CLC
		ADC DCOUNT			; Add Entry Count
		LDY DCOL			; Column for selected directory
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

SDnext:
		INC DENTRY			; Next Entry
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

SetCursor:	TYA
		PHA
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
		LDY #0				; Column 0
		JSR CursorAt			; Set the cursor

		LDA MYDRIVE			; Current Device#
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
; LOAD LEFT/RIGHT DIRECTORY
;======================================================================================
; Sets up pointers for LEFT/RIGHT directory
; Initialize pointers for TOP,SEL,END,COUNT

LoadLeftDir:
		LDA LDIRMEMHI			; HI Pointer to Directory Storage Address
		STA ZP1+1			; HI Pointer

		LDA #<FNDir0			; LO Pointer to Filename ("$0")
		STA FNADR			; LO Filename Address pointer
		LDA #>FNDir0			; HI
		STA FNADR+1			; HI 

		JSR LoadDirectory		; Read directory to memory, count entries

;---- replace these with copy loop!
		LDA DTOP			; Now we copy results back to LEFT DIR
		STA LDIRTOP			; Start at Top of List
		LDA DSEL
		STA LDIRSEL			; Selected at top
		LDA DCOUNT
		STA LDIREND			; End of directory
		RTS

LoadRightDir:
		LDA RDIRMEMHI			; HI Pointer to Directory Storage Address
		STA ZP1+1			; HI

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
; FNADR pointer must point to directory string
; FNLEN must be set to length of string.-----------> To be fixed
; Counts # of directory entries to DCOUNT.

;-------------- Prep
LoadDirectory:
		LDA #0				; Clear pointers
		STA ZP1				; LO Work Pointer
		STA DTOP			; Index of TOP entry
		STA DBOT			; Index of BOTTOMentry		
		STA DEND			; Index of END Entry
		STA DCOUNT			; Counter for directory display
		LDA #1				; Make entry after header SELECTED
		STA DSEL			; Index of Selected Entry
  
		LDY #6				; Print "Reading.."
		JSR PrintMsgNM

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

;-------------- Listing is complete

StopListing:	JSR CLSEI			; close file with $E0, unlisten
		LDY #5		
		JSR PrintNM
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

InitStuff:	LDX #0				; Make our data start on page boundry
		STX ACTIVE			; Active Directory Listing (0/1)
		LDX STREND+1			; HI End of Arrays
		INX				; Start on boundry of next page
		TXA
		CLC

!IF SCREENSAVE=1 {
		STX SCNMEMHI			; HI Screen Save Buffer
		ADC #8				; Reserve 8 pages for 80-col screen
}
		STA LDIRMEMHI			; HI LEFT Directory Buffer
		ADC #24				; Reserve 24 pages (6K) for LEFT Dir
		STA RDIRMEMHI			; HI RIGHT Directory Buffer
		
		LDA #8				; Default Drive=8
		STA MYDRIVE			; Set it
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

CopyLoop:	LDA (ZP1),Y			; Read byte
		STA (ZP2),Y			; Save it
		INY				; Next byte
		BNE CopyLoop			; Finished a page? No, go back for more
		INC ZP1 + 1			; Next Page
		INC ZP2 + 1			; Next page
		DEX				; Count the page
		BNE CopyLoop			; Back for more		
		RTS

!IF SCREENSAVE=1 {
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

		LDX #HELPROW-1			; Row 23
		LDY #104			; Horizontal line chr
		JSR FillRow			; Fill the row with 

		LDX #HELPROW
		LDY #12				; Key Command Bar
		JSR PrintRowNM			; Print Key Bar
		RTS

;======================================================================================
; DATA TABLES
;======================================================================================

KeyBar:		!BYTE HELPROW,0					; HELP ROW

FNDir0:		!PET "$0",0					; Directory string
FNDir1:		!PET "$1",0					; Directory string


;======================================================================================
; PRINT ROUTINES
;======================================================================================
; CursorAt:	.A=ROW, .Y=COL			Positions cursor.
; PrintMsg:	.A=LO,  .Y=HI addr of string
; ClearMsg:	NONE				Clears Status line with SPACES
; ClearRow:	.A=ROW, 			Clears the entire ROW with SPACE.
; FillRow:	.A=ROW, .Y=CHR			Clears the entire ROW with CHR.

;-------------- Numbered Message Print Routines

; PrintMsgNM:   .Y=Msg#, Row=MSGROW, Col=0	Print Message on MSGROW
; PrintRowNM:	.Y=Msg#, .X=Row#,    Col=0	Print String# at Row,0
; PrintAtNM:	.Y=Msg#, .X=Row#,    .A=Col#	Print String# at Row,Col
; PrintNM:	.Y=Msg#, at Cursor Position	Print String#

CursorAt:	STA CursorRow			; Set ROW
		STY CursorCol			; Set COL
		JSR SetCursor			; Position Cursor
		RTS

ClearMsg:	LDA #MSGROW			; Message ROW
ClearRow:	LDY #32				; <SPACE>
FillRow:	JSR CursorAt			; Set the cursor
		LDA CursorCol			; Put CHR to A
		LDY #0				; Start Counter at cursor pos
ClearRloop:	STA (ScrPtr),Y			; Write character to screen
		INY				; Next position
		CPY SCNWID			; Is is end of line? 
		BNE ClearRloop			; No, go back for more
		RTS
 
PrintMsgNM:	LDX #MSGROW			; Status Row
PrintRowNM:	LDA #0				; Col=0
PrintAtNM:	STA CursorCol			; Set COL
		STX CursorRow			; Set ROW
		JSR SetCursor			; Position Cursor
		
PrintNM:	LDA NMOffset,Y			; Get Offset
		TAY				; put in .Y
		LDA #<NMS			; LO
		STA ZP2				; LO Pointer
		LDA #>NMS			; HI
		STA ZP2+1			; Hi Pointer
PNMloop		LDA (ZP2),Y			; Get the character to print
		CMP #0				; End of String?
		BEQ PNMexit			; Yes,done
		JSR PRINT			; No, print it
		INY				; next character
		JMP PNMloop			; loop for more
PNMexit:	RTS

;-------------- Numbered Message Strings

NMOffset:	!BYTE NM0-NMS,NM1-NMS,NM2-NMS,NM3-NMS		; 0-3
		!BYTE NM4-NMS,NM5-NMS,NM6-NMS,NM7-NMS		; 4-7
		!BYTE NM8-NMS,NM9-NMS,NM10-NMS,NM11-NMS		; 8-11
		!BYTE NM12-NMS,NM13-NMS,NM14-NMS		; 12-14

NMS:								; Start of Numbered Message String Area

NM0:	!BYTE 0							; For print@ 
NM1:	!BYTE 19,19,147,17,0					; <HOME><HOME><CLS><DOWN>
NM2:	!PET "quit?",0					
NM3:	!PET "device# (",RVS,"8",ROFF," ",RVS,"9",ROFF
	!PET " 1",RVS,"0",ROFF," 1",RVS,"1",ROFF," 1",RVS,"2",ROFF,")?",0

NM4:	!PET "unit (0,1)?",0
NM5:	!PET 146,"# of entries:",0
NM6:	!PET "reading",0
NM7:	!PET "copying...",0
NM8:	!PET "renaming...",13,0
NM9:	!PET "load prg?",0			; Load prompt
NM10:	!PET 147,"dL",0				; <CLS>dL<QUOTE> - Pre-filename
NM11:	!PET ",d0,u8",0				; d0,u8          - Post-filename

NM12:	!PET  RVS,"cls"   ,ROFF,"drive "	; Select Drive
	!PET  RVS,"/"     ,ROFF,"switch "	; Switch Sides
	!PET  RVS,"crsr"  ,ROFF,"sel "		; Cursor
	!PET  RVS,"home"  ,ROFF,"top "		; Home
	!PET  RVS,"<>"    ,ROFF,"page "		; Page Up/Down
	!PET  RVS,"spc"   ,ROFF,"mark "		; Space
	!PET  RVS,"rtn"   ,ROFF,"run/cd "	; Return
	!PET  RVS,"q"     ,ROFF,"quit",0	; Quit

NM13:	!PET " are you sure (y/n)?",0		; Are you Sure?
NM14:	!PET "stevebrowse 2021-04-13",0 			; Title text

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
