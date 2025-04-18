; -----------------------------------------------------------------------------
;
;       Macros used to define words.
;
; -----------------------------------------------------------------------------

MACRO DEFENTRY flags%,name$
        EQUB flags%+LEN(name$)
        IF LEN(name$)>1
                EQUB MID$(name$,1,LEN(name$)-1)
        ENDIF
        EQUB %10000000+ASC(RIGHT$(name$,1))
ENDMACRO

MACRO DEFWORD name$
        DEFENTRY %10000000,name$
ENDMACRO

MACRO DEFIMM name$
        DEFENTRY %11000000,name$
ENDMACRO

; -----------------------------------------------------------------------------
;
;       Forth build configuration flags.
;
; -----------------------------------------------------------------------------

RELOCATE =? TRUE
STRIP    =? FALSE

; -----------------------------------------------------------------------------
;
;       Acorn MOS (Machine Operating System) system call interface.
;
; -----------------------------------------------------------------------------

BRKV   = $0202
OSARGS = $FFDA
OSBYTE = $FFF4
OSCLI  = $FFF7
OSFIND = $FFCE
OSGBPB = $FFD1
OSNEWL = $FFE7
OSRDCH = $FFE0
OSWORD = $FFF1
OSWRCH = $FFEE

ClearEscapeCondition    = $7E
EnterLanguageRom        = $8E
EscapeFlag              = $FF
FlushBufferClass        = $0F
OpenFileInputOutput     = $C0
ReadDisplayAddr         = $85
ReadHighOrderAddr       = $82
ReadHighWaterMark       = $83
ReadHimem               = $84
ReadKeyWithTimeLimit    = $81
ReadLine                = $00
SetKeyboardRepeatDelay  = $0B
SetKeyboardRepeatPeriod = $0C

; -----------------------------------------------------------------------------
;
;       Various constants used through the code.
;
; -----------------------------------------------------------------------------

CarriageReturn          = $0D
EscapeKey               = $1B
JmpIndirectOpcode       = $6C
LineFeed                = $0A
PrintStrPtr             = $12

; -----------------------------------------------------------------------------

BOS     = $10             ; Bottom of the data stack.
TOS     = $58             ; Top of the data stack.
N       = $60             ; Scratch-pad area for various use.
XSAVE   = $68             ; Location to temporarily save the X register.
W       = XSAVE+2         ; Code field pointer.
IP      = W+2             ; Interpretive Pointer ("PC"), points to current cell.
UP      = IP+2            ; User Area pointer, points to UAREA .

WBSIZ   = 1+255+2         ; WORD BUFFER SIZE

UAREA   = $400            ; The location of the user area, pointed to by UP .
WORDBU  = UAREA+64        ; WORD BUFFER
TIBB    = WORDBU+WBSIZ    ; TERMINAL INPUT BUFFER
PADD    = TIBB+126        ; PAD
REL_DST = PADD+80         ; RAM address to relocate certain words to ($610).

EM      = $7C00           ; END OF MEMORY+1
BLKSIZ  = 1024
HDBT    = BLKSIZ+4
NOBUF   = 2
BUFS    = NOBUF*HDBT
BUF1    = EM-BUFS         ; FIRST BLOCK BUFFER

; -----------------------------------------------------------------------------
;
;       ROM header.
;
; -----------------------------------------------------------------------------

        ORG     $8000

.RomStart
        JMP     LanguageEntry
        JMP     ServiceEntry
        EQUB    %11100010
        EQUB    CopyrightStr-1-RomStart
        EQUB    1
.TitleStr
        EQUB    "FORTH",0
.VersionStr
        EQUB    "1.03",0
.CopyrightStr
        EQUB    "(C) Acornsoft Ltd. 1983",0
        EQUW    RomStart

        EQUW    0               ; Unused.

.ColdWarmStartStr
        EQUB    LineFeed,CarriageReturn
        EQUB    "COLD or WARM start (C/W)? ",0

        EQUB    0               ; Unused.

; -----------------------------------------------------------------------------
;
;       Prints a zero-terminated string located in its entirety in lower ROM
;       area $80xx. Before entry, zero-page location PrintStrPtr should be set
;       to the offset of the beginning of the string to the ROM address $8000,
;       as it is used as a two-byte pointer to the string.
;
; -----------------------------------------------------------------------------

.PrintStr
        LDA     #>RomStart      ; Set the high byte of the pointer to $80xx.
        STA     PrintStrPtr+1
        LDY     #0              ; Start with the first character to print.
.PrintChr
        LDA     (PrintStrPtr),Y ; Fetch the next character to print. If it is
        BEQ     PrintStrDone    ; a zero byte, return, otherwise print it and
        JSR     OSWRCH          ; move on to the next character.
        INY
        BNE     PrintChr
.PrintStrDone
        RTS

; -----------------------------------------------------------------------------
;
;       Service ROM entry point.
;
; -----------------------------------------------------------------------------

.ServiceEntry
        CMP     #4
        BNE     KnownStarCommand

        PHA
        TYA
        PHA

        LDA     ($F2),Y
        CMP     #'F'
        BEQ     GotF
        CMP     #'f'
        BNE     NotStarForth
.GotF
        INY
        LDA     ($F2),Y
        CMP     #'O'
        BEQ     GotFo
        CMP     #'o'
        BNE     CheckAbbreviated
.GotFo
        INY
        LDA     ($F2),Y
        CMP     #'R'
        BEQ     GotFor
        CMP     #'r'
        BNE     CheckAbbreviated
.GotFor
        INY
        LDA     ($F2),Y
        CMP     #'T'
        BEQ     GotFort
        CMP     #'t'
        BNE     CheckAbbreviated
.GotFort
        INY
        LDA     ($F2),Y
        CMP     #'H'
        BEQ     GotForth
        CMP     #'h'
        BNE     CheckAbbreviated
.GotForth
        INY
        LDA     ($F2),Y
        CMP     #CarriageReturn
        BNE     CheckAbbreviated

        LDA     #EnterLanguageRom
        JMP     OSBYTE

.CheckAbbreviated
        CMP     #'.'
        BEQ     GotForth

.NotStarForth
        PLA
        TAY
        PLA
        RTS

.KnownStarCommand
        CMP     #9
        BEQ     StarHelp
        RTS

.StarHelp
        PHA
        TYA
        PHA
        JSR     OSNEWL
        LDA     #<TitleStr
        STA     PrintStrPtr
        JSR     PrintStr
        LDA     #' '
        JSR     OSWRCH
        LDA     #<VersionStr
        STA     PrintStrPtr
        JSR     PrintStr
        JSR     OSNEWL
        PLA
        TAY
        PLA
        RTS

; -----------------------------------------------------------------------------
;
;       Language ROM entry point.
;
; -----------------------------------------------------------------------------

.LanguageEntry
        CMP     #1
        BEQ     LanguageEntryProper
        RTS

.LanguageEntryProper
        CLI
        LDA     #<ColdWarmStartStr
        STA     PrintStrPtr

.AskWarmCold
        JSR     PrintStr
        JSR     OSRDCH
        CMP     #EscapeKey
        BNE     CheckWarmCold

        PHA
        LDA     #ClearEscapeCondition
        JSR     OSBYTE
        PLA

.CheckWarmCold
        CMP     #'W'
        BEQ     UserChoseWarm
        CMP     #'C'
        BNE     AskWarmCold
        JMP     JumpCold

.UserChoseWarm
        JMP     JumpWarm

        EQUB    $FF,$FF         ; Unused.

; -----------------------------------------------------------------------------
;
;       Forth proper starts here. See +ORIGIN .
;
; -----------------------------------------------------------------------------

        ORG     RomStart+$100

.ORIGIN
        NOP
        NOP
.JumpCold
        JMP     COLD+2
.JumpWarm
        JMP     WARM+2

; -----------------------------------------------------------------------------
;
;       ??? Why not refer to these words directly? These bytes may need to be
;       there for +ORIGIN to work, so they would go unused, but there is no
;       need for this indirection.
;
; -----------------------------------------------------------------------------

.PtrToStartPFA
        EQUW    START+2         ; Referred to by COLD .
.PtrToBracketAbortPFA
        EQUW    BRACKETABORT+2  ; Referred to by WARM .

; -----------------------------------------------------------------------------
;
;       At start-up, the user variable pointer UP is reset to point to UAREA
;       and (part of) the boot-up parameters below in there are reset to their
;       default values.  See the routine at Restart .
;
; -----------------------------------------------------------------------------

.BootUpParameters
        EQUW    TOPNFA          ; $0C +ORIGIN: last word in FORTH
                                ;              dictionary.
        EQUW    $7F             ; $0E +ORIGIN: backspace character.
.InitialUP
        EQUW    UAREA           ; $10 +ORIGIN: initial UP .
        EQUW    TOS             ; $12 +ORIGIN: initial S0 .
        EQUW    $01FF           ; $14 +ORIGIN: initial R0 .
        EQUW    TIBB            ; $16 +ORIGIN: initial TIB .
        EQUW    31              ; $18 +ORIGIN: initial WIDTH .
        EQUW    0               ; $1A +ORIGIN: initial WARNING .
        EQUW    TOPDP           ; $1C +ORIGIN: initial FENCE .
        EQUW    TOPDP           ; $1E +ORIGIN: initial DP .
        EQUW    VL0-REL         ; $20 +ORIGIN: initial VOC-LINK .
        EQUW    1               ; $22 +ORIGIN: initial BLK .

        EQUB    "RdeG-H"        ; Author Richard de Grandis-Harrison

; -----------------------------------------------------------------------------
;
;       ??? Why not refer to these words directly? There is no need for this
;       indirection.
;
; -----------------------------------------------------------------------------

.PtrToBrkHandler
        EQUW    BrkHandler      ; Referred to by COLD and WARM .
.PtrToEscapePFA
        EQUW    ESCAPE+2        ; Referred to by EscapeHandler .
.PtrToOSErrorPFA
        EQUW    OSERR+2         ; Referred to by BrkHandler .

; -----------------------------------------------------------------------------

.Restart
        LDA     InitialUP+1             ; Reset the User Pointer (UP) to its
        STA     UP+1                    ; initial value UAREA .
        LDA     InitialUP
        STA     UP

.ResetUserVariables
        LDA     BootUpParameters,Y      ; Reset the first #Y bytes of the user
        STA     (UP),Y                  ; variables pointed at by UP .
        DEY
        BPL     ResetUserVariables

        LDA     #JmpIndirectOpcode      ; Prepare a JMP (...) instruction at W,
        STA     W-1                     ; which is self-modified by NEXT .

        CLD
        JMP     RPSTORE+2               ; Execute RP!

; -----------------------------------------------------------------------------
;
;       LIT   ( ... n )
;
;       > Within a colon-definition, LIT is automatically compiled before each
;       > 16-bit literal number encountered in the input text. Later execution
;       > of LIT causes the contents of the following two bytes to be pushed
;       > onto the stack.
;
; -----------------------------------------------------------------------------

.LIT_NFA
        DEFWORD "LIT"
        EQUW    0               ; We are the first word in the dictionary.
.LIT
{       EQUW    *+2
        LDA     (IP),Y          ; Load the low byte of the literal value from
        PHA                     ; the parameter field, and push it onto the
        INC     IP              ; return stack. Then increment IP .
        BNE     skip
        INC     IP+1
.skip   LDA     (IP),Y          ; Load the high byte of the literal value from
        INC     IP              ; the parameter field into the accumulator, and
        BNE     PUSH            ; increment IP . Then fall through to PUSH it
        INC     IP+1            ; as a new cell onto the data stack.
}

; -----------------------------------------------------------------------------
;
;       > Push the accumulator (as high byte) and one byte from the return
;       > stack as a new number on the computation stack, and execute NEXT .
;
; -----------------------------------------------------------------------------

.PUSH   DEX                     ; Make room for a new cell on the data stack.
        DEX                     ; Fall through to PUT .

; -----------------------------------------------------------------------------
;
;       > Replace the current top stack item from the accumulator and return
;       > stack (as for PUSH)and execute NEXT .
;
; -----------------------------------------------------------------------------

.PUT    STA     1,X             ; Write the new cell to the data stack,
        PLA                     ; consisting of the accumulator (high byte)
        STA     0,X             ; and the value on top of the return stack
                                ; (low byte). Fall through to NEXT to execute
                                ; the next word.

; -----------------------------------------------------------------------------
;
;       > Transfer execution to the next word in the sequence.
;
; -----------------------------------------------------------------------------

.NEXT   LDY     #1              ; With IP pointing to the CFA of the next word
.NEXTY1 LDA     (IP),Y          ; to execute, store what CFA points to at the
        STA     W+1             ; code field pointer W . That is part of an
        DEY                     ; indirect JMP (...) instruction, meaning we
        LDA     (IP),Y          ; are modifying that instruction from here.
        STA     W

        CLC                     ; Increment IP by two, so it points to the PFA
        LDA     IP              ; of this word. If the word is a colon
        ADC     #2              ; definition, the Parameter Field will contain
        STA     IP              ; the addresses of the CFA's of each word that
        BCC     CheckEscape     ; was compiled in and needs to be executed.
        INC     IP+1

.CheckEscape
        BIT     EscapeFlag      ; Like with BBC Basic, execution can be stopped
        BMI     EscapePressed   ; by pressing the escape key.

        JMP     W-1             ; Perform a (modified) indirect jump JMP (...)
                                ; to what the CFA of this word points to.

.EscapePressed
        JMP     EscapeHandler   ; Jump to the escape handler. ??? Why not
                                ; insert its body here to save the three bytes
                                ; of the jump instruction?

; -----------------------------------------------------------------------------
;
;       > [SETUP] acts to transfer up to four items from the stack to a
;       > scratch-pad area in page zero. On entry the accumulator should
;       > contain the number of items to be transferred. On return from the
;       > subroutine the Y-register will contain zero and the value in the
;       > accumulator will be doubled; in other words it contains the number of
;       > bytes transferred from the stack. The byte immediately preceding the
;       > scratchpad area will also contain the number of bytes transferred.
;
;       Note that there is an invariant that the Y register is always zero,
;       which is why SETUP does not explicitly reset it at the beginning.
;
; -----------------------------------------------------------------------------

.SETUP
{
        ASL     A               ; Each cell to copy consists of two bytes, and
        STA     N-1             ; the number of bytes is stored in N-1.
.loop   LDA     0,X             ; Load a byte from the data stack and store it
        STA     N,Y             ; in the scratch-pad area. Keep doing this
        INX                     ; until all bytes have been copied. The stack
        INY                     ; pointer X is adjusted to remove them from the
        CPY     N-1             ; stack.
        BNE     loop
        LDY     #0              ; Restore the invariant that Y is always zero.
        RTS
}

; -----------------------------------------------------------------------------
;
;       EXECUTE   ( addr ... )
;
;       > Executes the definition whose code field (execution) address is on
;       > the stack.
;
; -----------------------------------------------------------------------------

.EXECUTE_NFA
        DEFWORD "EXECUTE"
        EQUW    LIT_NFA
.EXECUTE
        EQUW    *+2
        LDA     0,X             ; Transfer the CFA from the stack to W, thereby
        STA     W               ; modifying the indirect JMP (...) we'll use
        LDA     1,X             ; down below.
        STA     W+1
        INX                     ; Adjust the stack pointer.
        INX

        JMP     W-1             ; Perform a (modified) indirect jump JMP (...)
                                ; to what the CFA points to.

; -----------------------------------------------------------------------------
;
;       @EXECUTE   ( addr ... )
;
;       > Executes the definition whose code field (execution) address is
;       > contained in the two bytes at the address addr.
;
; -----------------------------------------------------------------------------

.FETCHEXECUTE_NFA
        DEFWORD "@EXECUTE"
        EQUW    EXECUTE_NFA
.FETCHEXECUTE {
        EQUW    *+2
        LDA     (0,X)           ; Transfer the CFA from the pointer at the top
        STA     W               ; of the stack, to W, thereby modifying the
        INC     0,X             ; indirect JMP (...) we'll use down below. We
        BNE     skip            ; have to increment the pointer on top of the
        INC     1,X             ; stack to point to the second byte of the CFA.
.skip   LDA     (0,X)
        STA     W+1
        INX                     ; Adjust the stack pointer.
        INX

        JMP     W-1             ; Perform a (modified) indirect jump JMP (...)
}                               ; to what the CFA points to.

; -----------------------------------------------------------------------------
;
;       BRANCH
;
;       > The run-time procedure to cause an unconditional branch. The
;       > following in-line value is added to the interpretive pointer to cause
;       > a forward or backward branch. It is compiled by ELSE , AGAIN and
;       > REPEAT .
;
; -----------------------------------------------------------------------------

.BRANCH_NFA
        DEFWORD "BRANCH"
        EQUW    FETCHEXECUTE_NFA
.BRANCH
        EQUW    DOBRANCH
.DOBRANCHX
        LDX     XSAVE           ; Restore the data stack pointer.
.DOBRANCH
        CLC
        LDA     (IP),Y          ; Read the low byte of the branch offset from
        ADC     IP              ; the parameter field, add the low byte of IP
        PHA                     ; to it. Save it on the return stack while we
        INY                     ; still need the original value of IP. Then
        LDA     (IP),Y          ; fetch the high byte of the offset and update
        ADC     IP+1            ; all of IP.
        STA     IP+1
        PLA
        STA     IP
        JMP     NEXTY1          ; Since Y is now one instead of zero, violating
                                ; the invariant, we call into NEXT right after
                                ; where it would have set Y to one as well. It
                                ; will take care of resetting Y for us.

; -----------------------------------------------------------------------------
;
;       0BRANCH   ( f ... )
;
;       > The run-time procedure to cause a conditional branch. If f is false
;       > the following in-line number is added to the interpretive pointer to
;       > cause a forward or backward branch. It is compiled by IF , UNTIL and
;       > WHILE .
;
; -----------------------------------------------------------------------------

.ZEROBRANCH_NFA
        DEFWORD "0BRANCH"
        EQUW    BRANCH_NFA
.ZEROBRANCH
        EQUW    *+2
        INX                     ; Update the stack pointer in advance to
        INX                     ; prevent it messing up our CPU flags. That
        LDA     $FE,X           ; means we need to load the flag f from the
        ORA     $FF,X           ; stack at -2,X and -1,X rather than the
        BEQ     DOBRANCH        ; usual 0,X and 1,X. Branch if flag f is zero.
.BUMPIP
{       CLC                     ; Advance IP by one cell to skip past the
        LDA     IP              ; branch offset that is stored in the parameter
        ADC     #2              ; field.
        STA     IP
        BCC     skip
        INC     IP+1
.skip   JMP     NEXT
}

; -----------------------------------------------------------------------------
;
;       (LOOP)
;
;       > The run-time procedure compiled by LOOP . It increments the loop
;       > index by one and tests for loop completion. See LOOP .
;
;       Note that the return stack contains the loop maximum and the loop
;       index.
;
; -----------------------------------------------------------------------------

.BRACKETLOOP_NFA
        DEFWORD "(LOOP)"
        EQUW    ZEROBRANCH_NFA
.BRACKETLOOP
        EQUW    *+2
        STX     XSAVE
        TSX                     ; Increment the low byte of the loop index on
        INC     $101,X          ; the return stack. If it is now zero, also
        BNE     CHECKLOOPDONE   ; increment the high byte by adding zero (the
        TYA                     ; Y register) with the carry set.
        SEC
        ADC     $102,X
        BVS     LOOPDONE        ; We're done upon a twos complement overflow.
        STA     $102,X
.CHECKLOOPDONE
        CLC
        LDA     $103,X          ; Access the loop maximum on the return stack
        SBC     $101,X          ; and subtract the current loop index, to see
        LDA     $104,X          ; whether we are done. The accumulator ends up
        SBC     $102,X          ; with the high byte of the subtraction.
        BVC     LOOPAGAIN       ; Check whether to loop again if there is no
        EOR     #$80            ; sign error, otherwise correct the sign first.
.LOOPAGAIN
        BPL     DOBRANCHX       ; Loop again when the maximum has not been
                                ; reached yet.
.LOOPDONE
        LDX     XSAVE           ; Restore the return stack pointer and remove
        PLA                     ; the loop index and loop maximum from it.
        PLA
        PLA
        PLA
        JMP     BUMPIP          ; Then bump IP beyond the branch offset in the
                                ; parameter field and continue beyond the loop.

; -----------------------------------------------------------------------------
;
;       (+LOOP)   ( n ... )
;
;       > The run-time procedure compiled by +LOOP . It increments the loop
;       > index by the signed quantity n and tests for loop completion. See
;       > +LOOP .
;
; -----------------------------------------------------------------------------

.BRACKETPLUSLOOP_NFA
        DEFWORD "(+LOOP)"
        EQUW    BRACKETLOOP_NFA
.BRACKETPLUSLOOP
        EQUW    *+2
        LDA     1,X
        PHA
        PHA
        LDA     0,X
        INX
        INX
        STX     XSAVE
        TSX
        INX
        INX
        CLC
        ADC     $101,X
        STA     $101,X
        PLA
        ADC     $102,X
        STA     $102,X
        PLA
        BVS     LOOPDONE
        BPL     CHECKLOOPDONE
        SEC
        LDA     $101,X
        SBC     $103,X
        LDA     $102,X
        SBC     $104,X
        BVS     LOOPDONE
        BVC     LOOPAGAIN

; -----------------------------------------------------------------------------
;
;       (ULOOP)
;
;       > The run-time procedure for an unsigned version of LOOP . It is used
;       > by the system for loops involving unsigned indices, for example,
;       > addresses. It is not available to the user in the system as provided
;       > but can be made so by entering the following definition:
;       >
;       > : ULOOP
;       >  3 ?PAIRS
;       >  COMPILE (ULOOP)
;       >  BACK
;       > ; IMMEDIATE
;       >
;       > It may then be used in a colon-definition as:
;       >
;       > ... DO .... ULOOP ....
;
; -----------------------------------------------------------------------------

.BRACKETULOOP_NFA
        DEFWORD "(ULOOP)"
        EQUW    BRACKETPLUSLOOP_NFA
.BRACKETULOOP
        EQUW    *+2
        STX     XSAVE
        TSX
        INC     $101,X
        BNE     L82A3
        INC     $102,X
.L82A3  CLC
        LDA     $103,X
        SBC     $101,X
        LDA     $104,X
        SBC     $102,X
        JMP     LOOPAGAIN

; -----------------------------------------------------------------------------
;
;       I   ( ... n )
;
;       > Used in a DO ... LOOP to place the current value of the loop index on
;       > the stack. It must be used at the same level of nesting as the DO ...
;       > LOOP i.e. it will not operate correctly if included in a colon-
;       > definition word between DO and LOOP .
;
; -----------------------------------------------------------------------------

.IDO_NFA
        DEFWORD "I"
        EQUW    BRACKETULOOP_NFA
.IDO
        EQUW    *+2
        STX     XSAVE
        TSX
        LDA     $101,X
        PHA
        LDA     $102,X
        LDX     XSAVE
        JMP     PUSH

;       (DO)

.XDO_NFA
        DEFWORD "(DO)"
        EQUW    IDO_NFA
.XDO
        EQUW    *+2
        LDA     3,X
        PHA
        LDA     2,X
        PHA
        LDA     1,X
        PHA
        LDA     0,X
        PHA

; -----------------------------------------------------------------------------
;
;       > Drop the top two stack items and execute NEXT .
;
; -----------------------------------------------------------------------------

.POPTWO INX                     ; Decrement the data stack pointer by one
        INX                     ; cell.

; -----------------------------------------------------------------------------
;
;       > Drop the top stack item and execute NEXT .
;
; -----------------------------------------------------------------------------

.POP    INX                     ; Decrement the data stack pointer by one
        INX                     ; cell.
        JMP     NEXT

; -----------------------------------------------------------------------------
;
;       HIADDR   ( ... n )
;
;       Pushes the machine's high order address onto the stack. The high order
;       address represents the upper 16 bits of a 32-bit memory address
;       (&NNNNxxxx), which is &0000 for a second processor connected to the
;       TUBE interface, and &FFFF for the I/O processor, i.e. the BBC or
;       Electron.
;
; -----------------------------------------------------------------------------

.HIADDR_NFA
        DEFWORD "HIADDR"
        EQUW    XDO_NFA
.HIADDR
        EQUW    *+2
        STX     XSAVE
        LDA     #ReadHighOrderAddr
        JSR     OSBYTE
        TXA
        LDX     XSAVE
        PHA
        TYA
        JMP     PUSH

; -----------------------------------------------------------------------------
;
;       MODEADDR   ( n ... addr )
;
;       Given a screen MODE 'n', returns the address at which its video memory
;       starts.
;
; -----------------------------------------------------------------------------

.MODEADDR_NFA
        DEFWORD "MODEADDR"
        EQUW    HIADDR_NFA
.MODEADDR
        EQUW    *+2
        STX     XSAVE
        LDA     0,X
        TAX
        LDA     #ReadDisplayAddr
        JSR     OSBYTE
        TXA
        LDX     XSAVE
        STY     1,X
        STA     0,X
        JMP     NEXT

;       DIGIT

.DIGIT_NFA
        DEFWORD "DIGIT"
        EQUW    MODEADDR_NFA
.DIGIT
        EQUW    *+2
        SEC
        LDA     2,X
        SBC     #'0'
        BMI     L8348
        CMP     #10
        BMI     L833B
        SEC
        SBC     #7
        CMP     #10
        BMI     L8348
.L833B  CMP     0,X
        BPL     L8348
        STA     2,X
        LDA     #1
        PHA
        TYA
        JMP     PUT

.L8348  TYA
        PHA
        INX
        INX
        JMP     PUT

;       (FIND)

.BRACKETFIND_NFA
        DEFWORD "(FIND)"
        EQUW    DIGIT_NFA
.BRACKETFIND
        EQUW    *+2
        LDA     #2
        JSR     SETUP
        STX     XSAVE
.L8361  LDY     #0
        LDA     (N),Y
        EOR     (N+2),Y
        AND     #$3F
        BNE     L8398
.L836B  INY
        LDA     (N),Y
        EOR     (N+2),Y
        ASL     A
        BNE     L8396
        BCC     L836B
        LDX     XSAVE
        DEX
        DEX
        DEX
        DEX
        CLC
        TYA
        ADC     #3
        ADC     N
        STA     2,X
        LDY     #0
        TYA
        ADC     N+1
        STA     3,X
        STY     1,X
        LDA     (N),Y
        STA     0,X
        LDA     #1
        PHA
        JMP     PUSH

.L8396  BCS     L839D
.L8398  INY
        LDA     (N),Y
        BPL     L8398
.L839D  INY
        LDA     (N),Y
        TAX
        INY
        LDA     (N),Y
        STA     N+1
        STX     N
        ORA     N
        BNE     L8361
        LDX     XSAVE
        LDA     #0
        PHA
        JMP     PUSH

;       ENCLOSE

.ENCLOSE_NFA
        DEFWORD "ENCLOSE"
        EQUW    BRACKETFIND_NFA
.ENCLOSE
        EQUW    *+2
        LDA     #2
        JSR     SETUP
        TXA
        SEC
        SBC     #8
        TAX
        DEY
        STY     N+1
        DEC     N+3
.L83CF  INY
        BNE     L83D6
        INC     N+1
        INC     N+3
.L83D6  LDA     (N+2),Y
        CMP     N
        BEQ     L83CF
        STY     4,X
        LDA     N+1
        STA     5,X
.L83E2  LDA     (N+2),Y
        BNE     L8404
        STY     2,X
        STY     0,X
        LDA     N+1
        STA     3,X
        STA     1,X
        TYA
        CMP     4,X
        BNE     L8401
        LDA     N+1
        CMP     5,X
        BNE     L8401
        INC     2,X
        BNE     L8401
        INC     3,X
.L8401  JMP     NEXT

.L8404  INY
        BNE     L840B
.L8407  INC     N+3
        INC     N+1
.L840B  CMP     N
        BNE     L83E2
        STY     0,X
        LDA     N+1
        STA     1,X
        STA     3,X
        TYA
        BNE     L841C
        DEC     3,X
.L841C  DEY
        STY     2,X
        JMP     NEXT

; -----------------------------------------------------------------------------
;
;       SP@   ( ... addr )
;
;       > Leaves the value of the stack pointer on the stack. The value
;       > corresponds to the state of the stack before the operation.
;
; -----------------------------------------------------------------------------

.SPFETCH_NFA
        DEFWORD "SP@"
        EQUW    ENCLOSE_NFA
.SPFETCH
        EQUW    *+2
        TXA                     ; Transfer the data stack pointer in register X
                                ; to the accumulator, and fall through to
                                ; PUSH0A to push it as a cell.

; -----------------------------------------------------------------------------
;
;       > Push zero (as high byte) and the accumulator (low byte) to the stack
;       > and execute NEXT .
;
; -----------------------------------------------------------------------------
 
.PUSH0A PHA
        LDA     #0
        JMP     PUSH

; -----------------------------------------------------------------------------
;
;       RP@   ( ... addr )
;
;       > Leaves the address of the return stack pointer. Note that this points
;       > one byte below the last return stack value.
;
; -----------------------------------------------------------------------------

.RPFETCH_NFA
        DEFWORD "RP@"
        EQUW    SPFETCH_NFA
.RPFETCH
        EQUW    *+2
        STX     XSAVE           ; Transfer the value of the SP register to the
        TSX                     ; accumulator, push it as the low byte onto the
        TXA                     ; stack, followed by a high byte of one (since
        LDX     XSAVE           ; the return stack resides in $01xx).
        PHA
        LDA     #1
        JMP     PUSH            ; Fall through to PUSH to push it as a cell.

; -----------------------------------------------------------------------------
;
;       SP!
;
;       > Initialises the computation stack pointer (i.e. clears the stack).
;
; -----------------------------------------------------------------------------

.L8445  DEFWORD "SP!"
        EQUW    RPFETCH_NFA
.SPSTORE
        EQUW    *+2
        LDY     #6              ; Transfer the low byte of the S0 user variable
        LDA     (UP),Y          ; to register X, which acts as the computation
        TAX                     ; stack pointer in zero page $00xx.
        JMP     NEXT

; -----------------------------------------------------------------------------
;
;       RP!
;
;       > Initialises the return stack pointer.
;
; -----------------------------------------------------------------------------

.RPSTORE_NFA
        DEFWORD "RP!"
        EQUW    L8445
.RPSTORE
        EQUW    *+2
        STX     XSAVE
        LDY     #8              ; Transfer the low byte of the R0 user variable  
        LDA     (UP),Y          ; to the hardware stack register SP, which
        TAX                     ; always resides in page one at $01xx.
        TXS
.L8465  LDX     XSAVE
        JMP     NEXT

; -----------------------------------------------------------------------------
;
;       (EMIT)   ( c ... )
;
;       > See EMIT .
;
; -----------------------------------------------------------------------------

.BRACKETEMIT_NFA
        DEFWORD "(EMIT)"
        EQUW    RPSTORE_NFA
.BRACKETEMIT
        EQUW    *+2
        TYA                     ; Increment the value of user variable OUT .
        SEC
        LDY     #$1A
        ADC     (UP),Y
        STA     (UP),Y
        INY
        LDA     #0
        ADC     (UP),Y
        STA     (UP),Y
        LDA     0,X             ; Retrieve the character to emit from the stack
        AND     #$7F            ; and mask the lower seven, then print it.
        JSR     OSWRCH
        JMP     POP             ; POP the value off the stack and continue.

; -----------------------------------------------------------------------------
;
;       >VDU   ( n ... )
;
;       > Transmits the low byte of n to the VDU driver, without incrementing
;       > OUT . See EMIT .
;
; -----------------------------------------------------------------------------

.L848E  DEFWORD ">VDU"
        EQUW    L9FFB-REL
.TOVDU  EQUW    *+2
        LDA     0,X
        JSR     OSWRCH
        JMP     POP

; -----------------------------------------------------------------------------
;
;       CMOVE   ( from\to\count ... )
;
;       > Moves 'count' bytes, starting at 'from' to the block of memory
;       > starting at 'to'. The byte at 'from' is moved first and the
;       > transfer proceeds towards high memory. No check is made as to
;       > whether the destination area overlaps the source area. Nothing
;       > is moved if 'count' is zero or negative.
;
; -----------------------------------------------------------------------------


.L849F  DEFWORD "CMOVE"
        EQUW    L848E
.CMOVE  EQUW    *+2
        SEC
        TYA
        SBC     0,X
        TYA
        SBC     1,X
        BPL     L84D0
        LDA     #3
        JSR     SETUP
.L84B7  CPY     N
        BNE     L84C2
        DEC     N+1
        BPL     L84C2
        JMP     NEXT

.L84C2  LDA     (N+4),Y
        STA     (N+2),Y
        INY
        BNE     L84B7
        INC     N+5
        INC     N+3
        JMP     L84B7

.L84D0  INX
        INX
        JMP     POPTWO

; -----------------------------------------------------------------------------
;
;       U*   ( u1\u2 ... ud )
;
;       > Leaves the unsigned double-precision product of two unsigned numbers.
;
; -----------------------------------------------------------------------------

.L84D5  DEFWORD "U*"
        EQUW    L849F
.USTAR  EQUW    *+2
        LDA     0,X
        STA     N
        LDA     1,X
        STA     N+1
        TYA
        STA     0,X
        CLC
        LDY     #$11
        BNE     L84F9

.L84EC  BCC     L84F9
        PHA
        CLC
        LDA     0,X
        ADC     N
        STA     0,X
        PLA
        ADC     N+1
.L84F9  ROR     A
        ROR     0,X
        ROR     3,X
        ROR     2,X
        DEY
        BNE     L84EC
        STA     1,X
        JMP     NEXT

; -----------------------------------------------------------------------------
;
;       U/   ( ud\u1 ... u2\u3 )
;
;       > Leaves the unsigned remainder u2 and unsigned quotient u3 from the
;       > division of the unsigned double number dividend ud by the unsigned
;       > divisor u1. No protection is given against arithmetical overflow or
;       > division by zero.
;
; -----------------------------------------------------------------------------

.L8508  DEFWORD "U/"
        EQUW    L84D5
.USLASH EQUW    *+2
        STY     N+1
        LDA     4,X
        LDY     2,X
        STY     4,X
        ASL     A
        STA     2,X
        LDA     5,X
        LDY     3,X
        STY     5,X
        ROL     A
        STA     3,X
        LDA     #$10
        STA     N
.L8527  ROL     4,X
        ROL     5,X
        ROL     N+1
        SEC
        LDA     4,X
        SBC     0,X
        TAY
        LDA     5,X
        SBC     1,X
        PHA
        LDA     N+1
        SBC     #0
        PLA
        BCC     L8543
        STY     4,X
        STA     5,X
.L8543  ROL     2,X
        ROL     3,X
        DEC     N
        BNE     L8527
        JMP     POP

;       AND

.L854E  DEFWORD "AND"
        EQUW    L8508
.AND    EQUW    *+2
        LDA     0,X
        AND     2,X
        PHA
        LDA     1,X
        AND     3,X
        INX
        INX
        JMP     PUT

;       OR

.L8564  DEFWORD "OR"
        EQUW    L854E
.OR     EQUW    *+2
        LDA     0,X
        ORA     2,X
        PHA
        LDA     1,X
        ORA     3,X
        INX
        INX
        JMP     PUT

;       XOR

.XOR_NFA
        DEFWORD "XOR"
        EQUW    L8564
.XOR    EQUW    *+2
        LDA     0,X
        EOR     2,X
        PHA
        LDA     1,X
        EOR     3,X
        INX
        INX
        JMP     PUT

; -----------------------------------------------------------------------------
;
;       ?KEY   ( n1 ... n2 )
;
;       > Flushes the keyboard buffer of all characters and tests if a key is
;       > being pressed. There are two cases:
;       >
;       > 1) If n1 is positive, i.e. in the range 0 to 32767 inclusive, ?KEY
;       >    will will wait for up to n1 hundredths of a second, constantly
;       >    testing to see if a key has been pressed. If a key is pressed
;       >    within the time limit its ASCII value will be returned as n2. If
;       >    the time limit expires before a key is pressed a negative number
;       >    will be returned as n2.
;       >
;       > 2) If n1 is negative a test will be made to see if a particular key
;       >    is pressed at the instant ?KEY is called. The value of n1
;       >    determines which key is to be tested according to the table given
;       >    in the description of INKEY in the BBC Microcomputer User Guide
;       >    (page 275). If the key is pressed n2 will be returned as -1,
;       >    otherwise n2 will be zero. These may be treated as true and false
;       >    flags respectively.
;
; -----------------------------------------------------------------------------

.QUERYKEY_NFA
        DEFWORD "?KEY"
        EQUW    XOR_NFA
.QUERYKEY
        EQUW    *+2
        STX     XSAVE

        LDX     #1                      ; 1/100th second
        LDA     #SetKeyboardRepeatPeriod
        JSR     OSBYTE
        STX     N                       ; save previous value

        LDX     #0                      ; 0/100th second
        LDA     #SetKeyboardRepeatDelay
        JSR     OSBYTE
        STX     N+1                     ; save previous value

        LDX     #1                      ; input buffer
        LDA     #FlushBufferClass
        JSR     OSBYTE
        
        LDX     XSAVE
        LDY     1,X
        LDA     0,X
        TAX
        LDA     #ReadKeyWithTimeLimit
        JSR     OSBYTE
        
        TXA
        PHA
        TYA
        PHA
        
        LDX     N                       ; restore previous value
        LDA     #SetKeyboardRepeatPeriod
        JSR     OSBYTE
        
        LDX     N+1                     ; restore previous value
        LDA     #SetKeyboardRepeatDelay
        JSR     OSBYTE
        
        PLA
        LDX     XSAVE
        JMP     PUT

; -----------------------------------------------------------------------------
;
;       (KEY)   ( ... c)
;
;       > See KEY .
;
; -----------------------------------------------------------------------------

.L85D7  DEFWORD "(KEY)"
        EQUW    QUERYKEY_NFA
.BRACKETKEY
        EQUW    *+2
        JSR     OSRDCH
        JMP     PUSH0A

; -----------------------------------------------------------------------------
;
;       EXIT
;
;       > When compiled within a colon-definition, terminates execution of the
;       > definition at that point. It may not be used within a DO ... LOOP .
;       > It is also used to terminate the interpretation of mass storage.
;
; -----------------------------------------------------------------------------

.L85E7  DEFWORD "EXIT"
        EQUW    KEY_NFA-REL
.EXIT   EQUW    *+2
        PLA                     ; Pop the previous value of IP, pointing in the
        STA     IP              ; parameter field of the word that called us,
        PLA                     ; into IP from the return stack, and continue
        STA     IP+1            ; there when NEXT is called.
        JMP     NEXT

; -----------------------------------------------------------------------------
;
;       R@   ( ... n )
;
;       > Copy the top of the return stack to the computation stack. The action
;       > is identical to that of I .
;
; -----------------------------------------------------------------------------

.RFETCH_NFA
        DEFWORD "R@"
        EQUW    L85E7
.RFETCH EQUW    *+2
        STX     XSAVE
        TSX
        LDA     $101,X
        PHA
        LDA     $102,X
        LDX     XSAVE
        JMP     PUSH

;       >R

.TOR_NFA
        DEFWORD ">R"
        EQUW    RFETCH_NFA
.TOR    EQUW    *+2
        LDA     1,X
        PHA
        LDA     0,X
        PHA
        JMP     POP

;       R>

.RFROM_NFA
        DEFWORD "R>"
        EQUW    TOR_NFA
.RFROM  EQUW    *+2
        DEX
        DEX
        PLA
        STA     0,X
        PLA
        STA     1,X
        JMP     NEXT

;       ><

.BYTESWAP_NFA
        DEFWORD "><"
        EQUW    RFROM_NFA
.BYTESWAP
        EQUW    *+2
        LDY     0,X
        LDA     1,X
        STA     0,X
        STY     1,X
        JMP     NEXT

;       LEAVE

.L8643  DEFWORD "LEAVE"
        EQUW    BYTESWAP_NFA
.LEAVE  EQUW    *+2
        STX     XSAVE
        TSX
        LDA     $101,X
        STA     $103,X
        LDA     $102,X
        STA     $104,X
        LDX     XSAVE
        JMP     NEXT

;       0=

.L8661  DEFWORD "0="
        EQUW    L8643
.ZEROEQUAL
        EQUW    *+2
        LDA     0,X
        ORA     1,X
        STY     1,X
        BNE     L8671
        INY
.L8671  STY     0,X
        JMP     NEXT

;       0<

.L8676  DEFWORD "0<"
        EQUW    L8661
.ZEROLESS
        EQUW    *+2
        ASL     1,X
        TYA
        ROL     A
        STY     1,X
        STA     0,X
        JMP     NEXT

;       <

.L8688  DEFWORD "<"
        EQUW    L8676
.LESS   EQUW    *+2
        SEC
        LDA     2,X
        SBC     0,X
        LDA     3,X
        SBC     1,X
        STY     3,X
        BVC     L869D
        EOR     #$80
.L869D  BPL     L86A0
        INY
.L86A0  STY     2,X
        JMP     POP

;       D<

.L86A5  DEFWORD "D<"
        EQUW    L8688
.DLESS  EQUW    *+2
        SEC
        LDA     6,X
        SBC     2,X
        LDA     7,X
        SBC     3,X
        LDA     4,X
        SBC     0,X
        LDA     5,X
        SBC     1,X
        BVC     L86C1
        EOR     #$80
.L86C1  BPL     L86C4
        INY
.L86C4  TYA
        LDY     #4
.L86C7  INX
        INX
        DEY
        BNE     L86C7
        JMP     PUSH0A

;       +

.L86CF  DEFWORD "+"
        EQUW    L86A5
.PLUS   EQUW    *+2
        CLC
        LDA     0,X
        ADC     2,X
        STA     2,X
        LDA     1,X
        ADC     3,X
        STA     3,X
        JMP     POP

;       D+

.L86E5  DEFWORD "D+"
        EQUW    L86CF
.DPLUS  EQUW    *+2
        CLC
        LDA     2,X
        ADC     6,X
        STA     6,X
        LDA     3,X
        ADC     7,X
        STA     7,X
        LDA     0,X
        ADC     4,X
        STA     4,X
        LDA     1,X
        ADC     5,X
        STA     5,X
        JMP     POPTWO

;       NEGATE

.L8708  DEFWORD "NEGATE"
        EQUW    L86E5
.NEGATE EQUW    *+2
        SEC
.L8714  TYA
        SBC     0,X
        STA     0,X
        TYA
        SBC     1,X
        STA     1,X
        JMP     NEXT

;       DNEGATE

.L8721  DEFWORD "DNEGATE"
        EQUW    L8708
.DNEGATE
        EQUW    *+2
        SEC
        TYA
        SBC     2,X
        STA     2,X
        TYA
        SBC     3,X
        STA     3,X
        JMP     L8714

;       DROP

.L873B  DEFWORD "DROP"
        EQUW    L8721
.DROP   EQUW    POP

;       2DROP

.L8744  DEFWORD "2DROP"
        EQUW    L873B
.TWODROP
        EQUW    POPTWO

;       DUP

.L874E  DEFWORD "DUP"
        EQUW    L8744
.DUP    EQUW    *+2
        LDA     0,X
        PHA
        LDA     1,X
        JMP     PUSH

;       ?DUP

.QUERYDUP_NFA
        DEFWORD "?DUP"
        EQUW    L874E
.QUERYDUP
        EQUW    *+2
        LDA     0,X
        ORA     1,X
        BNE     DUP+2
        JMP     NEXT

;       2DUP

.TWODUP_NFA
        DEFWORD "2DUP"
        EQUW    QUERYDUP_NFA
.TWODUP EQUW    *+2
        LDA     0,X
        PHA
        LDA     1,X
        PHA
        LDA     2,X
        PHA
        LDA     3,X
        DEX
        DEX
        STA     1,X
        PLA
        STA     0,X
        PLA
        JMP     PUSH

;       SWAP

.SWAP_NFA
        DEFWORD "SWAP"
        EQUW    TWODUP_NFA
.SWAP   EQUW    *+2
        LDA     2,X
        PHA
        LDA     0,X
        STA     2,X
        LDA     3,X
        LDY     1,X
        STY     3,X
        JMP     PUT

;       2SWAP

.TWOSWAP_NFA
        DEFWORD "2SWAP"
        EQUW    SWAP_NFA
.TWOSWAP
        EQUW    *+2
        LDY     6,X
        LDA     2,X
        STA     6,X
        STY     2,X
        LDY     7,X
        LDA     3,X
        STA     7,X
        STY     3,X
        LDA     4,X
        PHA
        LDA     0,X
        STA     4,X
        LDA     5,X
        LDY     1,X
        STY     5,X
        JMP     PUT

;       OVER

.OVER_NFA
        DEFWORD "OVER"
        EQUW    TWOSWAP_NFA
.OVER   EQUW    *+2
        LDA     2,X
        PHA
        LDA     3,X
        JMP     PUSH

;       2OVER

.TWOOVER_NFA
        DEFWORD "2OVER"
        EQUW    OVER_NFA
.TWOOVER
        EQUW    *+2
        LDA     4,X
        PHA
        LDA     5,X
        DEX
        DEX
        LDY     9,X
        STY     1,X
        LDY     8,X
        STY     0,X
        JMP     PUSH

;       ROT

.ROT_NFA
        DEFWORD "ROT"
        EQUW    TWOOVER_NFA
.ROT    EQUW    *+2
        LDA     4,X
        PHA
        LDY     5,X
        LDA     3,X
        STA     5,X
        LDA     2,X
        STA     4,X
        LDA     1,X
        STA     3,X
        LDA     0,X
        STA     2,X
        TYA
        JMP     PUT

;       TRAVERSE

.TRAVERSE_NFA
        DEFWORD "TRAVERSE"
        EQUW    ROT_NFA
.TRAVERSE
        EQUW    *+2
        CLC
        LDA     0,X
        ADC     2,X
        STA     2,X
        LDA     1,X
        ADC     3,X
        STA     3,X
        LDA     (2,X)
        CMP     #$80
        BMI     TRAVERSE+2
        JMP     POP

;       PAGE

.L8843  DEFWORD "PAGE"
        EQUW    TRAVERSE_NFA
.PAGE   EQUW    *+2
        STX     XSAVE
        LDA     #ReadHighWaterMark
        JSR     OSBYTE
        TXA
        PHA
        TYA
        LDX     XSAVE
        JMP     PUSH

;       HIMEM

.L885B  DEFWORD "HIMEM"
        EQUW    L8843
.HIMEM  EQUW    *+2
        STX     XSAVE
        LDA     #ReadHimem
        JSR     OSBYTE
        TXA
        PHA
        TYA
        LDX     XSAVE
        JMP     PUSH

; -----------------------------------------------------------------------------
;
;       ?FILE   ( ... n )
;
;       Leaves a value indicating the current filing system on the stack.
;
; -----------------------------------------------------------------------------

.L8874  DEFWORD "?FILE"
        EQUW    L885B
.QUERYFILE
        EQUW    *+2
        TYA                     ; A := Y := 0
        JSR     OSARGS  
        JMP     PUSH0A

;       C@

.L8885  DEFWORD "C@"
        EQUW    L8874
.CFETCH EQUW    *+2
        LDA     (0,X)
        STA     0,X
        STY     1,X
        JMP     NEXT

;       @

.L8895  DEFWORD "@"
        EQUW    L8885
.FETCH  EQUW    *+2
        LDA     (0,X)
        PHA
        INC     0,X
        BNE     L88A4
        INC     1,X
.L88A4  LDA     (0,X)
        JMP     PUT

;       C!

.L88A9  DEFWORD "C!"
        EQUW    L8895
.CSTORE
        EQUW    *+2
        LDA     2,X
        STA     (0,X)
        JMP     POPTWO

;       !

.L88B7  DEFWORD "!"
        EQUW    L88A9
.STORE  EQUW    *+2
        LDA     2,X
        STA     (0,X)
        INC     0,X
        BNE     L88C7
        INC     1,X
.L88C7  LDA     3,X
        STA     (0,X)
        JMP     POPTWO

;       +!

.L88CE  DEFWORD "+!"
        EQUW    L88B7
.PLUSSTORE
        EQUW    *+2
        CLC
        LDA     (0,X)
        ADC     2,X
        STA     (0,X)
        INC     0,X
        BNE     L88E2
        INC     1,X
.L88E2  LDA     (0,X)
        ADC     3,X
        STA     (0,X)
        JMP     POPTWO

;       TOGGLE

.L88EB  DEFWORD "TOGGLE"
        EQUW    L88CE
.TOGGLE
        EQUW    *+2
        LDA     (2,X)
        EOR     0,X
        STA     (2,X)
        JMP     POPTWO

; -----------------------------------------------------------------------------
;
;       R:
;
;       > A recursive version of <:> used as:
;       >
;       > R: NNNN .... R;
;       >
;       > With this form of colon-definition references may be made from within
;       > the definition to the name NNNN itself. It should be used with care
;       > since any error during compilation will leave the incomplete
;       > definition in an executable form.
;
;       : R:
;        ?EXEC
;        !CSP
;        CURRENT @   CONTEXT !
;        CREATE
;        ]
;        (;CODE) ... machine code here ...
;       ;
;
; -----------------------------------------------------------------------------

.L88FF  DEFWORD "R:"
        EQUW    L88EB
.RCOLON EQUW    DOCOLON
        EQUW    QUERYEXEC
        EQUW    STORECSP
        EQUW    CURRENT
        EQUW    FETCH
        EQUW    CONTEXT
        EQUW    STORE
        EQUW    CREATE
        EQUW    RBRAC
        EQUW    BRACKETSEMICOLONCODE
.DOCOLON
        LDA     IP+1            ; SAVE IP OF THE WORD THAT IS
        PHA                     ; CALLING US ONTO THE RETURN
        LDA     IP              ; STACK
        PHA
        CLC                     ; SINCE W POINTS TO THE CURRENT
        LDA     W               ; WORD'S CFA, WHERE WE ARE CALLED
        ADC     #2              ; FROM, SETTING IP TO W+2 HERE MEANS
        STA     IP              ; POINTING IT TO THE WORD'S PFA
        TYA                     ; NOTE THAT Y IS ALWAYS ZERO
        ADC     W+1
        STA     IP+1
        JMP     NEXT

;       R;

.RSEMICOLON_NFA
        DEFIMM  "R;"
        EQUW    L88FF
.RSEMICOLON
        EQUW    DOCOLON
        EQUW    QUERYCSP
        EQUW    COMPILE
        EQUW    EXIT
        EQUW    LBRAC
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       CONSTANT   ( n ... )
;
;       > A defining word used in the form:
;       >
;       > n CONSTANT CCCC
;       >
;       > It creates a constant CCCC with the value n contained in its
;       > parameter field. When CCCC is executed the value n will be left on
;       > the stack.
;
;       : CONSTANT
;        CREATE
;        ,
;        (;CODE) ... machine code here ...
;       ;
;
; -----------------------------------------------------------------------------

.CONSTANT_NFA
        DEFWORD "CONSTANT"
        EQUW    RSEMICOLON_NFA
.CONSTANT
        EQUW    DOCOLON
        EQUW    CREATE
        EQUW    COMMA
        EQUW    BRACKETSEMICOLONCODE
.DOCONSTANT
        LDY     #2              ; Since at run-time (W) points to the CFA of
        LDA     (W),Y           ; the word, we add two to get to the parameter
        PHA                     ; field. We retrieve the constant value stored
        INY                     ; there and then PUSH it onto the data stack.
        LDA     (W),Y
        JMP     PUSH

; -----------------------------------------------------------------------------
;
;       VARIABLE
;
;       : VARIABLE
;        CREATE
;        0 ,
;        (;CODE) ... machine code here...
;       ;
;
; -----------------------------------------------------------------------------

.VARIABLE_NFA
        DEFWORD "VARIABLE"
        EQUW    CONSTANT_NFA
.VARIABLE
        EQUW    DOCOLON
        EQUW    CREATE
        EQUW    ZERO
        EQUW    COMMA
        EQUW    BRACKETSEMICOLONCODE
.DOVARIABLE
        CLC
        LDA     W               ; Since at run-time (W) points to the CFA of
        ADC     #2              ; the word, we add two to get to the parameter
        PHA                     ; field. We then PUSH its address onto the data
        TYA                     ; stack.
        ADC     W+1
        JMP     PUSH

; -----------------------------------------------------------------------------
;
;       USER   ( n ... )
;
;       > A defining word used in the form:
;       >
;       > n USER CCCC
;       >
;       > It creates a user variable CCCC , execution of which leaves the
;       > address, in the user area, of the value of CCCC . The value of n is
;       > the offset from the start of the user variable area to the memory
;       > location (2 bytes) in which the value is stored. The value is not
;       > initialised. Offsets from 0 to &30 incusive are used by the system.
;
;       : USER
;        CONSTANT
;        (;CODE) ... machine code here ...
;       ;
;       
; -----------------------------------------------------------------------------

.USER_NFA
        DEFWORD "USER"
        EQUW    VARIABLE_NFA
.USER   EQUW    DOCOLON
        EQUW    CONSTANT
        EQUW    BRACKETSEMICOLONCODE
.DOUSER LDY     #2              ; Since at run-time (W) points to the CFA of
        CLC                     ; the word, we add two to get to the parameter
        LDA     (W),Y           ; field. We retrieve the constant value stored
        ADC     UP              ; there, which is the offset in bytes from the
        PHA                     ; start of the user area. We add it to the
        LDA     #0              ; beginning of the user area as contained in
        ADC     UP+1            ; UP, and then PUSH onto the data stack.
        JMP     PUSH

; -----------------------------------------------------------------------------
;
;       -2   ( ... n )
;
;       > These often-used numerical values are defined as constants in the
;       > dictionary to save both time and dictionary space.
;
; -----------------------------------------------------------------------------

.MINUSTWO_NFA
        DEFWORD "-2"
        EQUW    USER_NFA
.MINUSTWO
        EQUW    DOCONSTANT
        EQUW    -2

; -----------------------------------------------------------------------------
;
;       -1   ( ... n )
;
;       See -2.
;
; -----------------------------------------------------------------------------

.MINUSONE_NFA
        DEFWORD "-1"
        EQUW    MINUSTWO_NFA
.MINUSONE
        EQUW    DOCONSTANT
        EQUW    -1

; -----------------------------------------------------------------------------
;
;       0   ( ... n )
;
;       See -2.
;
; -----------------------------------------------------------------------------

.ZERO_NFA
        DEFWORD "0"
        EQUW    MINUSONE_NFA
.ZERO   EQUW    DOCONSTANT
        EQUW    0

; -----------------------------------------------------------------------------
;
;       1   ( ... n )
;
;       See -2.
;
; -----------------------------------------------------------------------------

.ONE_NFA
        DEFWORD "1"
        EQUW    ZERO_NFA
.ONE    EQUW    DOCONSTANT
        EQUW    1

; -----------------------------------------------------------------------------
;
;       2   ( ... n )
;
;       See -2.
;
; -----------------------------------------------------------------------------

.TWO_NFA
        DEFWORD "2"
        EQUW    ONE_NFA
.TWO    EQUW    DOCONSTANT
        EQUW    2

;       BL

.BL_NFA DEFWORD "BL"
        EQUW    TWO_NFA
.BL     EQUW    DOCONSTANT
        EQUW    $20

; -----------------------------------------------------------------------------
;
;       C/L
;
;       > A constant containing the number of characters per line. This is
;       > normally 64, so a full FORTH 'line' will, in Mode 7, occupy about
;       > 1 1/2 lines of the VDU display.
;
; -----------------------------------------------------------------------------

.CSLASHL_NFA
        DEFWORD "C/L"
        EQUW    BL_NFA
.CSLASHL
        EQUW    DOCONSTANT
        EQUW    64

; -----------------------------------------------------------------------------
;
;       PAD   ( ... addr )
;
;       > A constant which leaves the address of the start of the text
;       > scratchpad buffer. Numeric output characters are stored downwards
;       > from PAD , character text is stored upwards.
;
; -----------------------------------------------------------------------------

.PAD_NFA
        DEFWORD "PAD"
        EQUW    CSLASHL_NFA
.PAD    EQUW    DOCONSTANT
        EQUW    PADD

;       B/BUF

.BSLASHBUF_NFA
        DEFWORD "B/BUF"
        EQUW    LA01C-REL
.BSLASHBUF
        EQUW    DOCONSTANT
        EQUW    BLKSIZ

;       B/SCR

.BSLASHSCR_NFA
        DEFWORD "B/SCR"
        EQUW    BSLASHBUF_NFA
.BSLASHSCR
        EQUW    DOCONSTANT
        EQUW    1

; -----------------------------------------------------------------------------
;
;       +ORIGIN   ( n ... addr )
;
;       > Leaves the address of the nth byte after the start of the boot-up
;       > parameter area. Used to access or modify the boot-up parameters.
;
;       : +ORIGIN
;        [ HEX 8000 100 + ] LITERAL +
;       ;
;
; -----------------------------------------------------------------------------

.PLUSORIGIN_NFA
        DEFWORD "+ORIGIN"
        EQUW    BSLASHSCR_NFA
.PLUSORIGIN
        EQUW    DOCOLON
        EQUW    LIT,ORIGIN
        EQUW    PLUS
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       S0   ( ... addr )
;
;       > A user variable containing the address which marks the initial top of
;       > the computation stack.
;
; -----------------------------------------------------------------------------

.SZERO_NFA
        DEFWORD "S0"
        EQUW    PLUSORIGIN_NFA
.SZERO  EQUW    DOUSER
        EQUB    $06

; -----------------------------------------------------------------------------
;
;       R0   ( ... addr )
;
;       > A user variable containing the initial address of the top of the
;       > return stack.
;
; -----------------------------------------------------------------------------

.RZERO_NFGA
        DEFWORD "R0"
        EQUW    SZERO_NFA
.RZERO  EQUW    DOUSER
        EQUB    $08

;       TIB

.TIB_NFA
        DEFWORD "TIB"
        EQUW    RZERO_NFGA
.TIB    EQUW    DOUSER
        EQUB    $0A

; -----------------------------------------------------------------------------
;
;       WIDTH   ( ... addr )
;
;       > A user variable containing the maximum number of letters saved during
;       > the compilation of a definition's name. It must be a value between 1
;       > and 31 inclusive and has a default value of 31. The value may be
;       > changed at any time provided it is kept within the above limits. Use
;       > of a value less than 3 is not recommended.
;
;       HEX 0C USER WIDTH
;
; -----------------------------------------------------------------------------

.WIDTH_NFA
        DEFWORD "WIDTH"
        EQUW    TIB_NFA
.WIDTH
        EQUW    DOUSER
        EQUB    $0C

;       WARNING

.WARNING_NFA
        DEFWORD "WARNING"
        EQUW    WIDTH_NFA
.WARNING
        EQUW    DOUSER
        EQUB    $0E

;       FENCE

.FENCE_NFA
        DEFWORD "FENCE"
        EQUW    WARNING_NFA
.FENCE  EQUW    DOUSER
        EQUB    $10

; -----------------------------------------------------------------------------
;
;       DP   ( ... addr )
;
;       > The dictionary pointer, a user variable which leaves the address
;       > addr, whose contents point to the first free byte at the top of the
;       > dictionary.
;
;       HEX 12 USER DP
;
; -----------------------------------------------------------------------------

.DP_NFA DEFWORD "DP"
        EQUW    FENCE_NFA
.DP     EQUW    DOUSER
        EQUB    $12

; -----------------------------------------------------------------------------
;
;       VOC-LINK   ( ... addr )
;
;       > A user variable containing the address of a vocabulary link field in
;       > the word which defines the most recently created vocabulary. All
;       > vocabularies are linked through these fields in their defining words.
;
;       HEX 14 USER VOC-LINK
;
; -----------------------------------------------------------------------------

.VOCLINK_NFA
        DEFWORD "VOC-LINK"
        EQUW    DP_NFA
.VOCLINK
        EQUW    DOUSER
        EQUB    $14

;       BLK

.L8A5E  DEFWORD "BLK"
        EQUW    VOCLINK_NFA
.BLK    EQUW    DOUSER
        EQUB    $16

;       >IN

.L8A67  DEFWORD ">IN"
        EQUW    L8A5E
.TOIN
        EQUW    DOUSER
        EQUB    $18

; -----------------------------------------------------------------------------
;
;       OUT   ( ... addr )
;
;       > A user variable containing a value that is incremented by EMIT . It
;       > may be examined and changed by the user to control display formats.
;
;       HEX 1A USER OUT
;
; -----------------------------------------------------------------------------

.L8A70  DEFWORD "OUT"
        EQUW    L8A67
.OUT    EQUW    DOUSER
        EQUB    $1A

;       SCR

.L8A79  DEFWORD "SCR"
        EQUW    L8A70
.SCR    EQUW    DOUSER
        EQUB    $1C

;       OFFSET

.L8A82  DEFWORD "OFFSET"
        EQUW    L8A79
.OFFSET EQUW    DOUSER
        EQUB    $1E

;       CONTEXT

.L8A8E  DEFWORD "CONTEXT"
        EQUW    L8A82
.CONTEXT
        EQUW    DOUSER
        EQUB    $20

;       CURRENT

.L8A9B  DEFWORD "CURRENT"
        EQUW    L8A8E
.CURRENT
        EQUW    DOUSER
        EQUB    $22

; -----------------------------------------------------------------------------
;
;       STATE   ( ... addr )
;
;       > A user variable indicating the state of compilation. A zero value
;       > indicates execution and a non-zero value indicates compilation.
;
; -----------------------------------------------------------------------------

.STATE_NFA
        DEFWORD "STATE"
        EQUW    L8A9B
.STATE  EQUW    DOUSER
        EQUB    $24

;       BASE

.L8AB3  DEFWORD "BASE"
        EQUW    STATE_NFA
.BASE   EQUW    DOUSER
        EQUB    $26

;       DPL

.L8ABD  DEFWORD "DPL"
        EQUW    L8AB3
.DPL    EQUW    DOUSER
        EQUB    $28

; -----------------------------------------------------------------------------
;
;       CSP   ( ... addr )
;
;       > A user variable used for temporary storage of the stack pointer in
;       > checking of compilation errors.
;
; -----------------------------------------------------------------------------

.CSP_NFA
        DEFWORD "CSP"
        EQUW    L8ABD
.CSP    EQUW    DOUSER
        EQUB    $2C

;       R#

.L8ACF  DEFWORD "R#"
        EQUW    CSP_NFA
.RSHARP EQUW    DOUSER
        EQUB    $2E

;       HLD

.L8AD7  DEFWORD "HLD"
        EQUW    L8ACF
.HLD    EQUW    DOUSER
        EQUB    $30

;       1+

.L8AE0  DEFWORD "1+"
        EQUW    L8AD7
.ONEPLUS
        EQUW    DOCOLON
        EQUW    ONE
        EQUW    PLUS
        EQUW    EXIT

;       2+

.L8AED  DEFWORD "2+"
        EQUW    L8AE0
.TWOPLUS
        EQUW    DOCOLON
        EQUW    TWO
        EQUW    PLUS
        EQUW    EXIT

        EQUB    0

;       1-

.L8AFB  DEFWORD "1-"
        EQUW    L8AED
.ONEMINUS
        EQUW    DOCOLON
        EQUW    MINUSONE
        EQUW    PLUS
        EQUW    EXIT

;       2-

.L8B08  DEFWORD "2-"
        EQUW    L8AFB
.TWOMINUS
        EQUW    DOCOLON
        EQUW    MINUSTWO
        EQUW    PLUS
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       HERE   ( ... addr )
;
;       > Leaves the contents of DP , i.e. the address of the first unused byte
;       > in the dictionary.
;
;       : HERE
;        DP @
;       ;
;
; -----------------------------------------------------------------------------

.HERE_NFA
        DEFWORD "HERE"
        EQUW    L8B08
.HERE   EQUW    DOCOLON
        EQUW    DP
        EQUW    FETCH
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       ALLOT   ( n ... )
;
;       > The value of n is added to the dictionary pointer to reserve n bytes
;       > of dictionary space. The dictionary pointer may be moved backwards
;       > by use of a negative n but this should be used with caution to avoid
;       > losing essential dictionary content.
;
;       : ALLOT
;        DP +!
;       ;
;
; -----------------------------------------------------------------------------

.ALLOT_NFA
        DEFWORD "ALLOT"
        EQUW    HERE_NFA
.ALLOT  EQUW    DOCOLON
        EQUW    DP
        EQUW    PLUSSTORE
        EQUW    EXIT

;       SPACE

.L8B34  DEFWORD "SPACE"
        EQUW    ALLOT_NFA
.SPACE  EQUW    DOCOLON
        EQUW    BL
        EQUW    EMIT
        EQUW    EXIT

;       ,

.L8B44  DEFWORD ","
        EQUW    L8B34
.COMMA  EQUW    DOCOLON
        EQUW    HERE
        EQUW    STORE
        EQUW    TWO
        EQUW    ALLOT
        EQUW    EXIT

;       C,

.L8B54  DEFWORD "C,"
        EQUW    L8B44
.CCOMMA
        EQUW    DOCOLON
        EQUW    HERE
        EQUW    CSTORE
        EQUW    ONE
        EQUW    ALLOT
        EQUW    EXIT

;       LAST

.L8B65  DEFWORD "LAST"
        EQUW    L8B54
.LAST   EQUW    DOCOLON
        EQUW    CURRENT
        EQUW    FETCH
        EQUW    FETCH
        EQUW    EXIT

;       SMUDGE

.L8B76  DEFWORD "SMUDGE"
        EQUW    L8B65
.SMUDG  EQUW    DOCOLON
        EQUW    LAST
        EQUW    LIT,$20
        EQUW    TOGGLE
        EQUW    EXIT

;       -

.L8B8B  DEFWORD "-"
        EQUW    L8B76
.MINUS
        EQUW    DOCOLON
        EQUW    NEGATE
        EQUW    PLUS
        EQUW    EXIT

;       =

.L8B97  DEFWORD "="
        EQUW    L8B8B
.EQUAL  EQUW    DOCOLON
        EQUW    MINUS
        EQUW    ZEROEQUAL
        EQUW    EXIT

;       >

.L8BA3  DEFWORD ">"
        EQUW    L8B97
.GREATERTHAN
        EQUW    DOCOLON
        EQUW    SWAP
        EQUW    LESS
        EQUW    EXIT

;       U<

.L8BAF  DEFWORD "U<"
        EQUW    L8BA3
.ULESS  EQUW    DOCOLON
        EQUW    ZERO
        EQUW    SWAP
        EQUW    ZERO
        EQUW    DLESS
        EQUW    EXIT

;       LFA

.L8BC0  DEFWORD "LFA"
        EQUW    L8BAF
.LFA    EQUW    DOCOLON
        EQUW    LIT,4
        EQUW    MINUS
        EQUW    EXIT

;       NFA

.L8BD0  DEFWORD "NFA"
        EQUW    L8BC0
.NFA    EQUW    DOCOLON
        EQUW    LIT,5
        EQUW    MINUS
        EQUW    MINUSONE
        EQUW    TRAVERSE
        EQUW    EXIT

;       CFA

.L8BE4  DEFWORD "CFA"
        EQUW    L8BD0
.CFA    EQUW    DOCOLON
        EQUW    TWOMINUS
        EQUW    EXIT

;       PFA

.L8BF0  DEFWORD "PFA"
        EQUW    L8BE4
.PFA    EQUW    DOCOLON
        EQUW    ONE
        EQUW    TRAVERSE
        EQUW    LIT,5
        EQUW    PLUS
        EQUW    EXIT

;       NOT

.NOT_NFA
        DEFWORD "NOT"
        EQUW    L8BF0
.NOT    EQUW    DOCOLON
        EQUW    ZEROEQUAL
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       !CSP
;
;       > Stores the stack pointer value in user variable CSP. Used as part of
;       > the compiler security.
;
;       : !CSP  SP@ CSP !  ;
;
; -----------------------------------------------------------------------------

.L8C10  DEFWORD "!CSP"
        EQUW    NOT_NFA
.STORECSP
        EQUW    DOCOLON
        EQUW    SPFETCH
        EQUW    CSP
        EQUW    STORE
        EQUW    EXIT

;       ?ERROR

.QUERYERROR_NFA
        DEFWORD "?ERROR"
        EQUW    L8C10
.QUERYERROR
        EQUW    DOCOLON
        EQUW    SWAP
        EQUW    ZEROBRANCH,8
        EQUW    ERROR
        EQUW    BRANCH,4
        EQUW    DROP
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       ?COMP
;
;       > Issues an error message if not compiling.
;
;       : ?COMP
;        STATE @   0= 17 ?ERROR
;       ;
;
; -----------------------------------------------------------------------------

.QUERYCOMP_NFA
        DEFWORD "?COMP"
        EQUW    QUERYERROR_NFA
.QUERYCOMP
        EQUW    DOCOLON
        EQUW    STATE
        EQUW    FETCH
        EQUW    ZEROEQUAL
        EQUW    LIT,17
        EQUW    QUERYERROR
        EQUW    EXIT

;       ?EXEC

.L8C54  DEFWORD "?EXEC"
        EQUW    QUERYCOMP_NFA
.QUERYEXEC
        EQUW    DOCOLON
        EQUW    STATE
        EQUW    FETCH
        EQUW    LIT,18
        EQUW    QUERYERROR
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       ?PAIRS   ( n1\n2 ... )
;
;       > Issues an error message if n1 does not equal n2. The message
;       > indicates that compiled conditionals (IF ... ELSE ... THEN or BEGIN
;       > ... UNTIL etc.) do not match. It is part of the compiler security.
;       > The error message is given if, for example, the sequence IF ... UNTIL
;       > is found during compilation of a dictionary entry.
;
;       : ?PAIRS
;        -
;        19 ?ERROR
;       ;
;
; -----------------------------------------------------------------------------

.QUERYPAIRS_NFA
        DEFWORD "?PAIRS"
        EQUW    L8C54
.QUERYPAIRS
        EQUW    DOCOLON
        EQUW    MINUS
        EQUW    LIT,19
        EQUW    QUERYERROR
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       ?CSP
;
;       > Issues an error message if stack position differs from that saved in
;       > CSP . Used as part of the compiler security.
;
;       : ?CSP
;        SP@
;        CSP @ -
;        20 ?ERROR
;       ;
;
; -----------------------------------------------------------------------------

.QUERYCSP_NFA
        DEFWORD "?CSP"
        EQUW    QUERYPAIRS_NFA
.QUERYCSP
        EQUW    DOCOLON
        EQUW    SPFETCH
        EQUW    CSP
        EQUW    FETCH
        EQUW    MINUS
        EQUW    LIT,20
        EQUW    QUERYERROR
        EQUW    EXIT

;       ?LOADING

.L8C98  DEFWORD "?LOADING"
        EQUW    QUERYCSP_NFA
.QUERYLOADING
        EQUW    DOCOLON
        EQUW    BLK
        EQUW    FETCH
        EQUW    ZEROEQUAL
        EQUW    LIT,22
        EQUW    QUERYERROR
        EQUW    EXIT

;       IMMEDIATE

.IMMEDIATE_NFA
        DEFWORD "IMMEDIATE"
        EQUW    L8C98
.IMMEDIATE
        EQUW    DOCOLON
        EQUW    LAST
        EQUW    LIT,$40
        EQUW    TOGGLE
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       COMPILE
;
;       > COMPILE acts during the execution of the word containing it. The code
;       > field (execution) address of the word following COMPILE is compiled
;       > into the dictionary instead of executing, cf. [COMPILE] .
;
;       : COMPILE
;        ?COMP
;        R>    DUP 2+ >R
;        @ ,
;       ;
;
; -----------------------------------------------------------------------------

.COMPILE_NFA
        DEFWORD "COMPILE"
        EQUW    IMMEDIATE_NFA
.COMPILE
        EQUW    DOCOLON
        EQUW    QUERYCOMP
        EQUW    RFROM
        EQUW    DUP
        EQUW    TWOPLUS
        EQUW    TOR
        EQUW    FETCH
        EQUW    COMMA
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       [
;
;       > Used in the creation of a colon-definition in the form:
;       >
;       > : NNNN ... [ ... ] ... ;
;       >
;       > It suspends compilation of the definition and allows the subsequent
;       > input to be executed. See ] .
;
;       : [
;        0 STATE !
;       ; IMMEDIATE
;
; -----------------------------------------------------------------------------

.LBRAC_NFA
        DEFIMM  "["
        EQUW    COMPILE_NFA
.LBRAC  EQUW    DOCOLON
        EQUW    ZERO
        EQUW    STATE
        EQUW    STORE
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       ]
;
;       > Used during execution mode to force compilation of the subsequent
;       > input. See [ .
;
;       : ]
;        $C0 STATE !
;       ;
;
; -----------------------------------------------------------------------------

.RBRAC_NFA
        DEFWORD "]"
        EQUW    LBRAC_NFA
.RBRAC  EQUW    DOCOLON
        EQUW    LIT,$C0
        EQUW    STATE
        EQUW    STORE
        EQUW    EXIT

;       NOOP

; -----------------------------------------------------------------------------
;
;       NOOP
;
;       > A no-operation in FORTH. One possible use is to reserve address space
;       > in a colon-definition for later over-writing by the execution address
;       > of a subsequent definition.
;
;       : NOOP ;
;
; -----------------------------------------------------------------------------

.NOOP_NFA
        DEFWORD "NOOP"
        EQUW    RBRAC_NFA
.NOOP   EQUW    DOCOLON
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       ?TAB   ( ... f )
;
;       > Tests if the TAB key is pressed at the instant ?TAB is called. A true
;       > flag indicates that the key was pressed, otherwise a false flag is
;       > left.
;
;       : ?TAB
;        -97 ?KEY
;        1 AND
;       ;
;
; -----------------------------------------------------------------------------

.QTAB_NFA
        DEFWORD "?TAB"
        EQUW    NOOP_NFA
.QTAB   EQUW    DOCOLON
        EQUW    LIT,-97
        EQUW    QUERYKEY
        EQUW    ONE
        EQUW    AND
        EQUW    EXIT

;       HEX

.L8D25  DEFWORD "HEX"
        EQUW    QTAB_NFA
.HEX    EQUW    DOCOLON
        EQUW    LIT,16
        EQUW    BASE
        EQUW    STORE
        EQUW    EXIT

;       DECIMAL

.L8D37  DEFWORD "DECIMAL"
        EQUW    L8D25
.DECIM  EQUW    DOCOLON
        EQUW    LIT,10
        EQUW    BASE
        EQUW    STORE
        EQUW    EXIT

;       CR

.L8D4D  DEFWORD "CR"
        EQUW    L8D37
.CR     EQUW    DOCOLON
        EQUW    LIT,LineFeed
        EQUW    EMIT
        EQUW    LIT,CarriageReturn
        EQUW    EMIT
        EQUW    MINUSTWO
        EQUW    OUT
        EQUW    PLUSSTORE
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       (;CODE)
;
;       > The run-time procedure compiled by ;CODE that rewrites the code field
;       > address of the most recently defined word to point to the machine
;       > code following (;CODE) . It is used by the system defining words
;       > ( <:>, CONSTANT etc.) to define the machine code actions of
;       > dictionary entries using them. This is, in a sense, a machine-code
;       > version of DOES> .
;
; -----------------------------------------------------------------------------

.L8D68  DEFWORD "(;CODE)"
        EQUW    L8D4D
.BRACKETSEMICOLONCODE
        EQUW    DOCOLON
        EQUW    RFROM
        EQUW    LAST
        EQUW    PFA
        EQUW    CFA
        EQUW    STORE
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       ;CODE
;
;       > The use of this word requires the ASSEMBLER vocabulary to be loaded.
;       > Use in the form
;       >
;       > : NNNN ... ;CODE ... (assembler words) ... END-CODE
;       >
;       > Compilation of the definition NNNN is terminated and ASSEMBLER
;       > becomes the CONTEXT vocabulary. A defining word NNNN is created which
;       > when executed in the form
;       >
;       > NNNN CCCC
;       >
;       > will create a new word CCCC. When CCCC is itself executed it action
;       > will be determined by the machine code following ;CODE in NNNN .
;
;       : ;CODE
;        ?CSP
;        COMPILE (;CODE)
;        [ ASSEMBLER
;        !CSP
;       ; IMMEDATE
;
; -----------------------------------------------------------------------------

.L8D80  DEFIMM  ";CODE"
        EQUW    L8D68
.L8D88  EQUW    DOCOLON
        EQUW    QUERYCSP
        EQUW    COMPILE
        EQUW    BRACKETSEMICOLONCODE
        EQUW    LBRAC
        EQUW    ASSEMBLER
        EQUW    STORECSP
        EQUW    EXIT

; -----------------------------------------------------------------------------
;        
;       DOES>
;
;       > Used with CREATE in the form:
;       >
;       > : NNNN CREATE ... DOES> ... ;
;       >
;       > It creates a new defining word NNNN . Executing NNNN in the form
;       >
;       > NNNN CCCC
;       >
;       > creates a new word CCCC whose parameter area is allocated by the
;       > words following CREATE and whose action is governed by the words
;       > following DOES> in NNNN .
;
; -----------------------------------------------------------------------------

.L8D98  DEFIMM  "DOES>"
        EQUW    L8D80
.DOES   EQUW    DOCOLON
        EQUW    COMPILE
        EQUW    BRACKETSEMICOLONCODE
        EQUW    LIT,$20
        EQUW    CCOMMA
        EQUW    COMPILE
        EQUW    DODOES
        EQUW    EXIT

.DODOES DEX
        DEX
        CLC
        LDA     W
        ADC     #2
        STA     0,X
        TYA
        ADC     W+1
        STA     1,X
        SEC
        PLA
        SBC     #1
        STA     W
        PLA
        SBC     #0
        STA     W+1
        JMP     DOCOLON

;       0>

.L8DCE  DEFWORD "0>"
        EQUW    L8D98
.ZEROGREATER
        EQUW    DOCOLON
        EQUW    NEGATE
        EQUW    ZEROLESS
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       COUNT   ( addr1 ... addr2\n )
;
;       > Leaves the address addr2 and byte count n of a text string starting
;       > at addr1, in a form suitable for use by TYPE . It is assumed that the
;       > text string has its count byte at addr1 and that the actual character
;       > string starts at addr1 + 1.
;
;       : COUNT
;        DUP 1+ SWAP C@
;       ;
;
; -----------------------------------------------------------------------------

.COUNT_NFA
        DEFWORD "COUNT"
        EQUW    L8DCE
.COUNT  EQUW    DOCOLON
        EQUW    DUP
        EQUW    ONEPLUS
        EQUW    SWAP
        EQUW    CFETCH
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       TYPE   ( addr\count ... )
;
;       > Transmits 'count' characters of a string starting at the address addr
;       > to the output device.
;
;       : TYPE
;        DUP 0> IF
;         OVER + SWAP DO
;          I C@ EMIT
;         LOOP
;        ELSE
;         2DROP
;        THEN
;       ;
;
; -----------------------------------------------------------------------------

.TYPE_NFA
        DEFWORD "TYPE"
        EQUW    COUNT_NFA
.TYPE   EQUW    DOCOLON
        EQUW    DUP
        EQUW    ZEROGREATER
        EQUW    ZEROBRANCH,$18
        EQUW    OVER
        EQUW    PLUS
        EQUW    SWAP
        EQUW    XDO
        EQUW    IDO
        EQUW    CFETCH
        EQUW    EMIT
        EQUW    BRACKETULOOP,$FFF8
        EQUW    BRANCH,4
        EQUW    TWODROP
        EQUW    EXIT

;       -TRAILING

.L8E1A  DEFWORD "-TRAILING"
        EQUW    TYPE_NFA
.DTRAI  EQUW    DOCOLON
        EQUW    DUP
        EQUW    ZEROLESS
        EQUW    LIT,5
        EQUW    QUERYERROR
        EQUW    DUP
        EQUW    ZERO
        EQUW    XDO
        EQUW    TWODUP
        EQUW    PLUS
        EQUW    ONEMINUS
        EQUW    CFETCH
        EQUW    BL
        EQUW    MINUS
        EQUW    ZEROBRANCH,8
        EQUW    LEAVE
        EQUW    BRANCH,4
        EQUW    ONEMINUS
        EQUW    BRACKETULOOP,$FFE6
        EQUW    EXIT

;       TEXT,

.L8E56  DEFWORD "TEXT,"
        EQUW    L8E1A
.TEXTCOMMA
        EQUW    DOCOLON
        EQUW    DUP
        EQUW    CCOMMA
        EQUW    HERE
        EQUW    OVER
        EQUW    ALLOT
        EQUW    SWAP
        EQUW    CMOVE
        EQUW    EXIT

;       (.")

.L8E70  DEFWORD "(."")"
        EQUW    L8E56
.BRACKETDOTQUOTE
        EQUW    DOCOLON
        EQUW    RFETCH
        EQUW    COUNT
        EQUW    DUP
        EQUW    ONEPLUS
        EQUW    RFROM
        EQUW    PLUS
        EQUW    TOR
        EQUW    TYPE
        EQUW    EXIT

;       ."

.L8E8B  DEFIMM  "."""
        EQUW    L8E70
.DOTQ   EQUW    DOCOLON
        EQUW    MINUSONE
        EQUW    TOIN
        EQUW    PLUSSTORE
        EQUW    LIT,$22
        EQUW    PWORD
        EQUW    ONEMINUS
        EQUW    QUERYDUP
        EQUW    ZEROBRANCH,$20
        EQUW    SWAP
        EQUW    ONEPLUS
        EQUW    SWAP
        EQUW    STATE
        EQUW    FETCH
        EQUW    ZEROBRANCH,$C
        EQUW    COMPILE
        EQUW    BRACKETDOTQUOTE
        EQUW    TEXTCOMMA
        EQUW    BRANCH,4
        EQUW    TYPE
        EQUW    BRANCH,4
        EQUW    DROP
        EQUW    EXIT

;       (EXPECT)

.L8EC8  DEFWORD "(EXPECT)"
        EQUW    L8E8B
.PEXPEC EQUW    *+2
        STX     XSAVE
        DEX
        LDA     3,X
        STA     0,X
        LDA     1,X
        STA     2,X
        LDA     4,X
        STA     1,X
        LDA     #$20
        STA     3,X
        LDA     #$FF
        STA     4,X
        LDA     #ReadLine
        JSR     OSWORD
        LDX     XSAVE
        STY     2,X
        LDA     #0
        STA     3,X
        JMP     POP

;       EXPECT

.L8EFC  DEFWORD "EXPECT"
        EQUW    L8EC8
.EXPECT EQUW    DOCOLON
        EQUW    OVER
        EQUW    SWAP
        EQUW    PEXPEC
        EQUW    PLUS
        EQUW    ZERO
        EQUW    SWAP
        EQUW    STORE
        EQUW    EXIT

;       QUERY

.L8F17  DEFWORD "QUERY"
        EQUW    L8EFC
.QUERY  EQUW    DOCOLON
        EQUW    TIB
        EQUW    FETCH
        EQUW    LIT,80
        EQUW    EXPECT
        EQUW    ZERO
        EQUW    TOIN
        EQUW    STORE
        EQUW    EXIT

;       ASCII NULL

.L8F33  DEFIMM  CHR$(0)
        EQUW    L8F17
.NULL   EQUW    DOCOLON
        EQUW    BLK
        EQUW    FETCH
        EQUW    ZEROBRANCH,$28
        EQUW    ONE
        EQUW    BLK
        EQUW    PLUSSTORE
        EQUW    ZERO
        EQUW    TOIN
        EQUW    STORE
        EQUW    BLK
        EQUW    FETCH
        EQUW    BSLASHSCR
        EQUW    ONEMINUS
        EQUW    AND
        EQUW    ZEROEQUAL
        EQUW    ZEROBRANCH,8
        EQUW    QUERYEXEC
        EQUW    RFROM
        EQUW    DROP
        EQUW    BRANCH,6
        EQUW    RFROM
        EQUW    DROP
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       MOVE   ( from\to\count ... )
;
;       > Moves 'count' words (16-bit values) starting at 'from' to the block
;       > of memory starting at 'to'. The 16-bit value at 'from' is moved first
;       > and the transfer proceeds towards high memory. No check is made as to
;       > whether the destination and source areas overlap. If 'count' is zero
;       > or negative nothing is moved.
;
;       : MOVE
;        DUP + CMOVE  ( duplicate 'count' and call CMOVE )
;       ;
;
; -----------------------------------------------------------------------------

.L8F6D  DEFWORD "MOVE"
        EQUW    L8F33
.MOVE   EQUW    DOCOLON
        EQUW    DUP
        EQUW    PLUS
        EQUW    CMOVE
        EQUW    EXIT

;       FILL

.L8F7E  DEFWORD "FILL"
        EQUW    L8F6D
.FILL   EQUW    DOCOLON
        EQUW    OVER
        EQUW    ONE
        EQUW    LESS
        EQUW    ZEROBRANCH,$A
        EQUW    DROP
        EQUW    TWODROP
        EQUW    BRANCH,$14
        EQUW    SWAP
        EQUW    TOR
        EQUW    OVER
        EQUW    CSTORE
        EQUW    DUP
        EQUW    ONEPLUS
        EQUW    RFROM
        EQUW    ONEMINUS
        EQUW    CMOVE
        EQUW    EXIT

;       ERASE

.L8FAD  DEFWORD "ERASE"
        EQUW    L8F7E
.ERASE  EQUW    DOCOLON
        EQUW    ZERO
        EQUW    FILL
        EQUW    EXIT

;       BLANKS

.L8FBD  DEFWORD "BLANKS"
        EQUW    L8FAD
.BLANKS EQUW    DOCOLON
        EQUW    BL
        EQUW    FILL
        EQUW    EXIT

;       HOLD

.L8FCE  DEFWORD "HOLD"
        EQUW    L8FBD
.HOLD   EQUW    DOCOLON
        EQUW    MINUSONE
        EQUW    HLD
        EQUW    PLUSSTORE
        EQUW    HLD
        EQUW    FETCH
        EQUW    CSTORE
        EQUW    EXIT

;       (WORD)  ( C -- ADR LEN )

.L8FE5  DEFWORD "(WORD)"
        EQUW    L8FCE
.PWORD  EQUW    DOCOLON
        EQUW    BLK
        EQUW    FETCH
        EQUW    QUERYDUP
        EQUW    ZEROBRANCH,8
        EQUW    BLOCK
        EQUW    BRANCH,6
        EQUW    TIB
        EQUW    FETCH
        EQUW    TOIN
        EQUW    FETCH
        EQUW    PLUS
        EQUW    SWAP
        EQUW    ENCLOSE
        EQUW    TOIN
        EQUW    PLUSSTORE
        EQUW    OVER
        EQUW    MINUS
        EQUW    ROT
        EQUW    ROT
        EQUW    PLUS
        EQUW    SWAP
        EQUW    EXIT

;       WDSZ

.L9020  DEFWORD "WDSZ"
        EQUW    L8FE5
.WDSZ   EQUW    DOCONSTANT
        EQUW    WBSIZ           ; WORD BUFFER SIZE

;       WBFR

.L902B  DEFWORD "WBFR"
        EQUW    L9020
.WBFR   EQUW    DOCONSTANT
        EQUW    WORDBU          ; WORD BUFFER ADDR

;       1WORD

.L9036  DEFWORD "1WORD"
        EQUW    L902B
.ONEWRD EQUW    DOCOLON
        EQUW    PWORD
        EQUW    WDSZ
        EQUW    MIN
        EQUW    WBFR
        EQUW    CSTORE
        EQUW    WBFR
        EQUW    COUNT
        EQUW    ONEPLUS
        EQUW    CMOVE
        EQUW    WBFR
        EQUW    EXIT

;       WORD

.L9056  DEFWORD "WORD"
        EQUW    L9036
.WORD   EQUW    DOCOLON
        EQUW    ONEWRD
        EQUW    DUP
        EQUW    ONEPLUS
        EQUW    CFETCH
        EQUW    ZEROEQUAL
        EQUW    ZEROBRANCH,8
        EQUW    ZERO
        EQUW    OVER
        EQUW    CSTORE
        EQUW    EXIT

;       CONVERT

.L9075  DEFWORD "CONVERT"
        EQUW    L9056
.CONV   EQUW    DOCOLON
        EQUW    ONEPLUS
        EQUW    DUP
        EQUW    TOR
        EQUW    CFETCH
        EQUW    BASE
        EQUW    FETCH
        EQUW    DIGIT
        EQUW    ZEROBRANCH,$1C
        EQUW    SWAP
        EQUW    BASE
        EQUW    FETCH
        EQUW    USTAR
        EQUW    DROP
        EQUW    ROT
        EQUW    BASE
        EQUW    FETCH
        EQUW    USTAR
        EQUW    DPLUS
        EQUW    RFROM
        EQUW    BRANCH,-42
        EQUW    RFROM
        EQUW    EXIT

;       -FIND

.L90B1  DEFWORD "-FIND"
        EQUW    L9075
.DFIND  EQUW    DOCOLON
        EQUW    BL
        EQUW    ONEWRD
        EQUW    SWAP
        EQUW    BRACKETFIND
        EQUW    EXIT

;       FIND

.L90C5  DEFWORD "FIND"
        EQUW    L90B1
.FIND   EQUW    DOCOLON
        EQUW    CONTEXT
        EQUW    FETCH
        EQUW    FETCH
        EQUW    DFIND
        EQUW    ZEROBRANCH,8
        EQUW    DROP
        EQUW    BRANCH,4
        EQUW    ZERO
        EQUW    EXIT

;       ERROR

.L90E4  DEFWORD "ERROR"
        EQUW    L90C5
.ERROR  EQUW    DOCOLON
        EQUW    WARNING
        EQUW    FETCH
        EQUW    ZEROLESS
        EQUW    ZEROBRANCH,8
        EQUW    ABORT
        EQUW    BRANCH,$1F
        EQUW    WBFR
        EQUW    COUNT
        EQUW    TYPE
        EQUW    BRACKETDOTQUOTE
        EQUB    4,"  ? "
        EQUW    MESSAGE
        EQUW    SPSTORE
        EQUW    TWODROP
        EQUW    TOIN
        EQUW    FETCH
        EQUW    BLK
        EQUW    FETCH
        EQUW    QUIT
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       ID.   ( addr ... )
;
;       > Prints the name of a word from its name field address on the stack.
;
;       : ID.
;        PAD BL 95 FILL         ( clear PAD with spaces )
;        DUP PFA LFA            ( get to the LFA )
;        OVER -                 ( calculate the length of the name)
;        PAD SWAP CMOVE         ( copy the length-prepended name to PAD )
;        PAD COUNT              ( convert to a string and its length )
;        31 AND                 ( limit the length to 31 characters )
;        TYPE SPACE             ( print the name, followed by a space )
;       ;
;
; -----------------------------------------------------------------------------

.L911D  DEFWORD "ID."
        EQUW    L90E4
.IDDOT  EQUW    DOCOLON
        EQUW    PAD
        EQUW    BL
        EQUW    LIT,95
        EQUW    FILL
        EQUW    DUP
        EQUW    PFA
        EQUW    LFA
        EQUW    OVER
        EQUW    MINUS
        EQUW    PAD
        EQUW    SWAP
        EQUW    CMOVE
        EQUW    PAD
        EQUW    COUNT
        EQUW    LIT,31
        EQUW    AND
        EQUW    TYPE
        EQUW    SPACE
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       (CREATE)
;
;       > See CREATE .
;
; -----------------------------------------------------------------------------

.L914F  DEFWORD "(CREATE)"
        EQUW    L911D
.BRACKETCREATE
        EQUW    DOCOLON
        EQUW    FIRST
        EQUW    HERE
        EQUW    LIT,$A0
        EQUW    PLUS
        EQUW    ULESS
        EQUW    TWO
        EQUW    QUERYERROR
        EQUW    BL
        EQUW    WORD
        EQUW    DUP
        EQUW    CFETCH
        EQUW    DUP
        EQUW    ZEROEQUAL
        EQUW    LIT,10
        EQUW    QUERYERROR
        EQUW    OVER
        EQUW    CONTEXT
        EQUW    FETCH
        EQUW    FETCH
        EQUW    BRACKETFIND
        EQUW    ZEROBRANCH,$12
        EQUW    DROP
        EQUW    TWOPLUS
        EQUW    NFA
        EQUW    IDDOT
        EQUW    LIT,4
        EQUW    MESSAGE
        EQUW    SPACE
        EQUW    WIDTH
        EQUW    FETCH
        EQUW    MIN
        EQUW    DUP
        EQUW    DP
        EQUW    CFETCH
        EQUW    PLUS
        EQUW    LIT,$FC
        EQUW    EQUAL
        EQUW    ALLOT
        EQUW    ONEPLUS
        EQUW    DUP
        EQUW    TOR
        EQUW    HERE
        EQUW    SWAP
        EQUW    CMOVE
        EQUW    HERE
        EQUW    RFROM
        EQUW    ALLOT
        EQUW    DUP
        EQUW    LIT,$80
        EQUW    TOGGLE
        EQUW    HERE
        EQUW    ONEMINUS
        EQUW    LIT,$80
        EQUW    TOGGLE
        EQUW    LAST
        EQUW    COMMA
        EQUW    CURRENT
        EQUW    FETCH
        EQUW    STORE
        EQUW    LIT,DOVARIABLE
        EQUW    COMMA
        EQUW    EXIT

;       [COMPILE]

.L91E8  DEFIMM  "[COMPILE]"
        EQUW    LA028-REL
.BCOMP  EQUW    DOCOLON
        EQUW    CONTEXT
        EQUW    FETCH
        EQUW    FETCH
        EQUW    DFIND
        EQUW    ZEROEQUAL
        EQUW    ZERO
        EQUW    QUERYERROR
        EQUW    DROP
        EQUW    COMMA
        EQUW    EXIT

;       LITERAL

.LITERAL_NFA
        DEFIMM  "LITERAL"
        EQUW    L91E8
.LITERAL
        EQUW    DOCOLON
        EQUW    STATE
        EQUW    FETCH
        EQUW    ZEROBRANCH,8
        EQUW    COMPILE
        EQUW    LIT
        EQUW    COMMA
        EQUW    EXIT

;       DLITERAL

.L9226  DEFIMM  "DLITERAL"
        EQUW    LITERAL_NFA
.DLITER EQUW    DOCOLON
        EQUW    STATE
        EQUW    FETCH
        EQUW    ZEROBRANCH,8
        EQUW    SWAP
        EQUW    LITERAL
        EQUW    LITERAL
        EQUW    EXIT

;       ?STACK

.L9243  DEFWORD "?STACK"
        EQUW    L9226
.QSTAC  EQUW    DOCOLON
        EQUW    SPFETCH
        EQUW    SZERO
        EQUW    FETCH
        EQUW    GREATERTHAN
        EQUW    ONE
        EQUW    QUERYERROR
        EQUW    SPFETCH
        EQUW    LIT,BOS
        EQUW    LESS
        EQUW    LIT,7
        EQUW    QUERYERROR
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       :
;
;       > Used to create a colon-definition in the form
;       >
;       > : CCCC .... ;
;       >
;       > Creates a dictionary entry for the word CCCC as being equivalent to
;       > the sequence of FORTH words until the next <;>. Each word in the
;       > sequence is compiled into the dictionary entry, unless it is in the
;       > immediate execution mode.
;
;       : :
;        R:   SMUDGE
;       ;
;
; -----------------------------------------------------------------------------

.COLON_NFA
        DEFWORD ":"
        EQUW    L9243
.COLON  EQUW    DOCOLON
        EQUW    RCOLON
        EQUW    SMUDG
        EQUW    EXIT

;       ;

.L9276  DEFIMM  ";"
        EQUW    COLON_NFA
.SEMIS  EQUW    DOCOLON
        EQUW    RSEMICOLON
        EQUW    SMUDG
        EQUW    EXIT

;       NUMBER

.L9282  DEFWORD "NUMBER"
        EQUW    L9276
.NUMBER EQUW    DOCOLON
        EQUW    DUP
        EQUW    CFETCH
        EQUW    OVER
        EQUW    PLUS
        EQUW    TOR
        EQUW    ZERO
        EQUW    ZERO
        EQUW    ROT
        EQUW    DUP
        EQUW    ONEPLUS
        EQUW    CFETCH
        EQUW    LIT,'-'
        EQUW    EQUAL
        EQUW    DUP
        EQUW    TOR
        EQUW    PLUS
        EQUW    CONV
        EQUW    RFROM
        EQUW    ZEROBRANCH,8
        EQUW    TOR
        EQUW    DNEGATE
        EQUW    RFROM
        EQUW    RFROM
        EQUW    OVER
        EQUW    MINUS
        EQUW    DUP
        EQUW    ZEROLESS
        EQUW    ZEROBRANCH,8
        EQUW    TWODROP
        EQUW    BRANCH,$12
        EQUW    ZERO
        EQUW    QUERYERROR
        EQUW    CFETCH
        EQUW    LIT,'.'
        EQUW    MINUS
        EQUW    ZERO
        EQUW    QUERYERROR
        EQUW    EXIT

;       (

.L92E3  DEFIMM  "("
        EQUW    L9282
.PAREN  EQUW    DOCOLON
        EQUW    MINUSONE
        EQUW    TOIN
        EQUW    PLUSSTORE
        EQUW    LIT,')'
        EQUW    WORD
        EQUW    DROP
        EQUW    EXIT

;       (NUM)

.L92F9  DEFWORD "(NUM)"
        EQUW    L92E3
.BRACKETNUM
        EQUW    DOCOLON
        EQUW    DUP
        EQUW    CFETCH
        EQUW    OVER
        EQUW    PLUS
        EQUW    SWAP
        EQUW    NUMBER
        EQUW    ROT
        EQUW    CFETCH
        EQUW    LIT,'.'
        EQUW    MINUS
        EQUW    ZEROBRANCH,$A
        EQUW    DROP
        EQUW    LITERAL
        EQUW    BRANCH,4
        EQUW    DLITER
        EQUW    EXIT

;       INTERPRET

.L9329  DEFWORD "INTERPRET"
        EQUW    LA035-REL
.INTERPRET
        EQUW    DOCOLON

        EQUW    CONTEXT
        EQUW    FETCH
        EQUW    FETCH
        EQUW    DFIND
        EQUW    ZEROBRANCH,$18
        EQUW    STATE
        EQUW    FETCH
        EQUW    LESS
        EQUW    ZEROBRANCH,8
        EQUW    COMMA
        EQUW    BRANCH,4
        EQUW    EXECUTE
        EQUW    BRANCH,6
        EQUW    WBFR
        EQUW    NUM
        EQUW    QSTAC
        EQUW    BRANCH,-42
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       VOCABULARY
;
;       > A defining word used in the form:
;       >
;       > VOCABULARY CCCC
;       >
;       > It creates a defining word for a vocabulary with name CCCC .
;       > Execution of CCCC makes it the CONTEXT vocabulary in which a
;       > dictionary search will start. Execution of the sequence
;       >
;       > CCCC DEFINITIONS
;       >
;       > will make CCCC the CURRENT vocabulary into which new definitions are
;       > placed. Vocabulary CCCC is so linked that a dictionary search will
;       > also find all words in the vocabulary in which CCCC was originally
;       > defined. All vocabularies, therefore, ultimately link to FORTH .
;       >
;       > By convention all vocabulary defining words are declared IMMEDIATE .
;
;       : VOCABULARY
;        CREATE
;         [ HEX ] A081  ,
;         CURRENT @ CFA ,
;         HERE   VOC-LINK @ ,   VOC-LINK !
;        DOES>
;         2+ CONTEXT !
;       ;
;
; -----------------------------------------------------------------------------

.VOCABULARY_NFA
        DEFWORD "VOCABULARY"
        EQUW    L9329
.VOCABULARY
        EQUW    DOCOLON
        EQUW    CREATE
        EQUW    LIT
        DEFWORD " "
        EQUW    COMMA
        EQUW    CURRENT
        EQUW    FETCH
        EQUW    CFA
        EQUW    COMMA
        EQUW    HERE
        EQUW    VOCLINK
        EQUW    FETCH
        EQUW    COMMA
        EQUW    VOCLINK
        EQUW    STORE
        EQUW    BRACKETSEMICOLONCODE

.DOVOCABULARY
        JSR     DODOES
        EQUW    TWOPLUS
        EQUW    CONTEXT
        EQUW    STORE
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       QUIT
;
;       > Clears the return stack, stops and returns control to the keyboard.
;       > No message is given.
;
;       : QUIT
;        0 BLK !
;        [
;        RP!
;        BEGIN
;         CR
;         QUERY INTERPRET
;         STATE @ 0= IF
;          ." OK"
;         THEN
;        AGAIN
;       ;
;
; -----------------------------------------------------------------------------

.L939D  DEFWORD "QUIT"
        EQUW    LA03F-REL
.QUIT   EQUW    DOCOLON
        EQUW    ZERO
        EQUW    BLK
        EQUW    STORE
        EQUW    LBRAC
        EQUW    RPSTORE
        EQUW    CR
        EQUW    QUERY
        EQUW    INTERPRET
        EQUW    STATE
        EQUW    FETCH
        EQUW    ZEROEQUAL
        EQUW    ZEROBRANCH,7
        EQUW    BRACKETDOTQUOTE
        EQUB    2,"OK"
        EQUW    BRANCH,-25
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       DEFINITIONS
;
;       > Sets the CURRENT vocabulary to the CONTEXT vocabulary. If used in the
;       > form:
;       >
;       > CCCC DEFINITIONS
;       >
;       > where CCCC is a VOCABULARY word, all subsequent definitions will be
;       > placed in the vocabulary CCCC.
;
;       : DEFINITIONS
;        CONTEXT @   CURRENT !
;       ;

; -----------------------------------------------------------------------------

.L93CB  DEFWORD "DEFINITIONS"
        EQUW    L939D

.DEFINITIONS
        EQUW    DOCOLON

        EQUW    CONTEXT
        EQUW    FETCH
        EQUW    CURRENT
        EQUW    STORE
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       (WARM)
;
;       > A routine which returns control to the keyboard interpreter. It is
;       > used by COLD, WARM and the error-handling procedures. The numeric
;       > base is set to decimal and FORTH becomes both the current and context
;       > vocabularies. The return stack (but not the computation stack) is
;       > cleared.
;
;       : (WARM)
;        SP!
;        CR ." OK"
;        DECIMAL
;        FORTH DEFINITIONS
;        QUIT
;       ;
;
; -----------------------------------------------------------------------------

.L93E5  DEFWORD "(WARM)"
        EQUW    L93CB
.PWARM  EQUW    DOCOLON
        EQUW    SPSTORE
        EQUW    CR
        EQUW    BRACKETDOTQUOTE
        EQUB    2,"OK"
        EQUW    DECIM
        EQUW    FORTH
        EQUW    DEFINITIONS
        EQUW    QUIT
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       (ABORT)
;
;       > Clears the data and return stacks and sets execution mode. Control is
;       > returned to the keyboard interpreter. See ABORT .
;
;       : (ABORT)
;        SP!
;        CR CR ." FORTH"
;        (WARM)
;       ;
;
; -----------------------------------------------------------------------------

.L9403  DEFWORD "(ABORT)"
        EQUW    L93E5
.BRACKETABORT
        EQUW    DOCOLON
        EQUW    SPSTORE
        EQUW    CR
        EQUW    CR
        EQUW    BRACKETDOTQUOTE
        EQUB    5,"FORTH"
        EQUW    PWARM
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       ESCAPE
;
;       > Prints the message 'Escape' and re-enters the system via QUIT. This
;       > is the routine executed when the ESCAPE key is pressed.
;
;       : ESCAPE
;        SP!
;        CR ." Escape"
;        QUIT
;       ;
;
; -----------------------------------------------------------------------------

.L9421  DEFWORD "ESCAPE"
        EQUW    LA04F-REL
.ESCAPE
        EQUW    DOCOLON
        EQUW    SPSTORE
        EQUW    CR
        EQUW    BRACKETDOTQUOTE
        EQUB    6,"Escape"
        EQUW    QUIT
        EQUW    EXIT

;-----------------------------------------------------------------------------
;
;       OSERROR
;
;       > The routine executed when an operating
;       > system error is detected. The error message number is
;       > given in decimal base and the relevant operating system
;       > error message is displayed. Controls is returned to the
;       > keyboard via (WARM).
;
;       : OSERROR
;        SP!
;        CR ." O.S.Error "   $00FD @   DUP C@ DEC.
;        0 CLOSE
;        SPACE   BEGIN
;                 1+
;                 DUP C@
;                 DUP
;                WHILE
;                 EMIT
;                REPEAT
;        2DROP
;        (WARM)
;       ;
;
; -----------------------------------------------------------------------------

.L943D  DEFWORD "OSERROR"
        EQUW    L9421
.OSERR  EQUW    DOCOLON
        EQUW    SPSTORE
        EQUW    CR
        EQUW    BRACKETDOTQUOTE
        EQUB    10,"O.S.Error "
        EQUW    LIT,$FD
        EQUW    FETCH
        EQUW    DUP
        EQUW    CFETCH
        EQUW    DECDOT
        EQUW    ZERO
        EQUW    CLOSE
        EQUW    SPACE
        EQUW    ONEPLUS
        EQUW    DUP
        EQUW    CFETCH
        EQUW    DUP
        EQUW    ZEROBRANCH,8
        EQUW    EMIT
        EQUW    BRANCH,-16
        EQUW    TWODROP
        EQUW    PWARM
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       MODE   ( n ... )
;
;       > Sets the VDU display to mode n. In the tape or disk versions, only
;       > modes 4-7 inclusive are allowed.
;
;       : MODE
;        HIADDR -1 = IF   ( check if we are on I/O processor )
;         DUP MODEADDR
;         MOVE-BUFFERS
;        THEN
;        22 >VDU >VDU
;       ;
;
; -----------------------------------------------------------------------------

.MODE_NFA
        DEFWORD "MODE"
        EQUW    L943D
.MODE   EQUW    DOCOLON
        EQUW    HIADDR
        EQUW    MINUSONE
        EQUW    EQUAL
        EQUW    ZEROBRANCH,8
        EQUW    DUP
        EQUW    MODEADDR
        EQUW    MOVBUF
        EQUW    LIT,$16
        EQUW    TOVDU
        EQUW    TOVDU
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       START
;
;       > The high-level entry point to FORTH on a cold start. The computation
;       > and return stacks are cleared. Any applications dictionary is
;       > discarded and all vectored words are initialised to their default
;       > values. The mass storage buffers are initialised to the number of
;       > buffers given by MINBUF and marked as being empty; OFFSET is set to
;       > zero. User-defined keys 8 and 9 are programmed for the correct WARM
;       > and COLD entry points respectively and printer output is disabled.
;       > Control is passed to the keyboard interpreter via (ABORT) .
;
;       : START
;        SP!
;        INITVECS   INITBUF   0 OFFSET !
;        $1E +ORIGIN @ PRUNE
;        3 >VDU
;        PAGE 2+ DUP   DP !   FENCE !
;        (ABORT)
;       ;
;
; -----------------------------------------------------------------------------

.START_NFA
        DEFWORD "START"
        EQUW    MODE_NFA
.START  EQUW    DOCOLON
        EQUW    SPSTORE

IF RELOCATE

        EQUW    LIT,REL_SRC
        EQUW    LIT,REL_DST
        EQUW    LIT,REL_SZ
        EQUW    CMOVE
ENDIF
        EQUW    INIVEC
        EQUW    INIBUF

        EQUW    ZERO
        EQUW    OFFSET
        EQUW    STORE

        EQUW    LIT,$1E
        EQUW    PLUSORIGIN
        EQUW    FETCH
        EQUW    PRUNE

        EQUW    LIT,3
        EQUW    TOVDU

        EQUW    PAGE
        EQUW    TWOPLUS
        EQUW    DUP

        EQUW    DP
        EQUW    STORE

        EQUW    FENCE
        EQUW    STORE

        EQUW    BRACKETABORT
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       Handler for the user pressing the escape key during execution. Called
;       from NEXT when it detects the operating system has set the escape flag.
;
; -----------------------------------------------------------------------------

.EscapeHandler
        LDA     #ClearEscapeCondition   ; Tell the OS the escape press has been
        JSR     OSBYTE                  ; dealt with.

        LDA     PtrToEscapePFA+1        ; Set IP to point to the PFA of the 
        STA     IP+1                    ; ESCAPE word, so that will be executed
        LDA     PtrToEscapePFA          ; next.
        STA     IP

        LDY     #$0F                    ; Reset the first eight user variables
        JMP     Restart                 ; and restart Forth.

; -----------------------------------------------------------------------------
;
;       BRK HANDLER
;
; -----------------------------------------------------------------------------

.BrkHandler
        LDA     PtrToOSErrorPFA+1
        STA     IP+1
        LDA     PtrToOSErrorPFA
        STA     IP

        LDY     #$0F
        JMP     Restart

; -----------------------------------------------------------------------------
;
;       COLD
;
;       > The cold start procedure used on first entry to the system. The
;       > dictionary pointer and user variables are initialised from the
;       > boot-up parameters and the system re-started via (ABORT). The mass
;       > storage buffers are cleared, function keys 8 and 9 are nitialised,
;       > and printer output is disabled. All vectored words are set to their
;       > default actions. It may be called from the keyboard to remove all
;       > application programs and restart with the nucleus dictionary alone.
;
; -----------------------------------------------------------------------------

.COLD_NFA
        DEFWORD "COLD"
        EQUW    START_NFA
.COLD   EQUW    *+2

        LDA     PtrToBrkHandler+1
        STA     BRKV+1
        LDA     PtrToBrkHandler
        STA     BRKV

        LDA     PtrToStartPFA+1
        STA     IP+1
        LDA     PtrToStartPFA
        STA     IP

        LDY     #$15
        JMP     Restart

; -----------------------------------------------------------------------------
;
;       WARM
;
;       > Performs a warm start. The stacks are cleared. The CURRENT and
;       > CONTEXT vocabularies are set to FORTH, and DECIMAL numeric base is
;       > selected. No other initialisation takes place. In particular the
;       > user's dictionary and the contents of the buffer are preserved. All
;       > vectored routines maintain their current assignments.
;
; -----------------------------------------------------------------------------

.L9534  DEFWORD "WARM"
        EQUW    COLD_NFA
.WARM   EQUW    *+2

        LDA     PtrToBracketAbortPFA+1
        STA     IP+1
        LDA     PtrToBracketAbortPFA
        STA     IP

        LDA     PtrToBrkHandler+1
        STA     BRKV+1
        LDA     PtrToBrkHandler
        STA     BRKV

        LDY     #$0F
        JMP     Restart

;       S->D

.L9558  DEFWORD "S->D"
        EQUW    L9534
.STOD   EQUW    *+2
        LDA     1,X
        BPL     L9566
        DEY
.L9566  TYA
        PHA
        JMP     PUSH

;       +-

.L956B  DEFWORD "+-"
        EQUW    L9558
.PM     EQUW    DOCOLON
        EQUW    ZEROLESS
        EQUW    ZEROBRANCH,4
        EQUW    NEGATE
        EQUW    EXIT

;       D+-

.L957C  DEFWORD "D+-"
        EQUW    L956B
.DPM    EQUW    DOCOLON
        EQUW    ZEROLESS
        EQUW    ZEROBRANCH,4
        EQUW    DNEGATE
        EQUW    EXIT

;       ABS

.L958E  DEFWORD "ABS"
        EQUW    L957C
.ABS    EQUW    DOCOLON
        EQUW    DUP
        EQUW    PM
        EQUW    EXIT

;       DABS

.L959C  DEFWORD "DABS"
        EQUW    L958E
.DABS   EQUW    DOCOLON
        EQUW    DUP
        EQUW    DPM
        EQUW    EXIT

;       MIN

.L95AB  DEFWORD "MIN"
        EQUW    L959C
.MIN    EQUW    DOCOLON
        EQUW    TWODUP
        EQUW    GREATERTHAN
        EQUW    ZEROBRANCH,4
        EQUW    SWAP
        EQUW    DROP
        EQUW    EXIT

;       MAX

.L95C1  DEFWORD "MAX"
        EQUW    L95AB
.MAX    EQUW    DOCOLON
        EQUW    TWODUP
        EQUW    LESS
        EQUW    ZEROBRANCH,4
        EQUW    SWAP
        EQUW    DROP
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       U/MOD   ( ud\u1 ... u2\u3 )
;
;       > The action is similar to that of U/ except that an error message is
;       > given if division by zero is attempted. All other division words use
;       > U/MOD as their basis and are therefore protected against division by
;       > zero.
;
;       : U/MOD
;        ?DUP IF
;         U/
;        ELSE
;         11 ERROR
;        THEN
;       ;
;
; -----------------------------------------------------------------------------

.L95D7  DEFWORD "U/MOD"
        EQUW    L95C1
.USLASHMOD
        EQUW    DOCOLON
        EQUW    QUERYDUP
        EQUW    ZEROBRANCH,8
        EQUW    USLASH
        EQUW    BRANCH,8
        EQUW    LIT,11
        EQUW    ERROR
        EQUW    EXIT

;       M*

.L95F5  DEFWORD "M*"
        EQUW    L95D7
.MSTAR  EQUW    DOCOLON
        EQUW    TWODUP
        EQUW    XOR
        EQUW    TOR
        EQUW    ABS
        EQUW    SWAP
        EQUW    ABS
        EQUW    USTAR
        EQUW    RFROM
        EQUW    DPM
        EQUW    EXIT

;       M/

.L9610  DEFWORD "M/"
        EQUW    L95F5
.MSLASH
        EQUW    DOCOLON
        EQUW    OVER
        EQUW    TOR
        EQUW    TOR
        EQUW    DABS
        EQUW    RFETCH
        EQUW    ABS
        EQUW    USLASHMOD
        EQUW    RFROM
        EQUW    RFETCH
        EQUW    XOR
        EQUW    PM
        EQUW    SWAP
        EQUW    RFROM
        EQUW    PM
        EQUW    SWAP
        EQUW    EXIT

;       *

.L9637  DEFWORD "*"
        EQUW    L9610
.STAR   EQUW    DOCOLON
        EQUW    USTAR
        EQUW    DROP
        EQUW    EXIT

;       /MOD

.L9643  DEFWORD "/MOD"
        EQUW    L9637
.SLASHMOD
        EQUW    DOCOLON
        EQUW    TOR
        EQUW    STOD
        EQUW    RFROM
        EQUW    MSLASH
        EQUW    EXIT

;       /

.L9656  DEFWORD "/"
        EQUW    L9643
.SLASH  EQUW    DOCOLON
        EQUW    SLASHMOD
        EQUW    SWAP
        EQUW    DROP
        EQUW    EXIT

;       MOD

.L9664  DEFWORD "MOD"
        EQUW    L9656
.MOD    EQUW    DOCOLON
        EQUW    SLASHMOD
        EQUW    DROP
        EQUW    EXIT

;       */MOD

.L9672  DEFWORD "*/MOD"
        EQUW    L9664
.STARSLASHMOD
        EQUW    DOCOLON
        EQUW    TOR
        EQUW    MSTAR
        EQUW    RFROM
        EQUW    MSLASH
        EQUW    EXIT

;       */

.L9686  DEFWORD "*/"
        EQUW    L9672
.STARSLASH
        EQUW    DOCOLON
        EQUW    STARSLASHMOD
        EQUW    SWAP
        EQUW    DROP
        EQUW    EXIT

;       M/MOD

.L9695  DEFWORD "M/MOD"
        EQUW    L9686
.MSLASHMOD
        EQUW    DOCOLON
        EQUW    TOR
        EQUW    ZERO
        EQUW    RFETCH
        EQUW    USLASHMOD
        EQUW    RFROM
        EQUW    SWAP
        EQUW    TOR
        EQUW    USLASHMOD
        EQUW    RFROM
        EQUW    EXIT

;       SPACES

.SPACES_NFA
        DEFWORD "SPACES"
        EQUW    L9695
.SPACES EQUW    DOCOLON
        EQUW    ZERO
        EQUW    MAX
        EQUW    QUERYDUP
        EQUW    ZEROBRANCH,$C
        EQUW    ZERO
        EQUW    XDO
        EQUW    SPACE
        EQUW    BRACKETULOOP,$FFFC
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       <#
;
;       > Sets up for numeric output formatting. The conversion is performed
;       > on a double number to produce text at PAD . See also #, #>, #S ,
;       > SIGN .
;
; -----------------------------------------------------------------------------

.LESSSHARP_NFA
        DEFWORD "<#"
        EQUW    SPACES_NFA
.LESSSHARP
        EQUW    DOCOLON
        EQUW    PAD
        EQUW    HLD
        EQUW    STORE
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       #>   ( nd ... addr\count )
;
;       > Terminates numeric output conversion by dropping the double number nd
;       > and leaving the address and the character count of the converted
;       > string in a form suitable for TYPE .
;
;       : #>
;        2DROP
;        HLD @   PAD   OVER - 
;       ;
;
; -----------------------------------------------------------------------------

.SHARPGREATER_NFA
        DEFWORD "#>"
        EQUW    LESSSHARP_NFA
.SHARPGREATER
        EQUW    DOCOLON
        EQUW    TWODROP
        EQUW    HLD
        EQUW    FETCH
        EQUW    PAD
        EQUW    OVER
        EQUW    MINUS
        EQUW    EXIT

        EQUB    $66             ; Unused.

;       SIGN

.SIGN_NFA
        DEFWORD "SIGN"
        EQUW    SHARPGREATER_NFA
.SIGN   EQUW    DOCOLON
        EQUW    ROT
        EQUW    ZEROLESS
        EQUW    ZEROBRANCH,8
        EQUW    LIT,'-'
        EQUW    HOLD
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       #   ( nd1 ... nd2 )
;
;       > Converts the least-significant digit (in the current base) of the
;       > double-precision number nd1 to the corresponding ASCII character,
;       > which it then stores at PAD . The remaining part of the number is
;       > left as nd2 for further conversions. # is used between <# and #> .
;
;       : #
;        BASE @   M/MOD   ROT
;        9 OVER   < IF
;         7 +
;        THEN
;        [ CHAR 0 ] +
;        HOLD
;       ;
;
; -----------------------------------------------------------------------------

.SHARP_NFA
        DEFWORD "#"
        EQUW    SIGN_NFA
.SHARP  EQUW    DOCOLON
        EQUW    BASE
        EQUW    FETCH
        EQUW    MSLASHMOD
        EQUW    ROT
        EQUW    LIT,9
        EQUW    OVER
        EQUW    LESS
        EQUW    ZEROBRANCH,8
        EQUW    LIT,7
        EQUW    PLUS
        EQUW    LIT,'0'
        EQUW    PLUS
        EQUW    HOLD
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       #S   ( n1 ... n2 )
;
;       > Converts the double-precision number nd1 into ASCII text by repeated
;       > use of # , and stores the text at PAD . The double-precision number
;       > nd2 is left on the stack, and has the value zero. #S is used  between
;       > <# and #> .
;
;       : #S
;        BEGIN
;         #
;         2DUP OR
;         0=
;        UNTIL
;       ;
;
; -----------------------------------------------------------------------------

.SHARPS_NFA
        DEFWORD "#S"
        EQUW    SHARP_NFA
.SHARPS EQUW    DOCOLON
        EQUW    SHARP
        EQUW    TWODUP
        EQUW    OR
        EQUW    ZEROEQUAL
        EQUW    ZEROBRANCH,-10
        EQUW    EXIT

;       D.R

.L9751  DEFWORD "D.R"
        EQUW    SHARPS_NFA
.DDOTR  EQUW    DOCOLON
        EQUW    TOR
        EQUW    SWAP
        EQUW    OVER
        EQUW    DABS
        EQUW    LESSSHARP
        EQUW    SHARPS
        EQUW    SIGN
        EQUW    SHARPGREATER
        EQUW    RFROM
        EQUW    OVER
        EQUW    MINUS
        EQUW    SPACES
        EQUW    TYPE
        EQUW    EXIT

;       D.

.L9775  DEFWORD "D."
        EQUW    L9751
.DDOT   EQUW    DOCOLON
        EQUW    ZERO
        EQUW    DDOTR
        EQUW    SPACE
        EQUW    EXIT

;       .R

.L9784  DEFWORD ".R"
        EQUW    L9775
.DOTR   EQUW    DOCOLON
        EQUW    TOR
        EQUW    STOD
        EQUW    RFROM
        EQUW    DDOTR
        EQUW    EXIT

;       .

.L9795  DEFWORD "."
        EQUW    L9784
.DOT    EQUW    DOCOLON
        EQUW    STOD
        EQUW    DDOT
        EQUW    EXIT

;       U.

.L97A1  DEFWORD "U."
        EQUW    L9795
.UDOT   EQUW    DOCOLON
        EQUW    ZERO
        EQUW    DDOT
        EQUW    EXIT

;       ?

.L97AE  DEFWORD "?"
        EQUW    L97A1
.QUES   EQUW    DOCOLON
        EQUW    FETCH
        EQUW    DOT
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       DEC.   ( n ... )
;
;       > Displays n in DECIMAL base, using the format of <.>, regardless of
;       > the current value of BASE .
;
;       : DEC.
;        BASE @  SWAP
;        DECIMAL .
;        BASE !
;       ;
;
; -----------------------------------------------------------------------------

.L97BA  DEFWORD "DEC."
        EQUW    L97AE
.DECDOT EQUW    DOCOLON
        EQUW    BASE
        EQUW    FETCH
        EQUW    SWAP
        EQUW    DECIM
        EQUW    DOT
        EQUW    BASE
        EQUW    STORE
        EQUW    EXIT

;       H.

.HDOT_NFA
        DEFWORD "H."
        EQUW    L97BA
.HDOT   EQUW    DOCOLON
        EQUW    BASE
        EQUW    FETCH
        EQUW    SWAP
        EQUW    HEX
        EQUW    DOT
        EQUW    BASE
        EQUW    STORE
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       MSG#   ( n ... )
;
;       > See MESSAGE .
;
;       : MSG#
;        ?DUP IF
;         (.") [ 6 C, CHAR M C, CHAR S C, CHAR G C, 32 C, CHAR # C, 32 C, ]
;         DEC.
;        THEN
;       ;
;
; -----------------------------------------------------------------------------

.MSGNUM_NFA
        DEFWORD "MSG#"
        EQUW    HDOT_NFA
.MSGNUM EQUW    DOCOLON
        EQUW    QUERYDUP
        EQUW    ZEROBRANCH,$D
        EQUW    BRACKETDOTQUOTE
        EQUB    6,"MSG # "
        EQUW    DECDOT
        EQUW    EXIT

;       2*

.TSTAR_NFA
        DEFWORD "2*"
        EQUW    LA05B-REL
.TSTAR  EQUW    *+2
        ASL     0,X
        ROL     1,X
        JMP     NEXT

;       2/

.TSLAS_NFA
        DEFWORD "2/"
        EQUW    TSTAR_NFA
.TSLAS  EQUW    *+2
        CLC
        LDA     1,X
        BPL     L9829
        INC     0,X
        BNE     L9826
        INC     1,X
.L9826  BEQ     L9829
        SEC
.L9829  ROR     1,X
        ROR     0,X
        JMP     NEXT

;       J

.JDO_NFA
        DEFWORD "J"
        EQUW    TSLAS_NFA
.JDO    EQUW    DOCOLON
        EQUW    RPFETCH
        EQUW    LIT,7
        EQUW    PLUS
        EQUW    FETCH
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       BACK   ( addr ... )
;
;       > Calculates the backward branch offset from HERE to addr and compiles
;       > into the next available dictionary memory address. Used in the
;       > compilation of conditionals ( AGAIN, UNTIL etc).
;
;       : BACK
;        HERE - ,
;       ;
;
; -----------------------------------------------------------------------------

.BACK_NFA
        DEFWORD "BACK"
        EQUW    JDO_NFA
.BACK   EQUW    DOCOLON
        EQUW    HERE
        EQUW    MINUS
        EQUW    COMMA
        EQUW    EXIT

;       DO

.DO_NFA DEFIMM  "DO"
        EQUW    BACK_NFA
.DO     EQUW    DOCOLON
        EQUW    COMPILE
        EQUW    XDO
        EQUW    HERE
        EQUW    LIT,3
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       LOOP
;
;       > Used in a colon-definition in the form:
;       >
;       > DO ... LOOP
;       >
;       > During execution LOOP controls branching back to the corresponding
;       > DO , dependent on the loop index and loop limit. The loop index is
;       > incremented by one and tested against the loop limit. Branching to DO
;       > continues until the index is equal to or greater than the limit when
;       > execution continues with the word following LOOP .
;
;       : LOOP
;        3 ?PAIRS
;        COMPILE (LOOP)
;        BACK
;       ; IMMEDIATE
;
; -----------------------------------------------------------------------------

.LOOP_NFA
        DEFIMM  "LOOP"
        EQUW    DO_NFA
.LOOP   EQUW    DOCOLON
        EQUW    LIT,3
        EQUW    QUERYPAIRS
        EQUW    COMPILE
        EQUW    BRACKETLOOP
        EQUW    BACK
        EQUW    EXIT

;       +LOOP

.PLOOP_NFA
        DEFIMM  "+LOOP"
        EQUW    LOOP_NFA
.PLOOP  EQUW    DOCOLON
        EQUW    LIT,3
        EQUW    QUERYPAIRS
        EQUW    COMPILE
        EQUW    BRACKETPLUSLOOP
        EQUW    BACK
        EQUW    EXIT

;       IF

.IF_NFA DEFIMM  "IF"
        EQUW    PLOOP_NFA
.IF     EQUW    DOCOLON
        EQUW    COMPILE
        EQUW    ZEROBRANCH
        EQUW    HERE
        EQUW    ZERO
        EQUW    COMMA
        EQUW    TWO
        EQUW    EXIT

;       THEN

.THEN_NFA
        DEFIMM  "THEN"
        EQUW    IF_NFA
.THEN   EQUW    DOCOLON
        EQUW    QUERYCOMP
        EQUW    TWO
        EQUW    QUERYPAIRS
        EQUW    HERE
        EQUW    OVER
        EQUW    MINUS
        EQUW    SWAP
        EQUW    STORE
        EQUW    EXIT

;       ELSE

.ELSE_NFA
        DEFIMM  "ELSE"
        EQUW    THEN_NFA
.ELSE   EQUW    DOCOLON
        EQUW    TWO
        EQUW    QUERYPAIRS
        EQUW    COMPILE
        EQUW    BRANCH
        EQUW    HERE
        EQUW    ZERO
        EQUW    COMMA
        EQUW    SWAP
        EQUW    TWO
        EQUW    THEN
        EQUW    TWO
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       BEGIN   ( ... addr 1 )
;
;       > Used in a colon definition in the forms:
;       >
;       > BEGIN ... AGAIN
;       > BEGIN ... UNTIL
;       > BEGIN ... WHILE ... REPEAT
;       >
;       > BEGIN marks the start of a sequence that may be executed repeatedly.
;       > It acts as a return point from the corresponding AGAIN , UNTIL or
;       > REPEAT .
;
;       : BEGIN
;        ?COMP
;        HERE 1
;       ;
;       IMMEDIATE
;
; -----------------------------------------------------------------------------

.BEGIN_NFA
        DEFIMM  "BEGIN"
        EQUW    ELSE_NFA
.BEGIN  EQUW    DOCOLON
        EQUW    QUERYCOMP
        EQUW    HERE
        EQUW    ONE
        EQUW    EXIT

;       UNTIL

.UNTIL_NFA
        DEFIMM  "UNTIL"
        EQUW    BEGIN_NFA
.UNTIL  EQUW    DOCOLON
        EQUW    ONE
        EQUW    QUERYPAIRS
        EQUW    COMPILE
        EQUW    ZEROBRANCH
        EQUW    BACK
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       AGAIN
;
;       > Used in a colon-definition in the form:
;       >
;       > BEGIN ... AGAIN
;       >
;       > During the execution of a word containing this sequence, AGAIN
;       > forces a branch back to the corresponding BEGIN to create an endless
;       > loop.
;
;       : AGAIN
;        1 ?PAIRS
;        ?COMP
;        BRANCH BACK
;       ;
;       IMMEDIATE
;
; -----------------------------------------------------------------------------

.AGAIN_NFA
        DEFIMM  "AGAIN"
        EQUW    UNTIL_NFA
.AGAIN  EQUW    DOCOLON
        EQUW    ONE
        EQUW    QUERYPAIRS
        EQUW    COMPILE
        EQUW    BRANCH
        EQUW    BACK
        EQUW    EXIT

;       WHILE

.WHILE_NFA
        DEFIMM  "WHILE"
        EQUW    AGAIN_NFA
.WHILE  EQUW    DOCOLON
        EQUW    IF
        EQUW    TWOPLUS
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       REPEAT
;
;       > Used in a colon-definition in the form:
;       >
;       > BEGIN ... WHILE ... REPEAT
;       >
;       > In execution REPEAT forces an unconditional branch back to BEGIN .
;
;       : REPEAT
;        >R >R   AGAIN   R> R>
;        2 -   THEN
;       ;
;       IMMEDIATE
;
; -----------------------------------------------------------------------------

.REPEAT_NFA
        DEFIMM  "REPEAT"
        EQUW    WHILE_NFA
.REPEAT EQUW    DOCOLON
        EQUW    TOR
        EQUW    TOR
        EQUW    AGAIN
        EQUW    RFROM
        EQUW    RFROM
        EQUW    TWO
        EQUW    MINUS
        EQUW    THEN
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       '   ( ... addr )
;
;       > Used in the form ' nnnn and leaves the parameter field address of
;       > dictionary word nnnn if in execution mode.
;       >
;       > If used within a colon-definition it will execute to compile the
;       > address as a literal numerical value (preceded by the address of the
;       > literal handler routine, LIT) in the defintion.
;
;       : '
;        FIND
;        DUP 0=   0 ?ERROR
;        2+ LITERAL
;       ; IMMEDIATE
;
; -----------------------------------------------------------------------------

.TICK_NFA
        DEFIMM  "'"
        EQUW    REPEAT_NFA
.TICK   EQUW    DOCOLON
        EQUW    FIND
        EQUW    DUP
        EQUW    ZEROEQUAL
        EQUW    ZERO
        EQUW    QUERYERROR
        EQUW    TWOPLUS
        EQUW    LITERAL
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       VLIST
;
;       > Display, on the output device, a list of the names of all words in
;       > the CONTEXT vocabulary and any other vocabulary to which the CONTEXT
;       > vocabulary is chained. All VLISTs will therefore include a listing of
;       > the words in the FORTH vocabulary. The listing can be interrupted by
;       > pressing the Tab key and resumed by pressing the Space Bar. If, after
;       > interruption, any key except the Space Bar is pressed, the listing
;       > will be aborted.
;
;       : VLIST
;        128 OUT !
;        CONTEXT @ @
;        BEGIN
;         OUT @ C/L > IF
;          CR
;          0 OUT !
;         THEN
;         DUP ID. SPACE SPACE
;         PFA LFA @ DUP
;         ?TAB IF
;          BEGIN  ?TAB NOT  UNTIL
;          KEY BL = IF -1 ELSE 0 THEN
;          AND
;         THEN
;         0=
;        UNTIL
;        DROP
;       ;
;
;      ??? Why the check on whether more than C/L (typically 64) characters
;      have been printed, after which a CR is output? Quite odd, as in any
;      80-column mode this results in not using all available space, and in the
;      other 20- and 40-column modes it introduces line breaks at unnecessary
;      places.
;
;      ??? Why the pausing when tab is pressed and unpausing when the space bar
;      is pressed? The Acorn MOS uses ^N throughout to enable pagination and ^O
;      to disable it (VDU 14 and 15), which most users would be familiar with.
;
; -----------------------------------------------------------------------------

.VLIST_NFA
        DEFWORD "VLIST"
        EQUW    TICK_NFA
.VLIST  EQUW    DOCOLON
        EQUW    LIT,128
        EQUW    OUT
        EQUW    STORE
        EQUW    CONTEXT
        EQUW    FETCH
        EQUW    FETCH
        EQUW    OUT
        EQUW    FETCH
        EQUW    CSLASHL
        EQUW    GREATERTHAN
        EQUW    ZEROBRANCH,$A
        EQUW    CR
        EQUW    ZERO
        EQUW    OUT
        EQUW    STORE
        EQUW    DUP
        EQUW    IDDOT
        EQUW    SPACE
        EQUW    SPACE
        EQUW    PFA
        EQUW    LFA
        EQUW    FETCH
        EQUW    DUP
        EQUW    QTAB
        EQUW    ZEROBRANCH,$1E
        EQUW    QTAB
        EQUW    NOT
        EQUW    ZEROBRANCH,-6
        EQUW    KEY
        EQUW    BL
        EQUW    EQUAL
        EQUW    ZEROBRANCH,8
        EQUW    MINUSONE
        EQUW    BRANCH,4
        EQUW    ZERO
        EQUW    AND
        EQUW    ZEROEQUAL
        EQUW    ZEROBRANCH,-74
        EQUW    DROP
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       NOVEC
;
;       > The action assigned to a newly-created execution vector. See
;       > EXCVEC: .
;
;       : NOVEC
;        12 ERROR
;       ;
;       
; -----------------------------------------------------------------------------

.NOVEC_NFA
        DEFWORD "NOVEC"
        EQUW    VLIST_NFA
.NOVEC  EQUW    DOCOLON
        EQUW    LIT,12
        EQUW    ERROR
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       EXVEC:
;
;       > Used in the form:
;       >
;       > EXVEC: NNNN
;       >
;       > It creates an execution-vectored word NNNN , initially assigned to
;       > execute NOVEC which gives an error message. The action of NNNN should
;       > then be assigned to execute some other word CCCC by the use of
;       >
;       > ASSIGN NNNN TO-DO CCCC
;       >
;       > The action of NNNN may be reassigned at any time, when all previously
;       > compiled uses of NNNN will be changed to the new assignment.
;
;       : EXVEC:
;        CREATE
;         [ FIND NOVEC ] LITERAL
;        DOES>
;         @EXECUTE
;       ;
;
; -----------------------------------------------------------------------------

.L99E1  DEFWORD "EXVEC:"
        EQUW    NOVEC_NFA
.EXVEC  EQUW    DOCOLON
        EQUW    CREATE
        EQUW    LIT,NOVEC
        EQUW    COMMA
        EQUW    BRACKETSEMICOLONCODE
.DOEXVEC
        JSR     DODOES
        EQUW    FETCHEXECUTE
        EQUW    EXIT

;       ASSIGN

.L99FD  DEFIMM  "ASSIGN"
        EQUW    L99E1
.ASSIGN EQUW    DOCOLON
        EQUW    TICK
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       DOVEC   ( addr\pfa ... )
;
;       > Converts the parameter field address pfa to its code field
;       > (execution) address and stores the result at the address addr. Used
;       > in the reassignment of execution vectors.
;
;       : DOVEC
;        CFA SWAP !
;       ;
; -----------------------------------------------------------------------------

.L9A0C  DEFWORD "DOVEC"
        EQUW    L99FD
.DOVEC  EQUW    DOCOLON
        EQUW    CFA
        EQUW    SWAP
        EQUW    STORE
        EQUW    EXIT

;       TO-DO

.L9A1E  DEFIMM  "TO-DO"
        EQUW    L9A0C
.TODO   EQUW    DOCOLON
        EQUW    TICK
        EQUW    STATE
        EQUW    FETCH
        EQUW    ZEROBRANCH,4
        EQUW    COMPILE
        EQUW    DOVEC
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       INITVECS
;
;       > Initialises all vectored words in the nucleus dictionary to their
;       > default assignments.
;
;       : INITVECS
;        ?R/W
;        [ FIND (UPDATE) ] LITERAL   [ ' UPDATE  ] LITERAL   !
;        [ FIND (EMIT)   ] LITERAL   [ ' EMIT    ] LITERAL   !
;        [ FIND (KEY)    ] LITERAL   [ ' KEY     ] LITERAL   !
;        [ FIND (CREATE) ] LITERAL   [ ' CREATE  ] LITERAL   !
;        [ FIND (NUM)    ] LITERAL   [ ' NUM     ] LITERAL   !
;        [ FIND (ABORT)  ] LITERAL   [ ' ABORT   ] LITERAL   !
;        [ FIND $MSG     ] LITERAL   [ ' MESSAGE ] LITERAL   !
;       ;
;
; -----------------------------------------------------------------------------

.L9A38  DEFWORD "INITVECS"
        EQUW    L9A1E
.INIVEC EQUW    DOCOLON
        EQUW    QUERYRSLASHW

        EQUW    LIT,BRACKETUPDATE
        EQUW    LIT,UPDATE+2
        EQUW    STORE

        EQUW    LIT,BRACKETEMIT
        EQUW    LIT,EMIT+2
        EQUW    STORE
        
        EQUW    LIT,BRACKETKEY
        EQUW    LIT,KEY+2
        EQUW    STORE
        
        EQUW    LIT,BRACKETCREATE
        EQUW    LIT,CREATE+2
        EQUW    STORE
        
        EQUW    LIT,BRACKETNUM
        EQUW    LIT,NUM+2
        EQUW    STORE
        
        EQUW    LIT,BRACKETABORT
        EQUW    LIT,ABORT+2
        EQUW    STORE
        
        EQUW    LIT,DOLLARMSG
        EQUW    LIT,MESSAGE+2
        EQUW    STORE
        
        EQUW    EXIT

;       PRUNE

.L9A8F  DEFWORD "PRUNE"
        EQUW    L9A38
.PRUNE  EQUW    DOCOLON
        EQUW    VOCLINK
        EQUW    FETCH
        EQUW    DUP
        EQUW    ZEROBRANCH,$5A
        EQUW    DUP
        EQUW    TWOMINUS
        EQUW    CURRENT
        EQUW    STORE
        EQUW    SWAP
        EQUW    DUP
        EQUW    ONEMINUS
        EQUW    TOR
        EQUW    LAST
        EQUW    RFETCH
        EQUW    OVER
        EQUW    ULESS
        EQUW    OVER
        EQUW    LIT,$8000
        EQUW    ULESS
        EQUW    AND
        EQUW    ZEROBRANCH,$C
        EQUW    PFA
        EQUW    LFA
        EQUW    FETCH
        EQUW    BRANCH,-28
        EQUW    CURRENT
        EQUW    FETCH
        EQUW    STORE
        EQUW    RFROM
        EQUW    DROP
        EQUW    OVER
        EQUW    FETCH
        EQUW    SWAP
        EQUW    ROT
        EQUW    OVER
        EQUW    SWAP
        EQUW    ULESS
        EQUW    ZEROBRANCH,8
        EQUW    OVER
        EQUW    VOCLINK
        EQUW    STORE
        EQUW    SWAP
        EQUW    BRANCH,-92
        EQUW    TWODROP
        EQUW    FORTH
        EQUW    DEFINITIONS
        EQUW    EXIT

;       FORGET

.L9B03  DEFWORD "FORGET"
        EQUW    L9A8F
.FORG   EQUW    DOCOLON
        EQUW    CURRENT
        EQUW    FETCH
        EQUW    FETCH
        EQUW    DFIND
        EQUW    ZEROEQUAL
        EQUW    LIT,24
        EQUW    QUERYERROR
        EQUW    DROP
        EQUW    TWOPLUS
        EQUW    NFA
        EQUW    ZERO
        EQUW    PLUSORIGIN
        EQUW    OVER
        EQUW    ULESS
        EQUW    OVER
        EQUW    FENCE
        EQUW    FETCH
        EQUW    ULESS
        EQUW    OR
        EQUW    LIT,21
        EQUW    QUERYERROR
        EQUW    DUP
        EQUW    DP
        EQUW    STORE
        EQUW    PRUNE
        EQUW    EXIT

;       DEPTH

.L9B46  DEFWORD "DEPTH"
        EQUW    L9B03
.DEPTH  EQUW    DOCOLON
        EQUW    SPFETCH
        EQUW    SZERO
        EQUW    FETCH
        EQUW    MINUS
        EQUW    NEGATE
        EQUW    TSLAS
        EQUW    EXIT

;       .S

.L9B5E  DEFWORD ".S"
        EQUW    L9B46
.DOTS   EQUW    DOCOLON
        EQUW    CR
        EQUW    DEPTH
        EQUW    ZEROBRANCH,$1A
        EQUW    SPFETCH
        EQUW    SZERO
        EQUW    FETCH
        EQUW    TWOMINUS
        EQUW    XDO
        EQUW    IDO
        EQUW    QUES
        EQUW    MINUSTWO
        EQUW    BRACKETPLUSLOOP,$FFF8
        EQUW    BRANCH,$B
        EQUW    BRACKETDOTQUOTE
        EQUB    6,"EMPTY "
        EQUW    EXIT

;       PICK

.L9B90  DEFWORD "PICK"
        EQUW    L9B5E
.PICK   EQUW    *+2
        SEC
        TYA
        SBC     0,X
        TYA
        SBC     1,X
        BPL     L9BB6
        ASL     0,X
        ROL     1,X
        CLC
        TXA
        ADC     0,X
        TAY
        LDA     0,Y
        PHA
        INY
        LDA     0,Y
        JMP     PUT

.L9BB6  JMP     POP

;       ROLL

.L9BB9  DEFWORD "ROLL"
        EQUW    L9B90
.ROLL   EQUW    *+2
        SEC
        TYA
        SBC     0,X
        TYA
        SBC     1,X
        BPL     L9BF3
        ASL     0,X
        ROL     1,X
        CLC
        TXA
        ADC     0,X
        TAY
        LDA     0,Y
        PHA
        INY
        LDA     0,Y
        PHA
        STX     XSAVE
        TYA
        TAX
        DEX
.L9BE2  DEX
        LDA     0,X
        STA     0,Y
        DEY
        CPX     XSAVE
        BNE     L9BE2
        INX
        INX
        PLA
        JMP     PUT

.L9BF3  JMP     POP

;       OSCLI

.L9BF6  DEFWORD "OSCLI"
        EQUW    L9BB9
.CLI    EQUW    *+2
        STX     XSAVE
        LDY     1,X
        LDA     0,X
        TAX
        JSR     OSCLI
        LDX     XSAVE
        JMP     POP

;       STRING

.L9C0F  DEFWORD "STRING"
        EQUW    L9BF6
.STRING EQUW    DOCOLON
        EQUW    MINUSONE
        EQUW    TOIN
        EQUW    PLUSSTORE
        EQUW    PWORD
        EQUW    ONEMINUS
        EQUW    SWAP
        EQUW    OVER
        EQUW    ZEROBRANCH,$A
        EQUW    ONEPLUS
        EQUW    SWAP
        EQUW    BRANCH,4
        EQUW    DROP
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       ($+)   ( addr1\count\addr2 ... )
;
;       > The string of length 'count' whose first character is at addr1 is
;       > added to the end of the string whose count byte is at addr2 (i.e.
;       > whose first character is at addr2+1). The count byte at addr2 is
;       > incremented to be the new length of the concatenated string.
;
;       : ($+)
;        SWAP  >R   SWAP   OVER   COUNT   DUP   R@
;        PLUS   5 ROLL   C!   +   R>   CMOVE
;       ;
;
; -----------------------------------------------------------------------------

.BRACKETDOLLARPLUS_NFA
        DEFWORD "($+)"
        EQUW    L9C0F
.BRACKETDOLLARPLUS
        EQUW    DOCOLON
        EQUW    SWAP
        EQUW    TOR
        EQUW    SWAP
        EQUW    OVER
        EQUW    COUNT
        EQUW    DUP
        EQUW    RFETCH
        EQUW    PLUS
        EQUW    LIT,5
        EQUW    ROLL
        EQUW    CSTORE
        EQUW    PLUS
        EQUW    RFROM
        EQUW    CMOVE
        EQUW    EXIT

;       (CLI)

.BRACKETCLI_NFA
        DEFWORD "(CLI)"
        EQUW    BRACKETDOLLARPLUS_NFA
.BRACKETCLI
        EQUW    DOCOLON
        EQUW    RFETCH
        EQUW    COUNT
        EQUW    ONEPLUS
        EQUW    RFROM
        EQUW    PLUS
        EQUW    TOR
        EQUW    CLI
        EQUW    EXIT

;       >CLI

.L9C7B  DEFIMM  ">CLI"
        EQUW    BRACKETCLI_NFA
.TOCLI  EQUW    DOCOLON
        EQUW    STATE
        EQUW    FETCH
        EQUW    ZEROBRANCH,$C
        EQUW    COMPILE
        EQUW    BRACKETCLI
        EQUW    TEXTCOMMA
        EQUW    BRANCH,6
        EQUW    DROP
        EQUW    CLI
        EQUW    EXIT

;       OS'

.OSQUOTE_NFA
        DEFIMM  "OS'"
        EQUW    L9C7B
        EQUW    DOCOLON
        EQUW    LIT,$27
        EQUW    STRING
        EQUW    QUERYDUP
        EQUW    ZEROBRANCH,$20
        EQUW    PAD
        EQUW    ZERO
        EQUW    OVER
        EQUW    STORE
        EQUW    BRACKETDOLLARPLUS
        EQUW    LIT,CarriageReturn
        EQUW    SPFETCH
        EQUW    ONE
        EQUW    PAD
        EQUW    BRACKETDOLLARPLUS
        EQUW    DROP
        EQUW    PAD
        EQUW    COUNT
        EQUW    TOCLI
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       KEY'   ( n ... )
;
;       > Used in the form:
;       >
;       > n KEY' text '
;       >
;       > It programs user-defined key n to execute the following text, up to
;       > the terminating single quote. A <RETURN> may be embedded in the text
;       > by including |M, as described in chapter 25 of the BBC Microcomputer
;       > User Guide. When used in a colon-definition n must be either a
;       > literal numeric value or a constant appearing immediately before
;       > KEY' .
;
;       : KEY'
;        STATE @ IF
;         ( compiling, look for LIT,n compiled before us )
;         -4 ALLOT HERE @   ' LIT CFA   - IF
;          ( no, assume CONSTANT compiled before us )
;          2 ALLOT   ( restore up to CONSTANT )
;          HERE @EXECUTE   ( execute CONSTANT to retrieve value )
;         ELSE
;          ( yes, LIT,n compiled before us )
;          HERE 2+ C@   ( fetch the low byte of n )
;         THEN
;        THEN
;        0
;        ( build the string KEY##" where ## is the number )
;        <# [ CHAR " ] HOLD # # [ CHAR Y ] HOLD [ CHAR E ] HOLD [ CHAR K ] HOLD #>
;        ( copy to PAD )
;        PAD   2DUP C!   1+ SWAP CMOVE
;        ( read specified string until single quote )
;        [ CHAR ' ] LITERAL STRING
;        ?DUP IF
;         PAD ($+)
;        THEN
;        [ HEX ] 0D22 LITERAL   SP@ 2   PAD ($+)
;        DROP   PAD COUNT   >CLI
;       ;
;
; -----------------------------------------------------------------------------

.KEYQUOTE_NFA
        DEFIMM  "KEY'"
        EQUW    OSQUOTE_NFA
        EQUW    DOCOLON
        EQUW    STATE
        EQUW    FETCH
        EQUW    ZEROBRANCH,42
        EQUW    LIT,-4
        EQUW    ALLOT
        EQUW    HERE
        EQUW    FETCH
        EQUW    LIT,LIT+2               ; Why not just LIT,LIT without
        EQUW    CFA                     ; the CFA?
        EQUW    MINUS
        EQUW    ZEROBRANCH,14
        EQUW    TWO
        EQUW    ALLOT
        EQUW    HERE
        EQUW    FETCHEXECUTE
        EQUW    BRANCH,8
        EQUW    HERE
        EQUW    TWOPLUS
        EQUW    CFETCH
        EQUW    ZERO
        EQUW    LESSSHARP
        EQUW    LIT,'"'
        EQUW    HOLD
        EQUW    SHARP
        EQUW    SHARP
        EQUW    LIT,'Y'
        EQUW    HOLD
        EQUW    LIT,'E'
        EQUW    HOLD
        EQUW    LIT,'K'
        EQUW    HOLD
        EQUW    SHARPGREATER
        EQUW    PAD
        EQUW    TWODUP
        EQUW    CSTORE
        EQUW    ONEPLUS
        EQUW    SWAP
        EQUW    CMOVE
        EQUW    LIT,'''
        EQUW    STRING
        EQUW    QUERYDUP
        EQUW    ZEROBRANCH,6
        EQUW    PAD
        EQUW    BRACKETDOLLARPLUS
        EQUW    LIT,$D22
        EQUW    SPFETCH
        EQUW    TWO
        EQUW    PAD
        EQUW    BRACKETDOLLARPLUS
        EQUW    DROP
        EQUW    PAD
        EQUW    COUNT
        EQUW    TOCLI
        EQUW    EXIT

;       (OPEN)

.BRACKETOPEN_NFA
        DEFWORD "(OPEN)"
        EQUW    KEYQUOTE_NFA
.BRACKETOPEN
        EQUW    *+2
        STX     XSAVE
        LDY     1,X
        LDA     0,X
        TAX
        LDA     #OpenFileInputOutput
        JSR     OSFIND
        LDX     XSAVE
        STA     0,X
        LDY     #0
        STY     1,X
        JMP     NEXT

;       CLOSE

.CLOSE_NFA
        DEFWORD "CLOSE"
        EQUW    BRACKETOPEN_NFA
.CLOSE  EQUW    *+2
        TYA
        LDY     0,X
        JSR     OSFIND
        JMP     POP

;       (R/W)

.BRACKETRSLASHW_NFA
        DEFWORD "(R/W)"
        EQUW    CLOSE_NFA
.BRACKETRSLASHW
        EQUW    *+2
        LDA     0,X
        INX
        INX
        INX
        STX     XSAVE
        JSR     OSGBPB
        LDX     XSAVE
        INX
        JMP     POPTWO

;       OPEN

.OPEN_NFA
        DEFWORD "OPEN"
        EQUW    LA093-REL
.OPEN   EQUW    DOCOLON
        EQUW    BRACKETOPEN
        EQUW    DUP
        EQUW    ZEROEQUAL
        EQUW    ZEROBRANCH,$C
        EQUW    ZERO
        EQUW    CLOSE
        EQUW    LIT,8
        EQUW    ERROR
        EQUW    CHANNEL
        EQUW    CSTORE
        EQUW    EXIT

;       DR/W

.L9DCF  DEFWORD "DR/W"
        EQUW    OPEN_NFA
.DRSLASHW
        EQUW    DOCOLON
        EQUW    TOR
        EQUW    SWAP
        EQUW    TOR
        EQUW    DUP
        EQUW    ZEROLESS
        EQUW    LIT,6
        EQUW    QUERYERROR
        EQUW    SSLASHFILE
        EQUW    SLASHMOD
        EQUW    DUP
        EQUW    MAXFILES
        EQUW    ONEMINUS
        EQUW    GREATERTHAN
        EQUW    LIT,6
        EQUW    QUERYERROR
        EQUW    LIT,'0'
        EQUW    PLUS
        EQUW    DUP
        EQUW    LIT,'9'
        EQUW    GREATERTHAN
        EQUW    ZEROBRANCH,8
        EQUW    LIT,7
        EQUW    PLUS
        EQUW    FNAME
        EQUW    CSTORE
        EQUW    FNAME
        EQUW    OPEN
        EQUW    ZERO
        EQUW    SWAP
        EQUW    BSLASHBUF
        EQUW    STAR
        EQUW    ZERO
        EQUW    BSLASHBUF
        EQUW    HIADDR
        EQUW    RFROM
        EQUW    CHANNEL
        EQUW    CFETCH
        EQUW    BYTESWAP
        EQUW    RFROM
        EQUW    TSTAR
        EQUW    ONEPLUS
        EQUW    BRACKETRSLASHW
        EQUW    CHANNEL
        EQUW    CFETCH
        EQUW    CLOSE
        EQUW    LIT,9
        EQUW    QUERYERROR
        EQUW    DROP
        EQUW    TWODROP
        EQUW    EXIT

;       EMPTY-BUFFERS

.L9E4A  DEFWORD "EMPTY-BUFFERS"
        EQUW    LA0DD-REL
.MTBUF  EQUW    DOCOLON
        EQUW    FIRST
        EQUW    LIMIT
        EQUW    OVER
        EQUW    MINUS
        EQUW    ERASE
        EQUW    FIRST
        EQUW    DUP
        EQUW    USE
        EQUW    STORE
        EQUW    PREV
        EQUW    STORE
        EQUW    EXIT

;       SETBUF

.L9E74  DEFWORD "SETBUF"
        EQUW    L9E4A
.SETBUF EQUW    DOCOLON
        EQUW    LIMIT
        EQUW    BUFSZ
        EQUW    SHARPBUF
        EQUW    FETCH
        EQUW    STAR
        EQUW    MINUS
        EQUW    LIT,FIRST+2
        EQUW    STORE
        EQUW    EXIT

;       INITBUF

.L9E93  DEFWORD "INITBUF"
        EQUW    L9E74
.INIBUF EQUW    DOCOLON
        EQUW    HIADDR
        EQUW    MINUSONE
        EQUW    EQUAL
        EQUW    ZEROBRANCH,8
        EQUW    HIMEM
        EQUW    BRANCH,6
        EQUW    LIT,$8000
        EQUW    LIT,LIMIT+2
        EQUW    STORE
        EQUW    MINBUF
        EQUW    SHARPBUF
        EQUW    STORE
        EQUW    SETBUF
        EQUW    MTBUF
        EQUW    EXIT

;       +BUF

.L9EC5  DEFWORD "+BUF"
        EQUW    L9E93
.PBUF   EQUW    DOCOLON
        EQUW    BUFSZ
        EQUW    PLUS
        EQUW    DUP
        EQUW    LIMIT
        EQUW    EQUAL
        EQUW    ZEROBRANCH,6
        EQUW    DROP
        EQUW    FIRST
        EQUW    DUP
        EQUW    PREV
        EQUW    FETCH
        EQUW    MINUS
        EQUW    EXIT

;       (UPDATE)

.L9EEA  DEFWORD "(UPDATE)"
        EQUW    L9EC5
.BRACKETUPDATE
        EQUW    DOCOLON
        EQUW    PREV
        EQUW    FETCH
        EQUW    FETCH
        EQUW    LIT,$8000
        EQUW    OR
        EQUW    PREV
        EQUW    FETCH
        EQUW    STORE
        EQUW    EXIT

;       BUFFER

.L9F0B  DEFWORD "BUFFER"
        EQUW    LA0E8-REL
.BUFFE  EQUW    DOCOLON
        EQUW    USE
        EQUW    FETCH
        EQUW    DUP
        EQUW    TOR
        EQUW    PBUF
        EQUW    ZEROBRANCH,-4
        EQUW    USE
        EQUW    STORE
        EQUW    RFETCH
        EQUW    FETCH
        EQUW    ZEROLESS
        EQUW    ZEROBRANCH,$14
        EQUW    RFETCH
        EQUW    TWOPLUS
        EQUW    RFETCH
        EQUW    FETCH
        EQUW    LIT,$7FFF
        EQUW    AND
        EQUW    ZERO
        EQUW    RSLASHW
        EQUW    RFETCH
        EQUW    STORE
        EQUW    RFETCH
        EQUW    PREV
        EQUW    STORE
        EQUW    RFROM
        EQUW    TWOPLUS
        EQUW    EXIT

;       BLOCK

.L9F54  DEFWORD "BLOCK"
        EQUW    L9F0B
.BLOCK  EQUW    DOCOLON
        EQUW    OFFSET
        EQUW    FETCH
        EQUW    PLUS
        EQUW    TOR
        EQUW    PREV
        EQUW    FETCH
        EQUW    DUP
        EQUW    FETCH
        EQUW    RFETCH
        EQUW    MINUS
        EQUW    TSTAR
        EQUW    ZEROBRANCH,$30
        EQUW    PBUF
        EQUW    ZEROEQUAL
        EQUW    ZEROBRANCH,$12
        EQUW    DROP
        EQUW    RFETCH
        EQUW    BUFFE
        EQUW    DUP
        EQUW    RFETCH
        EQUW    ONE
        EQUW    RSLASHW
        EQUW    TWOMINUS
        EQUW    DUP
        EQUW    FETCH
        EQUW    RFETCH
        EQUW    MINUS
        EQUW    TSTAR
        EQUW    ZEROEQUAL
        EQUW    ZEROBRANCH,-38
        EQUW    DUP
        EQUW    PREV
        EQUW    STORE
        EQUW    RFROM
        EQUW    DROP
        EQUW    TWOPLUS
        EQUW    USE
        EQUW    FETCH
        EQUW    PREV
        EQUW    FETCH
        EQUW    EQUAL
        EQUW    ZEROBRANCH,$E
        EQUW    USE
        EQUW    FETCH
        EQUW    PBUF
        EQUW    DROP
        EQUW    USE
        EQUW    STORE
        EQUW    EXIT

;       LOAD

.L9FC8  DEFWORD "LOAD"
        EQUW    L9F54
.LOAD   EQUW    DOCOLON
        EQUW    BLK
        EQUW    FETCH
        EQUW    TOR
        EQUW    TOIN
        EQUW    FETCH
        EQUW    TOR
        EQUW    ZERO
        EQUW    TOIN
        EQUW    STORE
        EQUW    BSLASHSCR
        EQUW    STAR
        EQUW    BLK
        EQUW    STORE
        EQUW    INTERPRET
        EQUW    RFROM
        EQUW    TOIN
        EQUW    STORE
        EQUW    RFROM
        EQUW    BLK
        EQUW    STORE
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       Beginning of the section of words that are by default relocated to RAM
;       by START .
;
; -----------------------------------------------------------------------------

REL_SRC = *

IF RELOCATE
REL = REL_SRC-REL_DST
ELSE
REL = 0
ENDIF

; -----------------------------------------------------------------------------
;
;       EMIT   ( c ... )
;
;       > A vectored routine, initialised on a COLD start to execute (EMIT) ,
;       > which transmits ASCII character c to the output device. The contents
;       > of OUT are incremented for each character output. The stack value is
;       > masked to a 7-bit value before transmission.
;
; -----------------------------------------------------------------------------

.L9FFB  DEFWORD "EMIT"
        EQUW    BRACKETEMIT_NFA
.XEMIT  EQUW    DOEXVEC
        EQUW    BRACKETEMIT

EMIT = XEMIT-REL

; -----------------------------------------------------------------------------
;
;       KEY   ( ... c )
;
;       > A vectored routine initialised on a cold start to execute (KEY) which
;       > leaves the ASCII value of the next available character from the
;       > current input device.
;
; -----------------------------------------------------------------------------

.KEY_NFA
        DEFWORD "KEY"
        EQUW    L85D7
.XKEY   EQUW    DOEXVEC
        EQUW    BRACKETKEY

KEY = XKEY-REL

; -----------------------------------------------------------------------------
;
;       FIRST   ( ... addr )
;
;       > A constant that leaves the address addr of the first byte of the
;       > mass storage buffer area.
;
; -----------------------------------------------------------------------------


.LA010  DEFWORD "FIRST"
        EQUW    PAD_NFA
.XFIRS  EQUW    DOCONSTANT
        EQUW    BUF1

FIRST = XFIRS-REL

; -----------------------------------------------------------------------------
;
;       LIMIT   ( ... addr )
;
;       > A constant leaving the address of the first byte after the highest
;       > memory available for the mass storage buffers.
;
; -----------------------------------------------------------------------------

.LA01C  DEFWORD "LIMIT"
        EQUW    LA010-REL
.XLIMI  EQUW    DOCONSTANT
        EQUW    EM

LIMIT = XLIMI-REL

; -----------------------------------------------------------------------------
;
;       CREATE
;
;       > A vectored routine initialised on a cold start to execute (CREATE)
;       > which creates a new dictionary header. Used as
;       >
;       > CREATE CCCC
;       >
;       > to create a dictionary header for the word CCCC with the code pointer
;       > of VARIABLE . Later execution of CCCC will therefore leave the
;       > address of the first byte of its parameter field. See also DOES> .
;
; -----------------------------------------------------------------------------

.LA028  DEFWORD "CREATE"
        EQUW    L914F
.XCREATE
        EQUW    DOEXVEC
        EQUW    BRACKETCREATE

CREATE = XCREATE-REL

;       NUM

.LA035  DEFWORD "NUM"
        EQUW    L92F9
.XNUM   EQUW    DOEXVEC
        EQUW    BRACKETNUM

NUM     =       XNUM-REL

;       FORTH

.LA03F  DEFIMM  "FORTH"
        EQUW    VOCABULARY_NFA
.XFORT  EQUW    DOVOCABULARY
        DEFWORD " "
        EQUW    TOPNFA
.VL0    EQUW    0

FORTH   =       XFORT-REL

;       ABORT

.LA04F  DEFWORD "ABORT"
        EQUW    L9403
.XABORT EQUW    DOEXVEC
        EQUW    BRACKETABORT

ABORT   =       XABORT-REL

; -----------------------------------------------------------------------------
;
;       MESSAGE   ( n ... )
;
;       > A vectored error routine initialised on a cold start to execute MSG#
;       > which displays n as an error message number.
;
;       Note that it is actually set to execute $MSG in this ROM version.
;
; -----------------------------------------------------------------------------

.LA05B  DEFWORD "MESSAGE"
        EQUW    MSGNUM_NFA
.XMESSAGE
        EQUW    DOEXVEC
        EQUW    DOLLARMSG

MESSAGE         =       XMESSAGE-REL

;       S/FILE

.LA069  DEFWORD "S/FILE"
        EQUW    BRACKETRSLASHW_NFA
.XSSLASHFILE
        EQUW    DOCONSTANT
        EQUW    9

SSLASHFILE      =       XSSLASHFILE-REL

;       MAXFILES

.LA076  DEFWORD "MAXFILES"
        EQUW    LA069-REL
.XMAXFILES
        EQUW    DOCONSTANT
        EQUW    20

MAXFILES        =       XMAXFILES-REL

;       CHANNEL

.LA085  DEFWORD "CHANNEL"
        EQUW    LA076-REL
.XCHANNEL
        EQUW    DOVARIABLE
        EQUW    18

CHANNEL         =       XCHANNEL-REL

;       FNAME

.LA093  DEFWORD "FNAME"
        EQUW    LA085-REL
.XFNAME EQUW    DOVARIABLE
        EQUB    "1SCREEN",CarriageReturn

FNAME   =       XFNAME-REL

;       R/W

.LA0A5  DEFWORD "R/W"
        EQUW    L9DCF
.XRSLASHW
        EQUW    DOEXVEC
        EQUW    DRSLASHW

RSLASHW         =       XRSLASHW-REL

;       MINBUF

.LA0AF  DEFWORD "MINBUF"
        EQUW    LA0A5-REL
.XMINBUF
        EQUW    DOCONSTANT
        EQUW    NOBUF

MINBUF  =       XMINBUF-REL

;       BUFSZ

.LA0BC  DEFWORD "BUFSZ"
        EQUW    LA0AF-REL
.XBUFSZ EQUW    DOCONSTANT
        EQUW    HDBT

BUFSZ   =       XBUFSZ-REL

; -----------------------------------------------------------------------------
;
;       USE   ( ... addr )
;
;       > A variable containing the address of the mass storage buffer to use
;       > next, as the last recently written.
;
; -----------------------------------------------------------------------------

.LA0C8  DEFWORD "USE"
        EQUW    LA0BC-REL
.XUSE   EQUW    DOVARIABLE
        EQUW    BUF1

USE     =       XUSE-REL

; -----------------------------------------------------------------------------
;
;       PREV   ( ... addr )
;
;       > A variable containing a pointer to the start of the most recently
;       > used mass storage buffer.
;
; -----------------------------------------------------------------------------

.LA0D2  DEFWORD "PREV"
        EQUW    LA0C8-REL
.XPREV  EQUW    DOVARIABLE
        EQUW    BUF1+HDBT

PREV    =       XPREV-REL

;       #BUF

.LA0DD  DEFWORD "#BUF"
        EQUW    LA0D2-REL
.XSHARPBUF
        EQUW    DOVARIABLE
        EQUW    NOBUF

SHARPBUF        =       XSHARPBUF-REL

;       UPDATE

.LA0E8  DEFWORD "UPDATE"
        EQUW    L9EEA
.XUPDATE
        EQUW    DOEXVEC
        EQUW    BRACKETUPDATE

UPDATE  =       XUPDATE-REL

;       (DISK)

.LA0F5  DEFWORD "(DISK)"
        EQUW    L9FC8
.XPDIS  EQUW    DOCOLON
        EQUW    BRACKETCLI
        EQUB    5,"DISK",CarriageReturn
        EQUW    EXIT

PDISK   =       XPDIS-REL

;       TLD

.LA10A  DEFWORD "TLD"
        EQUW    LA274
.XTLD   EQUW    DOCOLON
        EQUW    BRACKETCLI
        EQUB    16
        EQUB    "LOAD"""
.LA11A  EQUB    "XXXX"" "
.LA120  EQUB    "YYYY",CarriageReturn
        EQUW    EXIT

TLD     =       XTLD-REL

;       TSV

.LA127  DEFWORD "TSV"
        EQUW    LA10A-REL
.XTSV   EQUW    DOCOLON
        EQUW    BRACKETCLI
        EQUB    21,"SAVE""XXXX"" YYYY ZZZZ",CarriageReturn
        EQUW    EXIT

TSV     =       XTSV-REL

;       SSV

.LA149  DEFWORD "SSV"
        EQUW    LA127-REL
.XSSV   EQUW    DOCOLON
        EQUW    BRACKETCLI
        EQUB    24
        EQUB    "SAVE"""
.LA159  EQUB    "XSCREEN"" "
.LA162  EQUB    "XXXX "
.LA167  EQUB    "YYYY",CarriageReturn
        EQUW    EXIT

SSV     =       XSSV-REL

;       ASSEMBLER

.LA16E  DEFIMM  "ASSEMBLER"
        EQUW    LA4F6
.XASSEMBLER
        EQUW    DOVOCABULARY
        DEFWORD " "
        EQUW    ENDCODE_NFA
.LA180  EQUW    VL0-REL

ASSEMBLER       =       XASSEMBLER-REL

;       MODE

.LA182  DEFWORD "MODE"
        EQUW    LA5C7
.XMOD   EQUW    DOVARIABLE
        EQUW    2

AMODE   =       XMOD-REL

; -----------------------------------------------------------------------------
;
;       EDITOR
;
; -----------------------------------------------------------------------------

.EDITOR_NFA
        DEFIMM  "EDITOR"
        EQUW    LAB39
.XEDITOR
        EQUW    DOVOCABULARY
        DEFWORD " "
        EQUW    C_NFA
        EQUW    LA180-REL
        EQUB    0

EDITOR  =       XEDITOR-REL


; -----------------------------------------------------------------------------
;
;       End of the section of words that is by default relocated to RAM by
;       START .
;
; -----------------------------------------------------------------------------

REL_SZ = *-REL_SRC

;       DISK

.LA19F  DEFWORD "DISK"
        EQUW    LA0F5-REL
.DISK   EQUW    DOCOLON
        EQUW    HIADDR
        EQUW    MINUSONE
        EQUW    EQUAL
        EQUW    ZEROBRANCH,$10
        EQUW    PAGE
        EQUW    LIT,$1900
        EQUW    ULESS
        EQUW    LIT,25
        EQUW    QUERYERROR
        EQUW    LIT,RSLASHW+2
        EQUW    LIT,DRSLASHW+2
        EQUW    DOVEC
        EQUW    PDISK
        EQUW    EXIT

;       SAVE-BUFFERS

.LA1CE  DEFWORD "SAVE-BUFFERS"
        EQUW    LA19F
.SAVBUF EQUW    DOCOLON
        EQUW    FIRST
        EQUW    LIMIT
        EQUW    FIRST
        EQUW    MINUS
        EQUW    BUFSZ
        EQUW    SLASH
        EQUW    ZERO
        EQUW    XDO
        EQUW    DUP
        EQUW    FETCH
        EQUW    ZEROLESS
        EQUW    ZEROBRANCH,$1C
        EQUW    DUP
        EQUW    FETCH
        EQUW    LIT,$7FFF
        EQUW    AND
        EQUW    OVER
        EQUW    STORE
        EQUW    DUP
        EQUW    TWOPLUS
        EQUW    OVER
        EQUW    FETCH
        EQUW    ZERO
        EQUW    RSLASHW
        EQUW    BUFSZ
        EQUW    PLUS
        EQUW    BRACKETLOOP,$FFD6
        EQUW    DROP
        EQUW    EXIT

;       FLUSH

.LA21F  DEFWORD "FLUSH"
        EQUW    LA1CE
.FLUSH  EQUW    DOCOLON
        EQUW    SAVBUF
        EQUW    MTBUF
        EQUW    EXIT

;       -->

.LA22F  DEFIMM  "-->"
        EQUW    LA21F
.ARROW  EQUW    DOCOLON
        EQUW    QUERYLOADING
        EQUW    ZERO
        EQUW    TOIN
        EQUW    STORE
        EQUW    BSLASHSCR
        EQUW    BLK
        EQUW    FETCH
        EQUW    OVER
        EQUW    MOD
        EQUW    MINUS
        EQUW    BLK
        EQUW    PLUSSTORE
        EQUW    EXIT

;       (LINE)

.LA251  DEFWORD "(LINE)"
        EQUW    LA22F
.PLINE  EQUW    DOCOLON
        EQUW    TOR
        EQUW    CSLASHL
        EQUW    BSLASHBUF
        EQUW    STARSLASHMOD
        EQUW    RFROM
        EQUW    BSLASHSCR
        EQUW    STAR
        EQUW    PLUS
        EQUW    BLOCK
        EQUW    PLUS
        EQUW    CSLASHL
        EQUW    EXIT

;       .LINE

.LA274  DEFWORD ".LINE"
        EQUW    LA251
.DOTLINE
        EQUW    DOCOLON
        EQUW    PLINE
        EQUW    DTRAI
        EQUW    TYPE
        EQUW    EXIT

;       4HEX

.LA286  DEFWORD "4HEX"
        EQUW    LA149-REL
.FHEX   EQUW    DOCOLON
        EQUW    BASE
        EQUW    FETCH
        EQUW    HEX
        EQUW    SWAP
        EQUW    ZERO
        EQUW    LESSSHARP
        EQUW    SHARP
        EQUW    SHARP
        EQUW    SHARP
        EQUW    SHARP
        EQUW    SHARPGREATER
        EQUW    ROT
        EQUW    BASE
        EQUW    STORE
        EQUW    EXIT

;       TR

.LA2AD  DEFWORD "TR"
        EQUW    LA286
.TR     EQUW    DOCOLON
        EQUW    FHEX
        EQUW    LIT,TLD+2
        EQUW    LIT,8
        EQUW    PLUS
        EQUW    SWAP
        EQUW    CMOVE
        EQUW    FHEX
        EQUW    LIT,TLD+2
        EQUW    LIT,$E
        EQUW    PLUS
        EQUW    SWAP
        EQUW    CMOVE
        EQUW    TLD
        EQUW    EXIT

;       TW

.LA2D8  DEFWORD "TW"
        EQUW    LA2AD
.TW     EQUW    DOCOLON
        EQUW    FHEX
        EQUW    LIT,TSV+2
        EQUW    LIT,8
        EQUW    PLUS
        EQUW    SWAP
        EQUW    CMOVE
        EQUW    DUP
        EQUW    FHEX
        EQUW    LIT,TSV+2
        EQUW    LIT,$E
        EQUW    PLUS
        EQUW    SWAP
        EQUW    CMOVE
        EQUW    BSLASHBUF
        EQUW    PLUS
        EQUW    FHEX
        EQUW    LIT,TSV+2
        EQUW    LIT,$13
        EQUW    PLUS
        EQUW    SWAP
        EQUW    CMOVE
        EQUW    TSV
        EQUW    EXIT

;       TR/W

.LA319  DEFWORD "TR/W"
        EQUW    LA2D8
.TRSLASHW
        EQUW    DOCOLON
        EQUW    ZEROBRANCH,8
        EQUW    TR
        EQUW    BRANCH,4
        EQUW    TW
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       ?R/W
;
;       Makes R/W point to an implementation for cassette (TR/W) or disc
;       (DR/W), depending on the active filing system.
;
;       : ?R/W
;        ?FILE 3 < IF
;         [ ' R/W ] [ ' TR/W ] DOVEC
;        ELSE
;         [ ' R/W ] [ ' DR/W ] DOVEC
;        THEN
;       ;
;
; -----------------------------------------------------------------------------

.LA330  DEFWORD "?R/W"
        EQUW    LA319
.QUERYRSLASHW
        EQUW    DOCOLON
        EQUW    QUERYFILE
        EQUW    LIT,3
        EQUW    LESS
        EQUW    ZEROBRANCH,16
        EQUW    LIT,RSLASHW+2
        EQUW    LIT,TRSLASHW+2
        EQUW    DOVEC
        EQUW    BRANCH,12
        EQUW    LIT,RSLASHW+2
        EQUW    LIT,DRSLASHW+2
        EQUW    DOVEC
        EQUW    EXIT

;       TAPE

.LA35F  DEFWORD "TAPE"
        EQUW    LA330
.TAPE   EQUW    DOCOLON
        EQUW    BRACKETCLI
        EQUB    5,"TAPE",CarriageReturn
        EQUW    LIT,RSLASHW+2
        EQUW    LIT,TRSLASHW+2
        EQUW    DOVEC
        EQUW    EXIT

;       CREATE-SCREENS

.LA37C  DEFWORD "CREATE-SCREENS"
        EQUW    LA35F
.CRESCR EQUW    DOCOLON
        EQUW    QUERYFILE
        EQUW    LIT,4
        EQUW    LESS
        EQUW    ZEROBRANCH,$E
        EQUW    BRACKETDOTQUOTE
        EQUB    7,"No disk"
        EQUW    ABORT
        EQUW    HERE
        EQUW    DUP
        EQUW    BSLASHBUF
        EQUW    BSLASHSCR
        EQUW    SSLASHFILE
        EQUW    STAR
        EQUW    STAR
        EQUW    TWODUP
        EQUW    PLUS
        EQUW    FIRST
        EQUW    OVER
        EQUW    ULESS
        EQUW    LIT,25
        EQUW    QUERYERROR
        EQUW    BRACKETDOTQUOTE
        EQUB    20,"Are you sure (Y/N)? "
        EQUW    KEY
        EQUW    LIT,'Y'
        EQUW    EQUAL
        EQUW    ZEROBRANCH,$6A
        EQUW    FHEX
        EQUW    LIT,LA167-REL
        EQUW    SWAP
        EQUW    CMOVE
        EQUW    BLANKS
        EQUW    FHEX
        EQUW    LIT,LA162-REL
        EQUW    SWAP
        EQUW    CMOVE
        EQUW    CR
        EQUW    BRACKETDOTQUOTE
        EQUB    17,"Creating screens "
        EQUW    LIT,$2F
        EQUW    FNAME
        EQUW    CSTORE
        EQUW    MAXFILES
        EQUW    ZERO
        EQUW    XDO
        EQUW    FNAME
        EQUW    CFETCH
        EQUW    ONEPLUS
        EQUW    DUP
        EQUW    LIT,$3A
        EQUW    EQUAL
        EQUW    ZEROBRANCH,8
        EQUW    LIT,7
        EQUW    PLUS
        EQUW    DUP
        EQUW    FNAME
        EQUW    CSTORE
        EQUW    LIT,LA159-REL
        EQUW    CSTORE
        EQUW    SSV
        EQUW    BRACKETLOOP,$FFD8
        EQUW    BRANCH,6
        EQUW    TWODROP
        EQUW    TWODROP
        EQUW    EXIT

;       LIST

.LA456  DEFWORD "LIST"
        EQUW    LA37C
.LIST   EQUW    DOCOLON
        EQUW    DECIM
        EQUW    CR
        EQUW    DUP
        EQUW    SCR
        EQUW    STORE
        EQUW    DUP
        EQUW    BLOCK
        EQUW    DROP
        EQUW    BRACKETDOTQUOTE
        EQUB    6,"SCR # "
        EQUW    DUP
        EQUW    DOT
        EQUW    LIT,5
        EQUW    SPACES
        EQUW    HDOT
        EQUW    BRACKETDOTQUOTE
        EQUB    1,'H'
        EQUW    LIT,16
        EQUW    ZERO
        EQUW    XDO
        EQUW    CR
        EQUW    IDO
        EQUW    LIT,3
        EQUW    DOTR
        EQUW    SPACE
        EQUW    IDO
        EQUW    SCR
        EQUW    FETCH
        EQUW    DOTLINE
        EQUW    BRACKETLOOP,$FFEA
        EQUW    CR
        EQUW    EXIT

;       79-STANDARD

.LA4AC  DEFWORD "79-STANDARD"
        EQUW    LA456
.STD79  EQUW    DOCOLON
        EQUW    EXIT

;       INDEX

.LA4BE  DEFWORD "INDEX"
        EQUW    LA4AC
.INDEX  EQUW    DOCOLON
        EQUW    LIT,$C
        EQUW    EMIT
        EQUW    CR
        EQUW    ONEPLUS
        EQUW    SWAP
        EQUW    XDO
        EQUW    CR
        EQUW    IDO
        EQUW    LIT,3
        EQUW    DOTR
        EQUW    SPACE
        EQUW    ZERO
        EQUW    IDO
        EQUW    DOTLINE
        EQUW    QTAB
        EQUW    ZEROBRANCH,4
        EQUW    LEAVE
        EQUW    BRACKETLOOP,$FFE4
        EQUW    EXIT

;       TRIAD

.LA4F6  DEFWORD "TRIAD"
        EQUW    LA4BE
.TRIAD  EQUW    DOCOLON
        EQUW    LIT,$C
        EQUW    EMIT
        EQUW    LIT,3
        EQUW    SLASH
        EQUW    LIT,3
        EQUW    STAR
        EQUW    LIT,3
        EQUW    OVER
        EQUW    PLUS
        EQUW    SWAP
        EQUW    XDO
        EQUW    CR
        EQUW    IDO
        EQUW    LIST
        EQUW    BRACKETLOOP,$FFF8
        EQUW    CR
        EQUW    EXIT

;       ?CURRENT

.LA52C  DEFWORD "?CURRENT"
        EQUW    LA16E-REL
.QCURR  EQUW    DOCOLON
        EQUW    TWOPLUS
        EQUW    CURRENT
        EQUW    FETCH
        EQUW    MINUS
        EQUW    LIT,14
        EQUW    QUERYERROR
        EQUW    EXIT    

;       N

.LA549  DEFWORD "N"
        EQUW    FORTH+2
        EQUW    DOCONSTANT
        EQUW    N

;       XSAVE

.LA551  DEFWORD "XSAVE"
        EQUW    LA549
        EQUW    DOCONSTANT
        EQUW    XSAVE

;       W

.LA55D  DEFWORD "W"
        EQUW    LA551
        EQUW    DOCONSTANT
        EQUW    W

;       IP

.LA565  DEFWORD "IP"
        EQUW    LA55D
        EQUW    DOCONSTANT
        EQUW    IP

;       UP

.LA56E  DEFWORD "UP"
        EQUW    LA565
        EQUW    DOCONSTANT
        EQUW    UP

;       PUSH

.LA577  DEFWORD "PUSH"
        EQUW    LA56E
        EQUW    DOCONSTANT
        EQUW    PUSH

;       PUT

.LA582  DEFWORD "PUT"
        EQUW    LA577
        EQUW    DOCONSTANT
        EQUW    PUT

;       NEXT

.LA58C  DEFWORD "NEXT"
        EQUW    LA582
        EQUW    DOCONSTANT
        EQUW    NEXT

;       SETUP

.LA597  DEFWORD "SETUP"
        EQUW    LA58C
        EQUW    DOCONSTANT
        EQUW    SETUP

;       POPTWO

.LA5A3  DEFWORD "POPTWO"
        EQUW    LA597
        EQUW    DOCONSTANT
        EQUW    POPTWO

;       POP

.LA5B0  DEFWORD "POP"
        EQUW    LA5A3
        EQUW    DOCONSTANT
        EQUW    POP

;       PUSH0A

.LA5BA  DEFWORD "PUSH0A"
        EQUW    LA5B0
        EQUW    DOCONSTANT
        EQUW    PUSH0A

;       BASE-ADDR

.LA5C7  DEFWORD "BASE-ADDR"
        EQUW    LA5BA
.BASEADDR
        EQUW    DOVARIABLE
        EQUB    $09,$09,$05,$15,$15,$01,$11,$80
        EQUB    $09,$80,$0D,$1D,$19,$80,$80,$80
        EQUB    $80,$00,$04,$14,$14,$80,$80,$80
        EQUB    $80,$80,$0C,$1C,$1C,$80,$80,$2C

;       .A

.LA5F5  DEFWORD ".A"
        EQUW    LA182-REL
.DOTA   EQUW    DOCOLON
        EQUW    ZERO
        EQUW    AMODE
        EQUW    STORE
        EQUW    EXIT

;       #

.LA604  DEFWORD "#"
        EQUW    LA5F5
.ANUM   EQUW    DOCOLON
        EQUW    ONE
        EQUW    AMODE
        EQUW    STORE
        EQUW    EXIT

;       MEM

.LA612  DEFWORD "MEM"
        EQUW    LA604
.MEM    EQUW    DOCOLON
        EQUW    TWO
        EQUW    AMODE
        EQUW    STORE
        EQUW    EXIT

;       ,X

.LA622  DEFWORD ",X"
        EQUW    LA612
.COMX   EQUW    DOCOLON
        EQUW    LIT,3
        EQUW    AMODE
        EQUW    STORE
        EQUW    EXIT

;       ,Y

.LA633  DEFWORD ",Y"
        EQUW    LA622
.COMY   EQUW    DOCOLON
        EQUW    LIT,4
        EQUW    AMODE
        EQUW    STORE
        EQUW    EXIT

;       X)

.LA644  DEFWORD "X)"
        EQUW    LA633
.XPAR   EQUW    DOCOLON
        EQUW    LIT,5
        EQUW    AMODE
        EQUW    STORE
        EQUW    EXIT

;       )Y

.LA655  DEFWORD ")Y"
        EQUW    LA644
.PARY   EQUW    DOCOLON
        EQUW    LIT,6
        EQUW    AMODE
        EQUW    STORE
        EQUW    EXIT

;       )

.LA666  DEFWORD ")"
        EQUW    LA655
.APAR   EQUW    DOCOLON
        EQUW    LIT,$F
        EQUW    AMODE
        EQUW    STORE
        EQUW    EXIT

;       BOT

.LA676  DEFWORD "BOT"
        EQUW    LA666
.ABOT   EQUW    DOCOLON
        EQUW    COMX
        EQUW    ZERO
        EQUW    EXIT

;       SEC

.LA684  DEFWORD "SEC"
        EQUW    LA676
.ASEC   EQUW    DOCOLON
        EQUW    COMX
        EQUW    TWO
        EQUW    EXIT

;       RP)

.LA692  DEFWORD "RP)"
        EQUW    LA684
.RPP    EQUW    DOCOLON
        EQUW    COMX
        EQUW    LIT,$101
        EQUW    EXIT

;       CHKMODE

.LA6A2  DEFWORD "CHKMODE"
        EQUW    LA692
.CHKMOD EQUW    DOCOLON
        EQUW    ZEROBRANCH,$1A
        EQUW    AMODE
        EQUW    FETCH
        EQUW    LIT,8
        EQUW    AND
        EQUW    ZEROEQUAL
        EQUW    ZEROBRANCH,$A
        EQUW    LIT,8
        EQUW    AMODE
        EQUW    PLUSSTORE
        EQUW    ONE
        EQUW    AMODE
        EQUW    FETCH
        EQUW    LIT,$F
        EQUW    AND
        EQUW    QUERYDUP
        EQUW    ZEROBRANCH,$E
        EQUW    ZERO
        EQUW    XDO
        EQUW    DUP
        EQUW    PLUS
        EQUW    BRACKETLOOP,$FFFA
        EQUW    OVER
        EQUW    ONEPLUS
        EQUW    FETCH
        EQUW    AND
        EQUW    ZEROEQUAL
        EQUW    EXIT

;       SOP

.LA6F4  DEFWORD "SOP"
        EQUW    LA6A2
.SOP    EQUW    DOCOLON
        EQUW    CREATE
        EQUW    CCOMMA
        EQUW    BRACKETSEMICOLONCODE
.DOSOP  JSR     DODOES
        EQUW    CFETCH
        EQUW    CCOMMA
        EQUW    MEM
        EQUW    EXIT

;       BRK,

.LA70D  DEFWORD "BRK,"
        EQUW    LA6F4
        EQUW    DOSOP
        EQUB    0

;       CLC,

.LA717  DEFWORD "CLC,"
        EQUW    LA70D
        EQUW    DOSOP
        EQUB    $18

;       CLD,

.LA721  DEFWORD "CLD,"
        EQUW    LA717
        EQUW    DOSOP
        EQUB    $D8

;       CLI,

.LA72B  DEFWORD "CLI,"
        EQUW    LA721
        EQUW    DOSOP
        EQUB    $58

;       CLV,

.LA735  DEFWORD "CLV,"
        EQUW    LA72B
        EQUW    DOSOP
        EQUB    $B8

;       DEX,

.LA73F  DEFWORD "DEX,"
        EQUW    LA735
        EQUW    DOSOP
        EQUB    $CA

;       DEY,

.LA749  DEFWORD "DEY,"
        EQUW    LA73F
        EQUW    DOSOP
        EQUB    $88

;       INX,

.LA753  DEFWORD "INX,"
        EQUW    LA749
        EQUW    DOSOP
        EQUB    $E8

;       INY,

.LA75D  DEFWORD "INY,"
        EQUW    LA753
        EQUW    DOSOP
        EQUB    $C8

;       NOP,

.LA767  DEFWORD "NOP,"
        EQUW    LA75D
        EQUW    DOSOP
        EQUB    $EA

;       PHA,

.LA771  DEFWORD "PHA,"
        EQUW    LA767
        EQUW    DOSOP
        EQUB    $48

;       PHP,

.LA77B  DEFWORD "PHP,"
        EQUW    LA771
        EQUW    DOSOP
        EQUB    8

;       PLA,

.LA785  DEFWORD "PLA,"
        EQUW    LA77B
        EQUW    DOSOP
        EQUB    $68

;       PLP,

.LA78F  DEFWORD "PLP,"
        EQUW    LA785
        EQUW    DOSOP
        EQUB    $28

;       RTI,

.LA799  DEFWORD "RTI,"
        EQUW    LA78F
        EQUW    DOSOP
        EQUB    $40

;       RTS,

.LA7A3  DEFWORD "RTS,"
        EQUW    LA799
        EQUW    DOSOP
        EQUB    $60

;       SEC,

.LA7AD  DEFWORD "SEC,"
        EQUW    LA7A3
        EQUW    DOSOP
        EQUB    $38

;       SED,

.LA7B7  DEFWORD "SED,"
        EQUW    LA7AD
        EQUW    DOSOP
        EQUB    $F8

;       SEI,

.LA7C1  DEFWORD "SEI,"
        EQUW    LA7B7
        EQUW    DOSOP
        EQUB    $78

;       TAX,

.LA7CB  DEFWORD "TAX,"
        EQUW    LA7C1
        EQUW    DOSOP
        EQUB    $AA

;       TAY,

.LA7D5  DEFWORD "TAY,"
        EQUW    LA7CB
        EQUW    DOSOP
        EQUB    $A8

;       TSX,

.LA7DF  DEFWORD "TSX,"
        EQUW    LA7D5
        EQUW    DOSOP
        EQUB    $BA

;       TXA,

.LA7E9  DEFWORD "TXA,"
        EQUW    LA7DF
        EQUW    DOSOP
        EQUB    $8A

;       TXS,

.LA7F3  DEFWORD "TXS,"
        EQUW    LA7E9
        EQUW    DOSOP
        EQUB    $9A

;       TYA,

.LA7FD  DEFWORD "TYA,"
        EQUW    LA7F3
        EQUW    DOSOP
        EQUB    $98

;       MOP

.LA807  DEFWORD "MOP"
        EQUW    LA7FD
.MOP    EQUW    DOCOLON
        EQUW    CREATE
        EQUW    CCOMMA
        EQUW    COMMA
        EQUW    BRACKETSEMICOLONCODE
.DOMOP  JSR     DODOES
        EQUW    DUP
        EQUW    ONEPLUS
        EQUW    FETCH
        EQUW    LIT,$80
        EQUW    AND
        EQUW    ZEROBRANCH,$A
        EQUW    LIT,$10
        EQUW    AMODE
        EQUW    PLUSSTORE
        EQUW    OVER
        EQUW    LIT,$FF00
        EQUW    AND
        EQUW    CHKMOD
        EQUW    CHKMOD
        EQUW    ZEROBRANCH,$10
        EQUW    MEM
        EQUW    CR
        EQUW    LAST
        EQUW    IDDOT
        EQUW    LIT,3
        EQUW    ERROR
        EQUW    CFETCH
        EQUW    AMODE
        EQUW    CFETCH
        EQUW    BASEADDR
        EQUW    PLUS
        EQUW    CFETCH
        EQUW    PLUS
        EQUW    CCOMMA
        EQUW    AMODE
        EQUW    CFETCH
        EQUW    LIT,7
        EQUW    AND
        EQUW    ZEROBRANCH,$1E
        EQUW    AMODE
        EQUW    CFETCH
        EQUW    LIT,$F
        EQUW    AND
        EQUW    LIT,7
        EQUW    LESS
        EQUW    ZEROBRANCH,8
        EQUW    CCOMMA
        EQUW    BRANCH,4
        EQUW    COMMA
        EQUW    MEM
        EQUW    EXIT

;       ADC,

.LA88E  DEFWORD "ADC,"
        EQUW    LA807
        EQUW    DOMOP
        EQUB    $60,$6E,$1C

;       AND,

.LA89A  DEFWORD "AND,"
        EQUW    LA88E
        EQUW    DOMOP
        EQUB    $20,$6E,$1C

;       CMP,

.LA8A6  DEFWORD "CMP,"
        EQUW    LA89A
        EQUW    DOMOP
        EQUB    $C0,$6E,$1C

;       EOR,

.LA8B2  DEFWORD "EOR,"
        EQUW    LA8A6
        EQUW    DOMOP
        EQUB    $40,$6E,$1C

;       LDA,

.LA8BE  DEFWORD "LDA,"
        EQUW    LA8B2
        EQUW    DOMOP
        EQUB    $A0,$6E,$1C

;       ORA,

.LA8CA  DEFWORD "ORA,"
        EQUW    LA8BE
        EQUW    DOMOP
        EQUB    0,$6E,$1C

;       SBC,

.LA8D6  DEFWORD "SBC,"
        EQUW    LA8CA
        EQUW    DOMOP
        EQUB    $E0,$6E,$1C

;       STA,

.LA8E2  DEFWORD "STA,"
        EQUW    LA8D6
        EQUW    DOMOP
        EQUB    $80,$6C,$1C

;       ASL,

.LA8EE  DEFWORD "ASL,"
        EQUW    LA8E2
        EQUW    DOMOP
        EQUB    1,$D,$D

;       DEC,

.LA8FA  DEFWORD "DEC,"
        EQUW    LA8EE
        EQUW    DOMOP
        EQUB    $C1,$C,$C

;       INC,

.LA906  DEFWORD "INC,"
        EQUW    LA8FA
        EQUW    DOMOP
        EQUB    $E1,$C,$C

;       LSR,

.LA912  DEFWORD "LSR,"
        EQUW    LA906
        EQUW    DOMOP
        EQUB    $41,$D,$D

;       ROL,

.LA91E  DEFWORD "ROL,"
        EQUW    LA912
        EQUW    DOMOP
        EQUB    $21,$D,$D

;       ROR,

.LA92A  DEFWORD "ROR,"
        EQUW    LA91E
        EQUW    DOMOP
        EQUB    $61,$D,$D

;       STX,

.LA936  DEFWORD "STX,"
        EQUW    LA92A
        EQUW    DOMOP
        EQUB    $81,$14,4

;       CPX,

.LA942  DEFWORD "CPX,"
        EQUW    LA936
        EQUW    DOMOP
        EQUB    $E0,$86,4

;       CPY,

.LA94E  DEFWORD "CPY,"
        EQUW    LA942
        EQUW    DOMOP
        EQUB    $C0,$86,4

;       LDX,

.LA95A  DEFWORD "LDX,"
        EQUW    LA94E
        EQUW    DOMOP
        EQUB    $A2,$96,$14

;       LDY,

.LA966  DEFWORD "LDY,"
        EQUW    LA95A
        EQUW    DOMOP
        EQUB    $A0,$8E,$C

;       STY,

.LA972  DEFWORD "STY,"
        EQUW    LA966
        EQUW    DOMOP
        EQUB    $80,$8C,4

;       JSR,

.LA97E  DEFWORD "JSR,"
        EQUW    LA972
        EQUW    DOMOP
        EQUB    $14,$80,4

;       JMP,

.LA98A  DEFWORD "JMP,"
        EQUW    LA97E
.JMPP   EQUW    DOMOP
        EQUB    $40,$80,$84

;       BIT,

.LA996  DEFWORD "BIT,"
        EQUW    LA98A
        EQUW    DOMOP
        EQUB    $20,$84,4

;       >BRANGE

.LA9A2  DEFWORD ">BRANGE"
        EQUW    LA996
.LA9AC  EQUW    DOCOLON
        EQUW    DUP
        EQUW    ZEROLESS
        EQUW    OVER
        EQUW    LIT,$7F
        EQUW    GREATERTHAN
        EQUW    OR
        EQUW    LIT,13
        EQUW    QUERYERROR
        EQUW    EXIT

;       <BRANGE

.LA9C4  DEFWORD "<BRANGE"
        EQUW    LA9A2
.LA9CE  EQUW    DOCOLON
        EQUW    DUP
        EQUW    ZEROGREATER
        EQUW    OVER
        EQUW    LIT,$FF80
        EQUW    LESS
        EQUW    OR
        EQUW    LIT,13
        EQUW    QUERYERROR
        EQUW    EXIT

;       0<

.LA9E6  DEFWORD "0<"
        EQUW    LA9C4
.LA9EB  EQUW    DOCONSTANT
        EQUW    $10

;       VS

.LA9EF  DEFWORD "VS"
        EQUW    LA9E6
.LA9F4  EQUW    DOCONSTANT
        EQUW    $50

;       CS

.LA9F8  DEFWORD "CS"
        EQUW    LA9EF
.LA9FD  EQUW    DOCONSTANT
        EQUW    $90

;       0=

.LAA01  DEFWORD "0="
        EQUW    LA9F8
.LAA06  EQUW    DOCONSTANT
        EQUW    $D0

;       NOT

.LAA0A  DEFWORD "NOT"
        EQUW    LAA01
.LAA10  EQUW    DOCOLON
        EQUW    LIT,$20
        EQUW    PLUS
        EQUW    EXIT

;       IF,

.LAA1A  DEFWORD "IF,"
        EQUW    LAA0A
.AIF    EQUW    DOCOLON
        EQUW    CCOMMA
        EQUW    HERE
        EQUW    ZERO
        EQUW    CCOMMA
        EQUW    TWO
        EQUW    EXIT

;       THEN,

.LAA2E  DEFWORD "THEN,"
        EQUW    LAA1A
.ATHEN  EQUW    DOCOLON
        EQUW    QUERYEXEC
        EQUW    TWO
        EQUW    QUERYPAIRS
        EQUW    HERE
        EQUW    OVER
        EQUW    CFETCH
        EQUW    ZEROBRANCH,$A
        EQUW    SWAP
        EQUW    STORE
        EQUW    BRANCH,$E
        EQUW    OVER
        EQUW    ONEPLUS
        EQUW    MINUS
        EQUW    LA9AC
        EQUW    SWAP
        EQUW    CSTORE
        EQUW    EXIT

;       ELSE,

.LAA5E  DEFWORD "ELSE,"
        EQUW    LAA2E
.AELSE  EQUW    DOCOLON
        EQUW    TWO
        EQUW    QUERYPAIRS
        EQUW    HERE
        EQUW    ONEPLUS
        EQUW    ONE
        EQUW    JMPP
        EQUW    SWAP
        EQUW    HERE
        EQUW    OVER
        EQUW    ONEPLUS
        EQUW    MINUS
        EQUW    LA9AC
        EQUW    SWAP
        EQUW    CSTORE
        EQUW    TWO
        EQUW    EXIT

;       BEGIN,

.LAA88  DEFWORD "BEGIN,"
        EQUW    LAA5E
.ABEGIN EQUW    DOCOLON
        EQUW    HERE
        EQUW    ONE
        EQUW    EXIT

;       UNTIL,

.LAA99  DEFWORD "UNTIL,"
        EQUW    LAA88
.AUNTIL EQUW    DOCOLON
        EQUW    QUERYEXEC
        EQUW    SWAP
        EQUW    ONE
        EQUW    QUERYPAIRS
        EQUW    CCOMMA
        EQUW    HERE
        EQUW    ONEPLUS
        EQUW    MINUS
        EQUW    LA9CE
        EQUW    CCOMMA
        EQUW    EXIT

;       AGAIN,

.LAABA  DEFWORD "AGAIN,"
        EQUW    LAA99
.AAGAIN EQUW    DOCOLON
        EQUW    QUERYEXEC
        EQUW    ONE
        EQUW    QUERYPAIRS
        EQUW    JMPP
        EQUW    EXIT

;       WHILE,

.LAACF  DEFWORD "WHILE,"
        EQUW    LAABA
.AWHIL  EQUW    DOCOLON
        EQUW    OVER
        EQUW    ONE
        EQUW    QUERYPAIRS
        EQUW    AIF
        EQUW    EXIT

;       REPEAT,

.LAAE4  DEFWORD "REPEAT,"
        EQUW    LAACF
.AREPEA EQUW    DOCOLON
        EQUW    QUERYEXEC
        EQUW    TWO
        EQUW    QUERYPAIRS
        EQUW    HERE
        EQUW    TWOPLUS
        EQUW    OVER
        EQUW    MINUS
        EQUW    LA9AC
        EQUW    SWAP
        EQUW    CSTORE
        EQUW    AAGAIN
        EQUW    EXIT

;       MACRO

.LAB08  DEFWORD "MACRO"
        EQUW    LAAE4
.MACRO  EQUW    DOCOLON
        EQUW    LIT,ASSEMBLER+2
        EQUW    QCURR
        EQUW    COLON
        EQUW    EXIT

;       END-CODE

.ENDCODE_NFA
        DEFIMM  "END-CODE"
        EQUW    LAB08
        EQUW    DOCOLON
        EQUW    CURRENT
        EQUW    FETCH
        EQUW    CONTEXT
        EQUW    STORE
        EQUW    QUERYEXEC
        EQUW    QUERYCSP
        EQUW    SMUDG
        EQUW    EXIT

;       CODE

.LAB39  DEFIMM  "CODE"
        EQUW    LA52C
.CODE   EQUW    DOCOLON
        EQUW    QUERYEXEC
        EQUW    CREATE
        EQUW    HERE
        EQUW    HERE
        EQUW    TWOMINUS
        EQUW    STORE
        EQUW    SMUDG
        EQUW    ASSEMBLER
        EQUW    MEM
        EQUW    STORECSP
        EQUW    EXIT

;       LOCATE

.LAB58  DEFWORD "LOCATE"
        EQUW    EDITOR_NFA-REL
.LOCATE EQUW    DOCOLON
        EQUW    QUERYFILE
        EQUW    LIT,4
        EQUW    LESS
        EQUW    ZEROBRANCH,$28
        EQUW    ONEMINUS
        EQUW    HIADDR
        EQUW    MINUSONE
        EQUW    EQUAL
        EQUW    ZEROBRANCH,$A
        EQUW    LIT,$8000
        EQUW    BRANCH,4
        EQUW    HERE
        EQUW    SWAP
        EQUW    ONE
        EQUW    RSLASHW
        EQUW    LIT,7
        EQUW    TOVDU
        EQUW    BRANCH,4
        EQUW    DROP
        EQUW    EXIT

;       SAVE

.LAB99  DEFWORD "SAVE"
        EQUW    LAB58
.SAVE   EQUW    DOCOLON
        EQUW    SCR
        EQUW    FETCH
        EQUW    LIST
        EQUW    CR
        EQUW    BRACKETDOTQUOTE
        EQUB    4,"OK? "
        EQUW    KEY
        EQUW    LIT,$59
        EQUW    EQUAL
        EQUW    ZEROBRANCH,$A
        EQUW    BRACKETUPDATE
        EQUW    SAVBUF
        EQUW    BRANCH,4
        EQUW    QUIT
        EQUW    EXIT

;       CLRSCR

.LABC9  DEFWORD "CLRSCR"
        EQUW    LAB99
.CLRSCR EQUW    DOCOLON
        EQUW    SCR
        EQUW    FETCH
        EQUW    DUP
        EQUW    LIT,$8000
        EQUW    AND
        EQUW    LIT,6
        EQUW    QUERYERROR
        EQUW    DUP
        EQUW    PREV
        EQUW    FETCH
        EQUW    DUP
        EQUW    BUFSZ
        EQUW    TWOMINUS
        EQUW    TWODUP
        EQUW    PLUS
        EQUW    ZERO
        EQUW    SWAP
        EQUW    STORE
        EQUW    BLANKS
        EQUW    STORE
        EQUW    LIST
        EQUW    EXIT

;       PROGRAM

.LAC04  DEFWORD "PROGRAM"
        EQUW    LABC9
        EQUW    DOCOLON
        EQUW    CR
        EQUW    BRACKETDOTQUOTE
        EQUB    19,"1st screen number? "
        EQUW    QUERY
        EQUW    INTERPRET
        EQUW    SCR
        EQUW    STORE
        EQUW    CLRSCR
        EQUW    EDITOR
        EQUW    EXIT

;       ANOTHER

.LAC36  DEFWORD "ANOTHER"
        EQUW    LAC04
.ANOTHR EQUW    DOCOLON
        EQUW    ONE
        EQUW    SCR
        EQUW    PLUSSTORE
        EQUW    CLRSCR
        EQUW    EXIT

;       MORE

.LAC4C  DEFWORD "MORE"
        EQUW    LAC36
.MORE   EQUW    DOCOLON
        EQUW    SAVE
        EQUW    ANOTHR
        EQUW    EXIT

;       TEXT

.LAC5B  DEFWORD "TEXT"
        EQUW    LAC4C
.TEXT   EQUW    DOCOLON
        EQUW    PAD
        EQUW    CSLASHL
        EQUW    TWOPLUS
        EQUW    BLANKS
        EQUW    PWORD
        EQUW    CSLASHL
        EQUW    MIN
        EQUW    PAD
        EQUW    CSTORE
        EQUW    PAD
        EQUW    COUNT
        EQUW    CMOVE
        EQUW    EXIT

;       LINE

.LAC7E  DEFWORD "LINE"
        EQUW    LAC5B
.LINE   EQUW    DOCOLON
        EQUW    DUP
        EQUW    LIT,$FFF0
        EQUW    AND
        EQUW    LIT,23
        EQUW    QUERYERROR
        EQUW    SCR
        EQUW    FETCH
        EQUW    PLINE
        EQUW    DROP
        EQUW    EXIT

;       WHERE

.LAC9F  DEFWORD "WHERE"
        EQUW    LAC7E
.WHERE  EQUW    DOCOLON
        EQUW    DUP
        EQUW    BSLASHSCR
        EQUW    SLASH
        EQUW    DUP
        EQUW    SCR
        EQUW    STORE
        EQUW    BRACKETDOTQUOTE
        EQUB    6,"SCR # "
        EQUW    DECDOT
        EQUW    SWAP
        EQUW    CSLASHL
        EQUW    SLASHMOD
        EQUW    CSLASHL
        EQUW    STAR
        EQUW    ROT
        EQUW    BLOCK
        EQUW    PLUS
        EQUW    CR
        EQUW    CSLASHL
        EQUW    TYPE
        EQUW    CR
        EQUW    WBFR
        EQUW    CFETCH
        EQUW    MINUS
        EQUW    SPACES
        EQUW    LIT,$5E
        EQUW    EMIT
        EQUW    EDITOR
        EQUW    SPSTORE
        EQUW    QUIT
        EQUW    EXIT

;       #LOCATE

.LACEE  DEFWORD "#LOCATE"
        EQUW    FORTH+2
.NLOCAT EQUW    DOCOLON
        EQUW    RSHARP
        EQUW    FETCH
        EQUW    CSLASHL
        EQUW    SLASHMOD
        EQUW    EXIT

;       #LEAD

.SHARPLEAD_NFA
        DEFWORD "#LEAD"
        EQUW    LACEE
.SHARPLEAD
        EQUW    DOCOLON
        EQUW    NLOCAT
        EQUW    LINE
        EQUW    SWAP
        EQUW    EXIT

;       #LAG

.SHARPLAG_NFA
        DEFWORD "#LAG"
        EQUW    SHARPLEAD_NFA
.SHARPLAG
        EQUW    DOCOLON
        EQUW    SHARPLEAD
        EQUW    DUP
        EQUW    TOR
        EQUW    PLUS
        EQUW    CSLASHL
        EQUW    RFROM
        EQUW    MINUS
        EQUW    EXIT

;       -MOVE

.LAD2F  DEFWORD "-MOVE"
        EQUW    SHARPLAG_NFA
.DMOVE  EQUW    DOCOLON
        EQUW    LINE
        EQUW    CSLASHL
        EQUW    CMOVE
        EQUW    UPDATE
        EQUW    EXIT

;       H

.LAD43  DEFWORD "H"
        EQUW    LAD2F
.HH     EQUW    DOCOLON
        EQUW    LINE
        EQUW    PAD
        EQUW    ONEPLUS
        EQUW    CSLASHL
        EQUW    DUP
        EQUW    PAD
        EQUW    CSTORE
        EQUW    CMOVE
        EQUW    EXIT

;       E

.LAD5B  DEFWORD "E"
        EQUW    LAD43
.EE     EQUW    DOCOLON
        EQUW    LINE
        EQUW    CSLASHL
        EQUW    BLANKS
        EQUW    UPDATE
        EQUW    EXIT

;       S

.LAD6B  DEFWORD "S"
        EQUW    LAD5B
.S      EQUW    DOCOLON
        EQUW    DUP
        EQUW    LIT,$FFF0
        EQUW    AND
        EQUW    NOT
        EQUW    ZEROBRANCH,26
        EQUW    DUP
        EQUW    LIT,$E
        EQUW    XDO
        EQUW    IDO
        EQUW    LINE
        EQUW    IDO
        EQUW    ONEPLUS
        EQUW    DMOVE
        EQUW    MINUSONE
        EQUW    BRACKETPLUSLOOP,-14
        EQUW    EE
        EQUW    EXIT

;       D

.LAD9B  DEFWORD "D"
        EQUW    LAD6B
.DD     EQUW    DOCOLON
        EQUW    DUP
        EQUW    HH
        EQUW    DUP
        EQUW    LIT,$F
        EQUW    MINUS
        EQUW    ZEROBRANCH,$1A
        EQUW    LIT,$F
        EQUW    DUP
        EQUW    ROT
        EQUW    XDO
        EQUW    IDO
        EQUW    ONEPLUS
        EQUW    LINE
        EQUW    IDO
        EQUW    DMOVE
        EQUW    BRACKETLOOP,$FFF4
        EQUW    EE
        EQUW    EXIT

;       M

.LADCD  DEFWORD "M"
        EQUW    LAD9B
.EMM    EQUW    DOCOLON
        EQUW    RSHARP
        EQUW    PLUSSTORE
        EQUW    CR
        EQUW    SPACE
        EQUW    SHARPLEAD
        EQUW    TYPE
        EQUW    LIT,$7C
        EQUW    EMIT
        EQUW    SHARPLAG
        EQUW    TYPE
        EQUW    NLOCAT
        EQUW    DOT
        EQUW    DROP
        EQUW    EXIT

;       T

.LADF1  DEFWORD "T"
        EQUW    LADCD
.TT     EQUW    DOCOLON
        EQUW    DUP
        EQUW    CSLASHL
        EQUW    STAR
        EQUW    RSHARP
        EQUW    STORE
        EQUW    HH
        EQUW    ZERO
        EQUW    EMM
        EQUW    EXIT

;       L

.LAE09  DEFWORD "L"
        EQUW    LADF1
.LL     EQUW    DOCOLON
        EQUW    SCR
        EQUW    FETCH
        EQUW    LIST
        EQUW    ZERO
        EQUW    EMM
        EQUW    EXIT

;       R

.LAE1B  DEFWORD "R"
        EQUW    LAE09
.R      EQUW    DOCOLON
        EQUW    PAD
        EQUW    ONEPLUS
        EQUW    SWAP
        EQUW    DMOVE
        EQUW    EXIT

;       P

.LAE2B  DEFWORD "P"
        EQUW    LAE1B
.PP     EQUW    DOCOLON
        EQUW    ONE
        EQUW    TEXT
        EQUW    R
        EQUW    EXIT

.I_NFA  DEFWORD "I"
        EQUW    LAE2B
.I      EQUW    DOCOLON
        EQUW    DUP
        EQUW    S
        EQUW    R
        EQUW    EXIT

;       TOP

.TOP_NFA
        DEFWORD "TOP"
        EQUW    I_NFA
.TOP    EQUW    DOCOLON
        EQUW    ZERO
        EQUW    RSHARP
        EQUW    STORE
        EQUW    EXIT

;       CLEAR

.LAE57  DEFWORD "CLEAR"
        EQUW    TOP_NFA
.CLEAR  EQUW    DOCOLON
        EQUW    SCR
        EQUW    STORE
        EQUW    LIT,16
        EQUW    ZERO
        EQUW    XDO
        EQUW    IDO
        EQUW    EE
        EQUW    BRACKETLOOP,$FFFA
        EQUW    EXIT

;       COPY

.LAE77  DEFWORD "COPY"
        EQUW    LAE57
.COPY   EQUW    DOCOLON
        EQUW    BSLASHSCR
        EQUW    STAR
        EQUW    OFFSET
        EQUW    FETCH
        EQUW    PLUS
        EQUW    SWAP
        EQUW    BSLASHSCR
        EQUW    STAR
        EQUW    BSLASHSCR
        EQUW    OVER
        EQUW    PLUS
        EQUW    SWAP
        EQUW    XDO
        EQUW    DUP
        EQUW    IDO
        EQUW    BLOCK
        EQUW    TWOMINUS
        EQUW    STORE
        EQUW    ONEPLUS
        EQUW    UPDATE
        EQUW    BRACKETLOOP,$FFF0
        EQUW    DROP
        EQUW    SAVBUF
        EQUW    EXIT

;       MATCH

.LAEB2  DEFWORD "MATCH"
        EQUW    LAE77
.MATCH  EQUW    *+2
        LDA     #4
        JSR     SETUP
        DEX
        DEX
        DEX
        DEX
        STY     0,X
        STY     1,X
.LAEC9  LDY     #$FF
.LAECB  INY
        CPY     N
        BCS     LAEFC
        LDA     (N+2),Y
        CMP     ($66),Y
        BEQ     LAECB
        INC     $66
        BNE     LAEDC
        INC     $67
.LAEDC  INC     0,X
        BNE     LAEE2
        INC     1,X
.LAEE2  LDA     N+4
        BNE     LAEE8
        DEC     N+5
.LAEE8  DEC     N+4
        LDA     N+4
        CMP     N
        LDA     N+5
        SBC     N+1
        BCS     LAEC9
        LDA     #0
        STA     2,X
        STA     3,X
        LDY     N+4
.LAEFC  CLC
        TYA
        ADC     0,X
        PHA
        LDA     #0
        ADC     1,X
        JMP     PUT

;       1LINE

.LAF08  DEFWORD "1LINE"
        EQUW    LAEB2
.ONELINE
        EQUW    DOCOLON
        EQUW    SHARPLAG
        EQUW    PAD
        EQUW    COUNT
        EQUW    MATCH
        EQUW    RSHARP
        EQUW    PLUSSTORE
        EQUW    EXIT

;       $FIND

.DOLLARFIND_NFA         DEFWORD "$FIND"
        EQUW    LAF08
.DOLLARFIND
        EQUW    DOCOLON
        EQUW    LIT,$3FF
        EQUW    RSHARP
        EQUW    FETCH
        EQUW    LESS
        EQUW    ZEROBRANCH,$12
        EQUW    TOP
        EQUW    PAD
        EQUW    HERE
        EQUW    CSLASHL
        EQUW    ONEPLUS
        EQUW    CMOVE
        EQUW    ZERO
        EQUW    ERROR
        EQUW    ONELINE
        EQUW    ZEROBRANCH,-34
        EQUW    EXIT

;       DELETE

.DELETE_NFA
        DEFWORD "DELETE"
        EQUW    DOLLARFIND_NFA
.DELETE EQUW    DOCOLON
        EQUW    TOR
        EQUW    SHARPLAG
        EQUW    PLUS
        EQUW    RFETCH
        EQUW    MINUS
        EQUW    SHARPLAG
        EQUW    RFETCH
        EQUW    NEGATE
        EQUW    RSHARP
        EQUW    PLUSSTORE
        EQUW    SHARPLEAD
        EQUW    PLUS
        EQUW    SWAP
        EQUW    CMOVE
        EQUW    RFROM
        EQUW    BLANKS
        EQUW    UPDATE
        EQUW    EXIT

;       N

.LAF7F  DEFWORD "N"
        EQUW    DELETE_NFA
.NN     EQUW    DOCOLON
        EQUW    DOLLARFIND
        EQUW    ZERO
        EQUW    EMM
        EQUW    EXIT

;       F

.LAF8D  DEFWORD "F"
        EQUW    LAF7F
.FF     EQUW    DOCOLON
        EQUW    ONE
        EQUW    TEXT
        EQUW    NN
        EQUW    EXIT

;       B

.LAF9B  DEFWORD "B"
        EQUW    LAF8D
.BB     EQUW    DOCOLON
        EQUW    PAD
        EQUW    CFETCH
        EQUW    NEGATE
        EQUW    EMM
        EQUW    EXIT

;       X

.LAFAB  DEFWORD "X"
        EQUW    LAF9B
.XX     EQUW    DOCOLON
        EQUW    ONE
        EQUW    TEXT
        EQUW    DOLLARFIND
        EQUW    PAD
        EQUW    CFETCH
        EQUW    DELETE
        EQUW    ZERO
        EQUW    EMM
        EQUW    EXIT

;       TILL

.LAFC3  DEFWORD "TILL"
        EQUW    LAFAB
.TILL   EQUW    DOCOLON
        EQUW    SHARPLEAD
        EQUW    PLUS
        EQUW    ONE
        EQUW    TEXT
        EQUW    ONELINE
        EQUW    ZEROEQUAL
        EQUW    ZERO
        EQUW    QUERYERROR
        EQUW    SHARPLEAD
        EQUW    PLUS
        EQUW    SWAP
        EQUW    MINUS
        EQUW    DELETE
        EQUW    ZERO
        EQUW    EMM
        EQUW    EXIT

;       C

.C_NFA  DEFWORD "C"
        EQUW    LAFC3
        EQUW    DOCOLON
        EQUW    ONE
        EQUW    TEXT
        EQUW    PAD
        EQUW    COUNT
        EQUW    SHARPLAG
        EQUW    ROT
        EQUW    OVER
        EQUW    MIN
        EQUW    TOR
        EQUW    RFETCH
        EQUW    RSHARP
        EQUW    PLUSSTORE
        EQUW    RFETCH
        EQUW    MINUS
        EQUW    TOR
        EQUW    DUP
        EQUW    WBFR
        EQUW    RFETCH
        EQUW    CMOVE
        EQUW    WBFR
        EQUW    SHARPLEAD
        EQUW    PLUS
        EQUW    RFROM
        EQUW    CMOVE
        EQUW    RFROM
        EQUW    CMOVE
        EQUW    UPDATE
        EQUW    ZERO
        EQUW    EMM
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       <CMOVE   ( from\to\count ... )
;
;       > Its overall action is the same as that of CMOVE except that the byte
;       > with the highest address is moved first and the transfer proceeds in
;       > order of decreasing address.
;
; -----------------------------------------------------------------------------

.LB02E  DEFWORD "<CMOVE"
        EQUW    LAC9F
.CMOVU  EQUW    *+2
        LDA     #3
        JSR     SETUP
        LDA     N+1
        BMI     LB068
        CLC
        ADC     N+3
        STA     N+3
        LDA     N+1
        CLC
        ADC     N+5
        STA     N+5
        LDY     N

.LB050  TYA
        BNE     LB060
        LDA     N+1
        BNE     LB05A
        JMP     NEXT

.LB05A  DEC     N+1
        DEC     N+3
        DEC     N+5
.LB060  DEY
        LDA     (N+4),Y
        STA     (N+2),Y
        JMP     LB050

.LB068  JMP     NEXT


; -----------------------------------------------------------------------------
;
;       MOVE-BUFFERS   ( modeaddr ... )
;
;       Given the display memory address as retrieved by MODEADDR, moves the
;       mass storage buffer area FIRST to its new location and upates PREV and
;       USE accordingly.
;
;       : MOVE-BUFFERS
;        LIMIT   2DUP -          ( calculate distance between MODEADDR-LIMIT )
;        ?DUP IF                 ( check if there is a non-zero distance )
;         FIRST OVER +           ( calculate new FIRST value to-be )
;         DUP HERE U< 25 ?ERROR  ( system memory clash if below HERE )
;         ROT FIRST -            ( calculate count of bytes to move )
;         >R  SWAP
;         DUP PREV +!            ( add distance to PREV ) 
;         DUP USE +!             ( add distance to USE  )
;         >R  FIRST              ( retrieve old FIRST )
;         OVER [ ' FIRST ] !     ( update FIRST with calculated value )
;         SWAP  R> R>  SWAP
;         0< IF                  ( check if distance is negative )
;          CMOVE                 ( move FIRST, first byte first )
;         ELSE
;          <CMOVE                ( move FIRST, last byte first )
;         THEN
;         [ ' LIMIT ] !          ( store MODEADDR as new LIMIT )
;        ELSE
;         2DROP                  ( no distance, nothing to do )
;        THEN
;       ;
;
; -----------------------------------------------------------------------------

.LB06B  DEFWORD "MOVE-BUFFERS"
        EQUW    LB02E
.MOVBUF EQUW    DOCOLON
        EQUW    LIMIT
        EQUW    TWODUP
        EQUW    MINUS
        EQUW    QUERYDUP
        EQUW    ZEROBRANCH,$56
        EQUW    FIRST
        EQUW    OVER
        EQUW    PLUS
        EQUW    DUP
        EQUW    HERE
        EQUW    ULESS
        EQUW    LIT,25
        EQUW    QUERYERROR
        EQUW    ROT
        EQUW    FIRST
        EQUW    MINUS
        EQUW    TOR
        EQUW    SWAP
        EQUW    DUP
        EQUW    PREV
        EQUW    PLUSSTORE
        EQUW    DUP
        EQUW    USE
        EQUW    PLUSSTORE
        EQUW    TOR
        EQUW    FIRST
        EQUW    OVER
        EQUW    LIT,FIRST+2
        EQUW    STORE
        EQUW    SWAP
        EQUW    RFROM
        EQUW    RFROM
        EQUW    SWAP
        EQUW    ZEROLESS
        EQUW    ZEROBRANCH,8
        EQUW    CMOVE
        EQUW    BRANCH,4
        EQUW    CMOVU
        EQUW    LIT,LIMIT+2
        EQUW    STORE
        EQUW    BRANCH,4
        EQUW    TWODROP
        EQUW    EXIT

;       PLOT

.LB0E0  DEFWORD "PLOT"
        EQUW    LB06B
.PLOT   EQUW    DOCOLON
        EQUW    LIT,$19
        EQUW    TOVDU
        EQUW    SWAP
        EQUW    ROT
        EQUW    TOVDU
        EQUW    DUP
        EQUW    TOVDU
        EQUW    BYTESWAP
        EQUW    TOVDU
        EQUW    DUP
        EQUW    TOVDU
        EQUW    BYTESWAP
        EQUW    TOVDU
        EQUW    EXIT

TOPNFA  =       *

; -----------------------------------------------------------------------------
;
;       $MSG   ( n ... )
;
;       > See MESSAGE .
;
; -----------------------------------------------------------------------------

.LB107  DEFWORD "$MSG"
        EQUW    LB0E0
.DOLLARMSG
        EQUW    DOCOLON
        EQUW    DUP
        EQUW    DUP
        EQUW    ZEROGREATER
        EQUW    SWAP
        EQUW    LIT,26
        EQUW    LESS
        EQUW    AND
        EQUW    ZEROBRANCH,$20
        EQUW    LIT,LB47E
        EQUW    SWAP
        EQUW    ZERO
        EQUW    XDO
        EQUW    DUP
        EQUW    CFETCH
        EQUW    PLUS
        EQUW    ONEPLUS
        EQUW    BRACKETLOOP,$FFF6
        EQUW    COUNT
        EQUW    TYPE
        EQUW    BRANCH,4
        EQUW    MSGNUM
        EQUW    EXIT

; -----------------------------------------------------------------------------
;
;       The following words are present in the original ROM but not accessible
;       because the FORTH vocabulary ends with $MSG above.
;
; -----------------------------------------------------------------------------

IF NOT(STRIP)

;       MM

.LB146  DEFWORD "MM"
        EQUW    LB107
.MM     EQUW    DOCOLON
        EQUW    TWOPLUS
        EQUW    SWAP
        EQUW    DUP
        EQUW    ROT
        EQUW    DUP
        EQUW    ROT
        EQUW    OVER
        EQUW    CFETCH
        EQUW    ONEPLUS
        EQUW    CMOVE
        EQUW    CFETCH
        EQUW    ONEPLUS
        EQUW    PLUS
        EQUW    EXIT

;       M1

.LB169  DEFWORD "M1"
        EQUW    LB146
.M1     EQUW    DOCOLON
        EQUW    BRACKETDOTQUOTE
        EQUB    11,"Stack empty"
        EQUW    EXIT

;       M2

.LB180  DEFWORD "M2"
        EQUW    LB169
.M2     EQUW    DOCOLON
        EQUW    BRACKETDOTQUOTE
        EQUB    15,"Dictionary full"
        EQUW    EXIT

;       M3

.LB19B  DEFWORD "M3"
        EQUW    LB180
.M3     EQUW    DOCOLON
        EQUW    BRACKETDOTQUOTE
        EQUB    26,"Has incorrect address mode"
        EQUW    EXIT

;       M4

.LB1C1  DEFWORD "M4"
        EQUW    LB19B
.M4     EQUW    DOCOLON
        EQUW    BRACKETDOTQUOTE
        EQUB    12,"Isn't unique"
        EQUW    EXIT

;       M5

.LB1D9  DEFWORD "M5"
        EQUW    LB1C1
.M5     EQUW    DOCOLON
        EQUW    BRACKETDOTQUOTE
        EQUB    29,"Parameter outside valid range"
        EQUW    EXIT

;       M6

.LB202  DEFWORD "M6"
        EQUW    LB1D9
.M6     EQUW    DOCOLON
        EQUW    BRACKETDOTQUOTE
        EQUB    26,"Screen number out of range"
        EQUW    EXIT

;       M7

.LB228  DEFWORD "M7"
        EQUW    LB202
.M7     EQUW    DOCOLON
        EQUW    BRACKETDOTQUOTE
        EQUB    10,"Stack full"
        EQUW    EXIT

;       M8

.LB23E  DEFWORD "M8"
        EQUW    LB228
.M8     EQUW    DOCOLON
        EQUW    BRACKETDOTQUOTE
        EQUB    25,"Can't open or extend file"
        EQUW    EXIT

;       M9

.LB263  DEFWORD "M9"
        EQUW    LB23E
.M9     EQUW    DOCOLON
        EQUW    BRACKETDOTQUOTE
        EQUB    24,"Read/Write not completed"
        EQUW    EXIT

;       M10

.LB287  DEFWORD "M10"
        EQUW    LB263
.M10    EQUW    DOCOLON
        EQUW    BRACKETDOTQUOTE
        EQUB    26,"Can't redefine end-of-line"
        EQUW    EXIT

;       M11

.LB2AE  DEFWORD "M11"
        EQUW    LB287
.M11    EQUW    DOCOLON
        EQUW    BRACKETDOTQUOTE
        EQUB    20,"Can't divide by zero"
        EQUW    EXIT

;       M12

.LB2CF  DEFWORD "M12"
        EQUW    LB2AE
.M12    EQUW    DOCOLON
        EQUW    BRACKETDOTQUOTE
        EQUB    26,"Undefined execution vector"
        EQUW    EXIT

;       M13

.LB2F6  DEFWORD "M13"
        EQUW    LB2CF
.M13    EQUW    DOCOLON
        EQUW    BRACKETDOTQUOTE
        EQUB    15,"Branch too long"
        EQUW    EXIT

;       M14

.LB312  DEFWORD "M14"
        EQUW    LB2F6
.M14    EQUW    DOCOLON
        EQUW    BRACKETDOTQUOTE
        EQUB    28,"Incorrect CURRENT vocabulary"
        EQUW    EXIT

;       M15

.LB33B  DEFWORD "M15"
        EQUW    LB312
.M15    EQUW    DOCOLON
        EQUW    BRACKETDOTQUOTE
        EQUB    1,' '
        EQUW    EXIT

;       M16

.LB349  DEFWORD "M16"
        EQUW    LB33B
.M16    EQUW    DOCOLON
        EQUW    BRACKETDOTQUOTE
        EQUB    1,' '
        EQUW    EXIT

;       M17

.LB357  DEFWORD "M17"
        EQUW    LB349
.M17    EQUW    DOCOLON
        EQUW    BRACKETDOTQUOTE
        EQUB    16,"Compilation only"
        EQUW    EXIT

;       M18

.LB374  DEFWORD "M18"
        EQUW    LB357
.M18    EQUW    DOCOLON
        EQUW    BRACKETDOTQUOTE
        EQUB    14,"Execution only"
        EQUW    EXIT

;       M19

.LB38F  DEFWORD "M19"
        EQUW    LB374
.M19    EQUW    DOCOLON
        EQUW    BRACKETDOTQUOTE
        EQUB    23,"Conditionals not paired"
        EQUW    EXIT

;       M20

.LB3B3  DEFWORD "M20"
        EQUW    LB38F
.M20    EQUW    DOCOLON
        EQUW    BRACKETDOTQUOTE
        EQUB    23,"Definition not finished"
        EQUW    EXIT

;       M21

.LB3D7  DEFWORD "M21"
        EQUW    LB3B3
.M21    EQUW    DOCOLON
        EQUW    BRACKETDOTQUOTE
        EQUB    23,"In protected dictionary"
        EQUW    EXIT

;       M22

.LB3FB  DEFWORD "M22"
        EQUW    LB3D7
.M22    EQUW    DOCOLON
        EQUW    BRACKETDOTQUOTE
        EQUB    21,"Use only when LOADing"
        EQUW    EXIT

;       M23

.LB41D  DEFWORD "M23"
        EQUW    LB3FB
.M23    EQUW    DOCOLON
        EQUW    BRACKETDOTQUOTE
        EQUB    26,"Off current editing screen"
        EQUW    EXIT

;       M24

.LB444  DEFWORD "M24"
        EQUW    LB41D
.M24    EQUW    DOCOLON
        EQUW    BRACKETDOTQUOTE
        EQUB    25,"NOT in CURRENT vocabulary"
        EQUW    EXIT

;       M25

.LB46A  DEFWORD "M25"
        EQUW    LB444
.M25    EQUW    DOCOLON
        EQUW    BRACKETDOTQUOTE
        EQUB    7,"No room"
        EQUW    EXIT

ENDIF

.LB47E  EQUW    1
        EQUB    11,"Stack empty"
        EQUB    15,"Dictionary full"
        EQUB    26,"Has incorrect address mode"
        EQUB    12,"Isn't unique"
        EQUB    29,"Parameter outside valid range"
        EQUB    26,"Screen number out of range"
        EQUB    10,"Stack full"
        EQUB    25,"Can't open or extend file"
        EQUB    24,"Read/Write not completed"
        EQUB    26,"Can't redefine end-of-line"
        EQUB    20,"Can't divide by zero"
        EQUB    26,"Undefined execution vector"
        EQUB    15,"Branch too long"
        EQUB    28,"Incorrect CURRENT vocabulary"
        EQUB    1,' '
        EQUB    1,' '
        EQUB    16,"Compilation only"
        EQUB    14,"Execution only"
        EQUB    23,"Conditionals not paired"
        EQUB    23,"Definition not finished"
        EQUB    23,"In protected dictionary"
        EQUB    21,"Use only when LOADing"
        EQUB    26,"Off current editing screen"
        EQUB    25,"NOT in CURRENT vocabulary"
        EQUB    7,"No room"

TOPDP   =       *       ; TOP OF DICTIONARY

FOR n,TOPDP,$BFFF

        EQUB    $FF

NEXT

        PRINT   "ROM bytes free: ",$C000-TOPDP
        SAVE    "forth-assembled.rom",RomStart,*
