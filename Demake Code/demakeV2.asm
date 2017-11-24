  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; VERT mirroring for HORIZ scrolling
  

;;;;;;;;;;;;;;;

;; DECLARE SOME VARIABLES HERE
  .rsset $0000  ;;start variables at ram location 0
  
scroll     .rs 1  ; horizontal scroll count
nametable  .rs 1  ; which nametable to use, 0 or 1
columnLow  .rs 1  ; low byte of new column address
columnHigh .rs 1  ; high byte of new column address
sourceLow  .rs 1  ; source for column data
sourceHigh .rs 1
columnNumber .rs 1  ; which column of level data to draw
isJumping 	 .rs 1

buttons1			.rs 1  ; player 1 controller buttons, one bit per button
playerX				.rs 1  ; Character sprite X position
playerY				.rs 1  ; Character sprite Y position
tempPlayerX			.rs 1  ; Temporary X position for use in sprite positioning
tempPlayerY			.rs 1  ; Temporary Y position for use in sprite positioning
gravity				.rs 1  ; Value for gravity
jumpAmount			.rs 1  ; Value for jump height 
isFalling			.rs 1  ; 0 for falling, 1 for not falling
isGreaterThan		.rs 1  ;
isLessThan			.rs 1  ;
coinsCollected		.rs 1  ; how many coins collected
coin1Active			.rs 1 ; whether coin1 is active
coin2Active			.rs 1 ; whether coin2 is active
coin3Active			.rs 1 ; whether coin3 is active

CONTROLLER_A      = %10000000
CONTROLLER_B      = %01000000
CONTROLLER_SELECT = %00100000
CONTROLLER_START  = %00010000
CONTROLLER_UP     = %00001000
CONTROLLER_DOWN   = %00000100
CONTROLLER_LEFT   = %00000010
CONTROLLER_RIGHT  = %00000001

CHARACTERYATTRIBUTE = $0200  ; Character sprite X position
CHARACTERXATTRIBUTE = $0203  ; Character sprite Y position

;COINS
COIN1Y = $0214
COIN1SPRITE = $0215
COIN1X = $0217

COIN2Y = $0218
COIN2SPRITE = $0219
COIN2X = $021B

COIN3Y = $021C
COIN3SPRITE = $021D
COIN3X = $021F

BLANKSPRITE = $04

; Scores
SCOREPOSITION = $08
SCORESPRITE = $0211
SCORE0SPRITE = $20
SCORE1SPRITE = $21
SCORE2SPRITE = $22
SCORE3SPRITE = $23

; Level borders
RIGHTWALL      = $F4
TOPWALL        = $10
BOTTOMWALL     = $C8
LEFTWALL       = $03


; Platform 0
P0TOP		   = $A8
P0BOTTOM	   = $AF
P0RIGHT		   = $A8
P0LEFT		   = $B8

; Platform 1
P1TOP		   = $9F
P1BOTTOM	   = $A8
P1RIGHT		   = $9F
P1LEFT		   = $AF

; Platform 2
P2TOP		   = $98
P2BOTTOM	   = $9F
P2RIGHT		   = $98
P2LEFT		   = $A8


; Platform 3
P3TOP		   = $8F
P3BOTTOM	   = $98
P3RIGHT		   = $8F
P3LEFT		   = $9F

; Platform 4
P4TOP		   = $88
P4BOTTOM	   = $8F
P4RIGHT		   = $88
P4LEFT		   = $98

; Platform 5
P5TOP		   = $80
P5BOTTOM	   = $88
P5RIGHT		   = $80
P5LEFT		   = $90

; Platform 6
P6TOP		   = $AF
P6BOTTOM	   = $B8
P6RIGHT		   = $AF
P6LEFT		   = $BF

; Platform 7
P7TOP		   = $B8
P7BOTTOM	   = $BF
P7RIGHT		   = $B8
P7LEFT		   = $BF

MAX_GRAVITY    = $03	; The maximum speed at which an object can fall
JUMP_HEIGHT	   = $12	; The height of the character's jump
;;;;;;;;;;;;
    
  .bank 0
  .org $C000 
RESET:
  SEI          ; disable IRQs
  CLD          ; disable decimal mode
  LDX #$40
  STX $4017    ; disable APU frame IRQ
  LDX #$FF
  TXS          ; Set up stack
  INX          ; now X = 0
  STX $2000    ; disable NMI
  STX $2001    ; disable rendering
  STX $4010    ; disable DMC IRQs

vblankwait1:       ; First wait for vblank to make sure PPU is ready
  BIT $2002
  BPL vblankwait1

clrmem:
  LDA #$00
  STA $0000, x
  STA $0100, x
  STA $0300, x
  STA $0400, x
  STA $0500, x
  STA $0600, x
  STA $0700, x
  LDA #$FE
  STA $0200, x
  INX
  BNE clrmem
   
vblankwait2:      ; Second wait for vblank, PPU is ready after this
  BIT $2002
  BPL vblankwait2


LoadPalettes:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$3F
  STA $2006             ; write the high byte of $3F00 address
  LDA #$00
  STA $2006             ; write the low byte of $3F00 address
  LDX #$00              ; start out at 0
LoadPalettesLoop:
  LDA palette, x        ; load data from address (palette + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$20              ; Compare X to hex $10, decimal 16 - copying 16 bytes = 4 sprites
  BNE LoadPalettesLoop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down



LoadSprites:
  LDX #$00              ; start at 0
LoadSpritesLoop:
  LDA sprites, x        ; load data from address (sprites +  x)
  STA $0200, x          ; store into RAM address ($0200 + x)
  INX                   ; X = X + 1
  CPX #$58           ; Compare X to hex $20, decimal 16
  BNE LoadSpritesLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
                        ; if compare was equal to 16, keep going down
              
              
InitializeNametables:
  LDA #$01
  STA nametable
  LDA #$00
  STA scroll
  STA columnNumber
InitializeNametablesLoop:
  JSR DrawNewColumn     ; draw bg column
  LDA scroll            ; go to next column
  CLC
  ADC #$08
  STA scroll
  INC columnNumber
  LDA columnNumber      ; repeat for first nametable 
  CMP #$20
  BNE InitializeNametablesLoop
  
  LDA #$00
  STA nametable
  LDA #$00
  STA scroll
  JSR DrawNewColumn     ; draw first column of second nametable
  INC columnNumber
  
  LDA #$00              ; set back to increment +1 mode
  STA $2000
InitializeNametablesDone:
  

InitializeAttributes:
  LDA #$01
  STA nametable
  LDA #$00
  STA scroll
  STA columnNumber
InitializeAttributesLoop:
  JSR DrawNewAttributes     ; draw attribs
  LDA scroll                ; go to next column
  CLC
  ADC #$20
  STA scroll

  LDA columnNumber      ; repeat for first nametable 
  CLC 
  ADC #$04
  STA columnNumber
  CMP #$20
  BNE InitializeAttributesLoop
  
  LDA #$00
  STA nametable
  LDA #$00
  STA scroll
  JSR DrawNewAttributes     ; draw first column of second nametable
InitializeAttributesDone:

  LDA #$21
  STA columnNumber

              
  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000

  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001
  
SetIntialValues:
  LDA #$70
  STA playerX
  STA playerY
  
  LDA #$01
  STA gravity
  STA isFalling
  STA coin1Active
  STA coin2Active
  STA coin3Active
  
  LDA #$00
  STA jumpAmount
  STA isJumping
  STA coinsCollected
  
  LDA #%10000000   ; enable NMI, sprites from Pattern Table 1
  STA $2000

  LDA #%00010000   ; enable sprites
  STA $2001

Forever:
  JMP Forever     ;jump back to Forever, infinite loop
  
 

NMI:
  INC scroll            ; add one to our scroll variable each frame


NTSwapCheck:
  LDA scroll            ; check if the scroll just wrapped from 255 to 0
  BNE NTSwapCheckDone  
NTSwap:
  LDA nametable         ; load current nametable number (0 or 1)
  EOR #$01              ; exclusive OR of bit 0 will flip that bit
  STA nametable         ; so if nametable was 0, now 1
                        ;    if nametable was 1, now 0
NTSwapCheckDone:


NewAttribCheck:
  LDA scroll
  AND #%00011111            ; check for multiple of 32
  BNE NewAttribCheckDone    ; if low 5 bits = 0, time to write new attribute bytes
  jsr DrawNewAttributes
NewAttribCheckDone:


NewColumnCheck:
  LDA scroll
  AND #%00000111            ; throw away higher bits to check for multiple of 8
  BNE NewColumnCheckDone    ; done if lower bits != 0
  JSR DrawNewColumn         ; if lower bits = 0, time for new column
  
  lda columnNumber
  clc
  adc #$01             ; go to next column
  and #%01111111       ; only 128 columns of data, throw away top bit to wrap
  sta columnNumber
NewColumnCheckDone:


  LDA #$00
  STA $2003       
  LDA #$02
  STA $4014       ; sprite DMA from $0200
  
  ; run other game graphics updating code here

  LDA #$00
  STA $2006        ; clean up PPU address registers
  STA $2006
  
  LDA scroll
  STA $2005        ; write the horizontal scroll count register

  LDA #$00         ; no vertical scrolling
  STA $2005
    
  ;;This is the PPU clean up section, so rendering the next frame starts properly.
  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  ORA nametable    ; select correct nametable for bit 0
  STA $2000
  
  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001
    
  ; run normal game engine code here
    JSR ReadController1 
  
ResetIsFalling:
 LDA #$01			; Loads 01 into A
 STA isFalling		; Resets isFalling to 01 
  
CheckPlatformCollision .macro  ; Platform: top, bottom, left, right
  LDA playerY		; Loads playerY
  CLC
  ADC #$04			; Adds 
  CMP \1			; Compares to argument 1
  BCC .Done\@		; Branch if more than 
  CMP \2			; Compare to argument 2
  BCS .Done\@
  LDA playerX
  CMP \3
  BCS .Done\@
  CMP \4
  BCC .Done\@
  LDA #$00
  STA isFalling		; sets isFalling to 0
  STA isJumping		; sets isJumping to 0
.Done\@
  .endm
  
  ;; Checks the platforms for collisions
  CheckPlatformCollision #P0TOP, #P0BOTTOM, #P0LEFT, #P0RIGHT
  CheckPlatformCollision #P1TOP, #P1BOTTOM, #P1LEFT, #P1RIGHT
  CheckPlatformCollision #P2TOP, #P2BOTTOM, #P2LEFT, #P2RIGHT
  CheckPlatformCollision #P3TOP, #P3BOTTOM, #P3LEFT, #P3RIGHT
  CheckPlatformCollision #P4TOP, #P4BOTTOM, #P4LEFT, #P4RIGHT
  CheckPlatformCollision #P5TOP, #P5BOTTOM, #P5LEFT, #P5RIGHT
  CheckPlatformCollision #P6TOP, #P6BOTTOM, #P6LEFT, #P6RIGHT
  CheckPlatformCollision #P7TOP, #P7BOTTOM, #P7LEFT, #P7RIGHT
  
      
CheckCoinCollision .macro ; COINX COINY COINSPRITE coinActive
  LDA \4
  CMP #$00
  BEQ .Done\@
  LDA \1			; Checks for collision between the player and coins
  SEC
  SBC playerY 
  CLC
  ADC #$04
  BMI .Done\@ 
  SEC
  SBC #$08
  BPL .Done\@
  
  LDA \2
  SEC
  SBC playerX
  CLC
  ADC #$04
  BMI .Done\@ 
  SEC
  SBC #$08
  BPL .Done\@ 
  
  LDA #BLANKSPRITE		; Sets the Coin sprite to  blank after collision
  STA \3
  
  LDA coinsCollected	;Loads coinsCollected
  CLC
  ADC #$01				; Adds 1
  STA coinsCollected  	; Saves to coinsCollected
  LDA #$00
  STA \4
  
  ; Updates the score sprite depending on coins collected
  LDA coinsCollected
  CMP #$00
  BEQ .Done\@
  CMP #$01
  BNE .CheckScoreIsTwo\@
  LDA #SCORE1SPRITE
  STA SCORESPRITE
.CheckScoreIsTwo\@
  CMP #$02
  BNE .CheckScoreIsThree\@
  LDA #SCORE2SPRITE
  STA SCORESPRITE
.CheckScoreIsThree\@  
  CMP #$03
  BNE .Done\@
  LDA #SCORE3SPRITE
  STA SCORESPRITE
.Done\@:
  .endm
  ; Runs the macro on the coins
  CheckCoinCollision COIN1Y, COIN1X, COIN1SPRITE, coin1Active
  CheckCoinCollision COIN2Y, COIN2X, COIN2SPRITE, coin2Active
  CheckCoinCollision COIN3Y, COIN3X, COIN3SPRITE, coin3Active

ReadLeft: 
  LDA buttons1					; player 1 left arrow
  AND #CONTROLLER_LEFT			; only look at bit 0
  BEQ .Done						; branch to .Done if button is not pressed
  LDA CHARACTERXATTRIBUTE		; Loads CHARACTERXATTRIBUTE
  CMP #LEFTWALL					; Compare to LEFTWALL
  BCC .Done						; Branch to Done
  LDA playerX					; load sprite X position
  SEC							; make sure carry flag is set
  SBC #$01						; A = A - 1
  STA playerX					; Save to playerX
.Done:							; Handling this button is done

ReadRight: 
  LDA buttons1 					; player 1 right arrow 
  AND #CONTROLLER_RIGHT  		; only look at bit 0
  BEQ .Done		    			; branch to .Done if button is not pressed
  LDA CHARACTERXATTRIBUTE		; Load CHARACTERXATTRIBUTE
  CMP #RIGHTWALL				; Compare to RIGHTWALL
  BCS .Done						; Branch
  LDA playerX					; load sprite X position
  CLC							; make sure carry flag is set
  ADC #$01						; A = A - 1
  STA playerX					; save to playerX
.Done:							; handling this button is done

ReadA:  ; TODO only allow double jump / cant jump off screen
  LDA #$0						; Loads 0 into A
  STA jumpAmount				; Saves A to jumpAmount
  LDA buttons1   				; Loads Player 1 A button
  AND #CONTROLLER_A 
  BEQ .Done  					; Branch to Done if button not pressed
  LDA isJumping
  CMP #$02
  BEQ .Done
  CLC
  ADC #$01
  STA isJumping
  LDA #JUMP_HEIGHT				; Loads JUMP_HEIGHT
  STA jumpAmount				; Save to jumpAmount
.Done: 						    ; handling this button is done

UpdateGravity:
  LDA isFalling					; Loads isFalling
  CMP #$01
  BNE .Done
  LDA playerY					; Load playerY
  CMP #BOTTOMWALL				; Compare to BOTTOMWALL
  BCS .Done						; Branch if playerY < BOTTOMWALL
  CLC							; Clear carry
  ADC gravity					; Adds the value of gravity to A
  STA playerY					; Saves to playerY
.Done

UpdateIsJumping:
  LDA playerY					; Load playerY
  CMP #BOTTOMWALL				; Compare to BOTTOMWALL
  BCC .Done						; Branch if playerY < BOTTOMWALL
  LDA #$00
  STA isJumping
.Done

IncreaseGravity: 
  LDA gravity 					; Loads gravity
  CMP #MAX_GRAVITY				; Compares to max gravity
  BEQ .Done						; Branch to done if gravity = MAX_GRAVITY
  CLC							; Clear carry
  ADC #$01						; Add 1 to gravity
  STA gravity  					; Save to gravity 
.Done: 

UpdateJump:
  LDA playerY					; Load playerY
  CMP #TOPWALL					; Compare to TOPWALL
  BCC .Done						; Branch is greater than
  SEC							; make sure carry flag is set
  SBC jumpAmount				; Subtract jumpAmount from A
  STA playerY 					; Save to playerY
.Done  
 

UpdateSprites			; Updates Charater's sprites position
 LDA playerY					; Loads playerX
 STA tempPlayerY				; Saves playerX value in tempPlayerX
 LDA playerX					; Loads playerY
 STA tempPlayerX					
 
 LDA tempPlayerY  				; Loads tempPlayerY
 STA $0200						; Save to top left sprite
 STA $0204						; Save  to top right sprite
 CLC							; Clear carry
 ADC #$08						; Add 8 to tempPlayerY
 STA $0208						; Save to bottom left sprite
 STA $020C						; Save to bottom right sprite
 
 LDA tempPlayerX				; Loads tempPlayerX
 STA $0203						; Save to top left sprite
 STA $020B						; Save to bottom left sprite
 CLC							; Clear carry
 ADC #$08						; Add 8 to tempPlayerX
 STA $0207						; Save to top right sprite
 STA $020F						; Save to bottom right sprite

  ; reading from controllers, etc
  
  RTI              ; return from interrupt
 
 ReadController1: 		; Loads controller 1
  LDA #$01
  STA $4016
  LDA #$00
  STA $4016
  LDX #$08
ReadController1Loop:
  LDA $4016
  LSR A            ; bit0 -> Carry
  ROL buttons1     ; bit0 <- Carry
  DEX
  BNE ReadController1Loop
  RTS
 
; Back to scrolling background update
DrawNewColumn:
  LDA scroll       ; calculate new column address using scroll register
  LSR A
  LSR A
  LSR A            ; shift right 3 times = divide by 8
  STA columnLow    ; $00 to $1F, screen is 32 tiles wide

  LDA nametable     ; calculate new column address using current nametable
  EOR #$01          ; invert low bit, A = $00 or $01
  ASL A             ; shift up, A = $00 or $02
  ASL A             ; $00 or $04
  CLC
  ADC #$20          ; add high byte of nametable base address ($2000)
  STA columnHigh    ; now address = $20 or $24 for nametable 0 or 1

  LDA columnNumber  ; column number * 32 = column data offset
  ASL A
  ASL A
  ASL A
  ASL A
  ASL A             
  STA sourceLow
  LDA columnNumber
  LSR A
  LSR A
  LSR A
  STA sourceHigh
  
  LDA sourceLow       ; column data start + offset = address to load column data from
  CLC 
  ADC #LOW(columnData)
  STA sourceLow
  LDA sourceHigh
  ADC #HIGH(columnData)
  STA sourceHigh

DrawColumn:
  LDA #%00000100        ; set to increment +32 mode
  STA $2000
  
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA columnHigh
  STA $2006             ; write the high byte of column address
  LDA columnLow
  STA $2006             ; write the low byte of column address
  LDX #$1E              ; copy 30 bytes
  LDY #$00
DrawColumnLoop:
  LDA [sourceLow], y
  STA $2007
  INY
  DEX
  BNE DrawColumnLoop

  RTS
  
  
  
DrawNewAttributes:
  LDA nametable
  EOR #$01          ; invert low bit, A = $00 or $01
  ASL A             ; shift up, A = $00 or $02
  ASL A             ; $00 or $04
  CLC
  ADC #$23          ; add high byte of attribute base address ($23C0)
  STA columnHigh    ; now address = $23 or $27 for nametable 0 or 1
  
  LDA scroll
  LSR A
  LSR A
  LSR A
  LSR A
  LSR A
  CLC
  ADC #$C0
  STA columnLow     ; attribute base + scroll / 32

  LDA columnNumber  ; (column number / 4) * 8 = column data offset
  AND #%11111100
  ASL A
  STA sourceLow
  LDA columnNumber
  LSR A
  LSR A
  LSR A
  LSR A
  LSR A
  LSR A
  LSR A
  STA sourceHigh
  
  LDA sourceLow       ; column data start + offset = address to load column data from
  CLC 
  ADC #LOW(attribData)
  STA sourceLow
  LDA sourceHigh
  ADC #HIGH(attribData)
  STA sourceHigh

  LDY #$00
  LDA $2002             ; read PPU status to reset the high/low latch
DrawNewAttributesLoop
  LDA columnHigh
  STA $2006             ; write the high byte of column address
  LDA columnLow
  STA $2006             ; write the low byte of column address
  LDA [sourceLow], y    ; copy new attribute byte
  STA $2007
  
  INY
  CPY #$08              ; copy 8 attribute bytes
  BEQ DrawNewAttributesLoopDone 
  
  LDA columnLow         ; next attribute byte is at address + 8
  CLC
  ADC #$08
  STA columnLow
  JMP DrawNewAttributesLoop
DrawNewAttributesLoopDone:

  rts
;;;;;;;;;;;;;;  
  
  
  

  
  .bank 1
  .org $E000
palette:
  .db $21,$00,$20,$0F,  $0F,$05,$25,$30,  $22,$30,$21,$0F,  $22,$27,$17,$0F   ;;background palette
  .db $21,$20,$25,$0F,  $22,$02,$38,$3C,  $22,$1C,$15,$14,  $0f,$10,$26,$05   ;;s	prite palette

sprites:
     ;vert tile attr horiz
  .db $80, $00, $00, $80   ;sprite 0  left head			$0200
  .db $80, $01, $00, $88   ;sprite 1  right head		$0204
  .db $88, $10, $00, $80   ;sprite 2  left body			$0208
  .db $88, $11, $00, $88   ;sprite 3  right body 		$020C
  .db SCOREPOSITION, SCORE0SPRITE, $02, SCOREPOSITION	; Score sprite	$0210
  .db $9F, $03, $01, $AF   ; Collectible 1   0210
  .db $AF, $03, $01, $BF   ; Collectible 2   0214
  .db $80, $03, $01, $8F   ; Collectible 3   0218
  .db P0TOP+8, $02, $03, P0RIGHT+8  	; p0 tile	
  .db P0TOP+8, $02, $03, P0RIGHT+16  	; level tile
  .db P1TOP+8, $02, $03, P1RIGHT+8  	; p1 tile	
  .db P1TOP+8, $02, $03, P1RIGHT+16  	; level tile
  .db P2TOP+8, $02, $03, P2RIGHT+8  	; P2 tile
  .db P2TOP+8, $02, $03, P2RIGHT+16  	; level tile
  .db P3TOP+8, $02, $03, P3RIGHT+8  	; P3 tile
  .db P3TOP+8, $02, $03, P3RIGHT+16  	; level tile
  .db P4TOP+8, $02, $03, P4RIGHT+8  	; P4 tile
  .db P4TOP+8, $02, $03, P4RIGHT+16  	; level tile
  .db P5TOP+8, $02, $03, P5RIGHT+8  	; P5 tile
  .db P5TOP+8, $02, $03, P5RIGHT+16  	; level tile
  .db P6TOP+8, $02, $03, P6RIGHT+8  	; P5 tile
  .db P6TOP+8, $02, $03, P6RIGHT+16  	; level tile

columnData:
  .incbin "bgtest.nam"

attribData:
  .incbin "bgtest.atr"

  .org $FFFA     ;first of the three vectors starts here
  .dw NMI        ;when an NMI happens (once per frame if enabled) the 
                   ;processor will jump to the label NMI:
  .dw RESET      ;when the processor first turns on or is reset, it will jump
                   ;to the label RESET:
  .dw 0          ;external interrupt IRQ is not used in this tutorial
  
  
;;;;;;;;;;;;;;  
  
  
  .bank 2
  .org $0000
  .incbin "demake.chr"   ;includes 8KB graphics file from SMB1