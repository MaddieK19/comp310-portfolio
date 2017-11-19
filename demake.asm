  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background mirroring
  

;;;;;;;;;;;;;;;
;; VARIABLES
  .rsset $0000  ;;start variables at ram location 0
buttons1			.rs 1  ; player 1 controller buttons, one bit per button
playerX				.rs 1  ; Character sprite X position
playerY				.rs 1  ; Character sprite Y position
tempPlayerX			.rs 1  ; Temporary X position for use in sprite positioning
tempPlayerY			.rs 1  ; Temporary Y position for use in sprite positioning
gravity				.rs 1  ; Value for gravity
jumpAmount			.rs 1  ; Value for jump height 
isFalling			.rs 1  ; 0 for falling, 1 for not falling

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

RIGHTWALL      = $F4
TOPWALL        = $10
BOTTOMWALL     = $D4
LEFTWALL       = $03

MAX_GRAVITY    = $03	; The maximum speed at which an object can fall
JUMP_HEIGHT	   = $08	; The height of the character's jump
    
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
  STA $0200, x
  STA $0400, x
  STA $0500, x
  STA $0600, x
  STA $0700, x
  LDA #$FE
  STA $0300, x
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
                          ; 1st time through loop it will load palette+0
                          ; 2nd time through loop it will load palette+1
                          ; 3rd time through loop it will load palette+2
                          ; etc
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$20              ; Compare X to hex $10, decimal 16 - copying 16 bytes = 4 sprites
  BNE LoadPalettesLoop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down



LoadSprites:
  LDX #$00              ; start at 0
.Loop:
  LDA sprites, x        ; load data from address (sprites +  x)
  STA $0200, x          ; store into RAM address ($0200 + x)
  INX                   ; X = X + 1
  CPX #$20              ; Compare X to hex $20, decimal 32  ; TODO meant to be 20??
  BNE .Loop   			; Branch to LoadSpritesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down
              
  
LoadBackground:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$20
  STA $2006             ; write the high byte of $2000 address
  LDA #$00
  STA $2006             ; write the low byte of $2000 address
  LDX #$00              ; start out at 0
  
LoadBackgroundLoop .macro
.Loop\@:
  LDA \1, x    ; load data from address (background + the value in x)
  STA $2007             	; write to PPU
  INX                   	; X = X + 1
  CPX #$00              	; Compare X to hex $80, decimal 128 - copying 128 bytes
  BNE .Loop\@  	; Branch to LoadBackgroundLoop if compare was Not Equal to zero
  .endm

  LoadBackgroundLoop backgroundPart1     
  LoadBackgroundLoop backgroundPart2  
  LoadBackgroundLoop backgroundPart3
  LoadBackgroundLoop backgroundPart4  
  
LoadAttribute:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$23
  STA $2006             ; write the high byte of $23C0 address
  LDA #$C0
  STA $2006             ; write the low byte of $23C0 address
  LDX #$00              ; start out at 0
LoadAttributeLoop:
  LDA attribute, x      ; load data from address (attribute + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$00              ; Compare X to hex $08, decimal 8 - copying 8 bytes  ; WAS 08
  BNE LoadAttributeLoop ; Branch to LoadAttributeLoop if compare was Not Equal to zero
                        ; if compare was equal to 128, keep going down  

SetIntialValues:
  LDA #$80
  STA playerX
  STA playerY
  
  LDA #$01
  STA gravity
  STA isFalling
  
  LDA #$00
  STA jumpAmount
  
  LDA #%10000000   ; enable NMI, sprites from Pattern Table 1
  STA $2000

  LDA #%00010000   ; enable sprites
  STA $2001

Forever:
  JMP Forever     ;jump back to Forever, infinite loop
  
NMI:
  LDA #$00
  STA $2003       ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014       ; set the high byte (02) of the RAM address, start the transfer

  JSR ReadController1 

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
  LDA #JUMP_HEIGHT				; Loads JUMP_HEIGHT
  STA jumpAmount				; Save to jumpAmount
.Done: 						    ; handling this button is done

UpdateGravity:
  LDA isFalling					; Loads isFalling
  CMP #$00
  BEQ .Done
  LDA playerY					; Load playerY
  CMP #BOTTOMWALL				; Compare to BOTTOMWALL
  BCS .Done						; Branch if playerY < BOTTOMWALL
  CLC							; Clear carry
  ADC gravity					; Adds the value of gravity to A
  STA playerY					; Saves to playerY
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
 

UpdateCharacterSprites			; Updates Charater's sprites position
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


  ;;This is the PPU clean up section, so rendering the next frame starts properly.
  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000
  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001
  LDA #$00         ;tell the ppu there is no background scrolling
  STA $2005
  STA $2005


  RTI             ; return from interrupt
    
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
 
;;;;;;;;;;;;;;  
  
  .bank 1
  .org $E000
palette:
  .db $0F,$82,$8F,$3D,  $0F,$05,$25,$30,  $22,$30,$21,$0F,  $22,$27,$17,$0F   ;;background palette
  .db $21,$20,$25,$0F,  $22,$02,$38,$3C,  $22,$1C,$15,$14,  $22,$02,$38,$3C   ;;sprite palette

sprites:
     ;vert tile attr horiz
  .db $80, $00, $00, $80   ;sprite 0  left head
  .db $80, $01, $00, $88   ;sprite 1  right head
  .db $88, $10, $00, $80   ;sprite 2  left body
  .db $88, $11, $00, $88   ;sprite 3  right body 

; 01 = sky  
; 02 = plain block 
; 03 - building
backgroundPart1:
  .db $03,$03,$01,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$01,$01,$01
  .db $01,$01,$03,$03,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$03,$03 
  
  .db $03,$03,$01,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$01,$01,$01
  .db $01,$01,$03,$03,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$03,$03 
  
  .db $03,$03,$01,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$01,$01,$01
  .db $01,$01,$03,$03,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$03,$03 
  
  .db $03,$03,$01,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$01,$01,$01
  .db $01,$01,$03,$03,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$03,$03 
  
  .db $03,$03,$01,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$01,$01,$01
  .db $01,$01,$03,$03,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$03,$03 
  
  .db $03,$03,$01,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$01,$01,$01
  .db $01,$01,$03,$03,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$03,$03 
  
  .db $03,$03,$01,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$01,$01,$01
  .db $01,$01,$03,$03,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$03,$03 
  
  .db $03,$03,$01,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$01,$01,$01
  .db $01,$01,$03,$03,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$03,$03 
  
backgroundPart2:
  .db $03,$03,$01,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$01,$01,$01
  .db $01,$01,$03,$03,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$03,$03 
  
  .db $03,$03,$01,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$01,$01,$01
  .db $01,$01,$03,$03,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$03,$03 
  
  .db $03,$03,$01,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$01,$01,$01
  .db $01,$01,$03,$03,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$03,$03 
  
  .db $03,$03,$01,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$01,$01,$01
  .db $01,$01,$03,$03,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$03,$03 
  
  .db $03,$03,$01,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$01,$01,$01
  .db $01,$01,$03,$03,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$03,$03 
  
  .db $03,$03,$01,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$01,$01,$01
  .db $01,$01,$03,$03,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$03,$03 
  
  .db $03,$03,$01,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$01,$01,$01
  .db $01,$01,$03,$03,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$03,$03 
  
  .db $03,$03,$01,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$01,$01,$01
  .db $01,$01,$03,$03,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$03,$03 
  
backgroundPart3:
  .db $03,$03,$01,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$01,$01,$01
  .db $01,$01,$03,$03,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$03,$03 
  
  .db $03,$03,$01,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$01,$01,$01
  .db $01,$01,$03,$03,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$03,$03 
  
  .db $03,$03,$01,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$01,$01,$01
  .db $01,$01,$03,$03,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$03,$03 
  
  .db $03,$03,$01,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$01,$01,$01
  .db $01,$01,$03,$03,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$03,$03 
  
  .db $03,$03,$01,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$01,$01,$01
  .db $01,$01,$03,$03,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$03,$03 
  
  .db $03,$03,$01,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$01,$01,$01
  .db $01,$01,$03,$03,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$03,$03 
  
  .db $03,$03,$01,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$01,$01,$01
  .db $01,$01,$03,$03,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$03,$03 
  
  .db $03,$03,$01,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$01,$01,$01
  .db $01,$01,$03,$03,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$03,$03 
  
backgroundPart4:
  .db $03,$03,$01,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$01,$01,$01
  .db $01,$01,$03,$03,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$03,$03 
  
  .db $03,$03,$01,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$01,$01,$01
  .db $01,$01,$03,$03,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$03,$03 
  
  .db $03,$03,$01,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$01,$01,$01
  .db $01,$01,$03,$03,$01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$03,$03 
  
  .db $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
  .db $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02 
  
  .db $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
  .db $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02 
  
  .db $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
  .db $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02  
   
  


attribute:
  .db %00000000, %00010000, %0010000, %00010000, %00000000, %00000000, %00000000, %00110000
  .db %00000000, %00010000, %0010000, %00010000, %00000000, %00000000, %00000000, %00110000
  .db %00000000, %00010000, %0010000, %00010000, %00000000, %00000000, %00000000, %00110000
  .db %00000000, %00010000, %0010000, %00010000, %00000000, %00000000, %00000000, %00110000
  .db %00000000, %00010000, %0010000, %00010000, %00000000, %00000000, %00000000, %00110000
  .db %00000000, %00010000, %0010000, %00010000, %00000000, %00000000, %00000000, %00110000
  .db %00000000, %00010000, %0010000, %00010000, %00000000, %00000000, %00000000, %00110000
  .db %00000000, %00010000, %0010000, %00010000, %00000000, %00000000, %00000000, %00110000  

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