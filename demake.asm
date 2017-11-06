  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background mirroring
  

;;;;;;;;;;;;;;;
;; VARIABLES
  .rsset $0000  ;;start variables at ram location 0
buttons1   .rs 1  ; player 1 controller buttons, one bit per button
playerX	   .rs 1
playerY	   .rs 1
tempPlayerX	   .rs 1
tempPlayerY	   .rs 1
isJumping  .rs 1

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
TOPWALL        = $20
BOTTOMWALL     = $E0
LEFTWALL       = $03
    
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
  BNE .Loop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down
              
  
LoadBackground:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$20
  STA $2006             ; write the high byte of $2000 address
  LDA #$00
  STA $2006             ; write the low byte of $2000 address
  LDX #$00              ; start out at 0
LoadBackgroundLoop:
  LDA backgroundPart1, x     ; load data from address (background + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$00              ; Compare X to hex $80, decimal 128 - copying 128 bytes
  BNE LoadBackgroundLoop  ; Branch to LoadBackgroundLoop if compare was Not Equal to zero
                        ; if compare was equal to 128, keep going down
; Loads second section of bg						
LoadBackgroundLoop2:
  LDA backgroundPart2, x     ; load data from address (background + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$00              ; Compare X to hex $80, decimal 128 - copying 128 bytes
  BNE LoadBackgroundLoop2  ; Branch to LoadBackgroundLoop if compare was Not Equal to zero
                        ; if compare was equal to 128, keep going down
						
LoadBackgroundLoop3:
  LDA backgroundPart3, x     ; load data from address (background + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$00              ; Compare X to hex $80, decimal 128 - copying 128 bytes
  BNE LoadBackgroundLoop3  ; Branch to LoadBackgroundLoop if compare was Not Equal to zero
                        ; if compare was equal to 128, keep going down
						
												
LoadBackgroundLoop4:
  LDA backgroundPart4, x     ; load data from address (background + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$00              ; Compare X to hex $80, decimal 128 - copying 128 bytes
  BNE LoadBackgroundLoop4  ; Branch to LoadBackgroundLoop if compare was Not Equal to zero
     
              
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
  BNE LoadAttributeLoop  ; Branch to LoadAttributeLoop if compare was Not Equal to zero
                        ; if compare was equal to 128, keep going down  

LoadIntialCharacterCoord:
  LDA $04
  STA playerX
  STA playerY

  LDA #%10000000   ; enable NMI, sprites from Pattern Table 1
  STA $2000

  LDA #%00010000   ; enable sprites
  STA $2001
  
  LDA #$50
  STA playerY
  
  LDA #$80
  STA playerX

Forever:
  JMP Forever     ;jump back to Forever, infinite loop
  
NMI:
  LDA #$00
  STA $2003       ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014       ; set the high byte (02) of the RAM address, start the transfer

  JSR ReadController1 

ReadUp: 
  LDA buttons1   ;Player 1 up arrow
  AND #CONTROLLER_UP 
  BEQ .Done  
  LDA CHARACTERYATTRIBUTE 
  CMP #TOPWALL
  BCC .Done
  LDA playerY
  SEC                           ; make sure carry flag is set
  SBC #$01        			    ; A = A - 1
  STA playerY
.Done:  
  
ReadDown: 
  LDA buttons1					;Player 1 down arrow
  AND #CONTROLLER_DOWN 
  BEQ .Done
  LDA CHARACTERYATTRIBUTE
  CMP #BOTTOMWALL
  BCS .Done
  LDA playerY
  CLC							; make sure carry flag is set
  ADC #$01						; A = A - 1
  STA playerY
.Done: 
  
ReadLeft: 
  LDA buttons1					; player 1 left arrow
  AND #CONTROLLER_LEFT			; only look at bit 0
  BEQ .Done						; branch to ReadLeftDone if button is NOT pressed (0)
  LDA CHARACTERXATTRIBUTE
  CMP #LEFTWALL
  BCC .Done
  LDA playerX					; load sprite X position
  SEC							; make sure carry flag is set
  SBC #$01						; A = A - 1
  STA playerX
.Done:							;  handling this button is done

ReadRight: 
  LDA buttons1 ; player 1 right arrow 
  AND #CONTROLLER_RIGHT  ; only look at bit 0
  BEQ .Done		    ; branch to ReadRightDone if button is NOT pressed (0)
  LDA CHARACTERXATTRIBUTE
  CMP #RIGHTWALL
  BCS .Done
  LDA playerX					 ; load sprite X position
  CLC							 ; make sure carry flag is set
  ADC #$01						 ; A = A - 1
  STA playerX					 ; save sprite X position

.Done:							 ; handling this button is done

UpdateCharacterSprites				; Updates Charater's sprites position
 LDA playerY						; Loads playerX
 STA tempPlayerY					; Saves playerX value in tempPlayerX
 LDA playerX						; Loads playerY
 STA tempPlayerX					
 
 LDA tempPlayerY  					; Loads tempPlayerY
 STA $0200							; Save to top left sprite
 STA $0204							; Save  to top right sprite
 CLC								; Clear carry
 ADC #$08							; Add 8 to tempPlayerY
 STA $0208							; Save to bottom left sprite
 STA $020C							; Save to bottom right sprite
 
 LDA tempPlayerX					; Loads tempPlayerX
 STA $0203
 STA $020B
 CLC
 ADC #$08
 STA $0207
 STA $020F


  ;;This is the PPU clean up section, so rendering the next frame starts properly.
  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000
  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001
  LDA #$00         ;tell the ppu there is no background scrolling
  STA $2005
  STA $2005


  RTI             ; return from interrupt
    
ReadController1:
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