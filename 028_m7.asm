                  ORG #D280

MMM               LD HL,TXTMAIN:LD DE,SMUS
                  RES 0,(IX+10):OR A
                  CALL PRMENU:CALL HLN
                  CALL FRAMES:CALL SATR
                  RES 1,(IX+10)
                  RES 2,(IX+10)
                  CALL CONTR:JR MMM
BASE EQU 25000
POSTAB            EQU BASE+2
PATTERNS EQU BASE+1282
S EQU 17960+PATTERNS
O EQU 483*15+S
SOUNDS EQU 123*15+O
END EQU SOUNDS+192
VARS DEFS 12
PMODE DEFB 1,2,7,"PATTERNCOMPOSE"
MT EQU #FF00
FNT EQU 53433
CLCA              LD A,1:LD (NOF),A
                  LD HL,#FC00:LD DE,#FC01
                  LD BC,#1FF:LD (HL),L:LDIR:RET
CH128             LD A,1:OR A:RET Z:POP AF:SCF:RET
S128              CALL CH128:LD HL,SCST
                  LD DE,#5B00:PUSH DE
                  LD BC,ATHL-SCST:LDIR
RADR              RET
SCST              LD BC,#7FFD:LD A,#17
                  DI:OUT (C),A:PUSH BC
                  LD HL,#4000:LD DE,#C000
                  LD BC,#1B00:LDIR:POP BC
                  INC A:OUT (C),A:EI:RET
ATHL              PUSH HL:PUSH AF
                  CALL S128:POP AF:POP HL
                  LD E,(HL):INC HL
                  LD D,(HL):INC HL
                  LD (VARS),DE:LD A,E
                  LD (XPOS+3),A:RET NC
                  LD B,(HL):INC HL
                  LD C,(HL):INC HL:PUSH HL
                  LD A,(HL):LD (RCO+1),A
                  PUSH BC:LD H,D:LD A,B:INC A
                  ADD A,E:LD L,A:PUSH HL
                  DEC D:DEC E:CALL SCRAD+4
                  EX DE,HL:LD A,C:ADD A,A
                  ADD A,A:ADD A,A:SUB 4
                  LD C,A:CALL HLIN:CALL HLIN
DFF               PUSH BC:LD C,0:LD E,L
                  LD (HL),#C0:INC L
DF1               LD (HL),C:INC L:DJNZ DF1
                  LD (HL),3:LD L,E:POP BC
                  CALL DF:DEC C:JR NZ,DFF
                  CALL HLIN:CALL HLIN
                  POP HL:POP BC:LD A,L
                  CP 32:JR NC,DFE
                  LD A,H:DEC C:INC B:INC B
                  RRCA:RRCA:RRCA:LD H,A
                  AND #E0:OR L:LD L,A:LD A,H
                  AND 3:OR #58:LD H,A:LD A,1
                  LD DE,32
VA                LD (HL),A:ADD HL,DE
                  DEC C:JR NZ,VA
HA                LD (HL),A:DEC L:DJNZ HA
DFE               POP HL:RET
DRVB              LD HL,#49F7:LD (HL),#18
                  INC H:LD (HL),#24
                  INC H:LD (HL),#42
                  LD HL,#4357:LD (HL),#42
                  INC H:LD (HL),#24
                  INC H:LD (HL),#18
                  LD HL,#4750:LD C,15
                  XOR A:LD (RCO+1),A:CALL DRS
                  LD HL,#41F0:LD C,31
DRS               LD B,13
DRS0              CALL HLIN:INC H:DEC C:JR NZ,DRS0
HLIN              PUSH BC:LD C,#FF
HLIN2             LD E,L:INC B:INC B
HLN0              LD (HL),C:INC L:DJNZ HLN0
                  LD L,E:POP BC
DF                INC H:LD A,H:AND 7:RET NZ
                  LD A,H:RRA:RRA:RRA:LD D,H:LD E,L
                  DEC A:AND 3:OR #58:LD H,A
RCO               LD A,0:PUSH BC:INC B:INC B
DFA               LD (HL),A:INC L:DJNZ DFA:POP BC
                  LD H,D:LD A,E:ADD A,32:LD L,A
                  RET C:LD A,H:SUB 8:LD H,A:RET
HLN               LD HL,#4020:LD C,7:LD E,#FF
HL0               PUSH HL:LD B,30:LD (HL),#1F
HL1               INC L:LD (HL),E:DJNZ HL1
                  INC L:LD (HL),#F0:POP HL
                  LD A,L:ADD A,32:LD L,A
                  DEC C:JR NZ,HL0
                  LD HL,#58E0
HL2               LD (HL),#47:INC L:JR NZ,HL2
                  LD HL,#4000:LD C,1
CLL               LD E,L:XOR A
CLL1              LD B,32
CLL0              LD (HL),A:INC L:DJNZ CLL0
                  INC H:LD L,E:DEC C:JR NZ,CLL1:RET
FRAMES            LD HL,#41E0:LD C,7:CALL CLL
                  LD HL,#4AE0:LD C,6:CALL CLL
                  LD DE,#47E0:CALL HFR
                  LD B,57:LD DE,#4800
FR0               PUSH BC:LD HL,VFRD
                  CALL DRFR:POP BC:DJNZ FR0
HFR               LD HL,HFRD
DRFR              PUSH DE:CALL DRFR1:POP DE
                  INC D:LD A,D:AND 7:RET NZ
                  LD HL,#F820:ADD HL,DE
                  EX DE,HL:RET
SATR              LD DE,#59E0:LD A,#47
SA0               LD (DE),A:INC E:JR NZ,SA0
                  CALL DRFR1:CALL SATR1
                  LD HL,ATD2:CALL DRFR1
SATR1             LD C,3
SATR0             LD HL,ATD:CALL DRFR1
                  DEC C:JR NZ,SATR0:RET
DRFR1             LD B,(HL):BIT 7,B:RET NZ
                  INC HL:LD A,(HL):INC HL
DRFR0             CALL LDB:JR DRFR1
LDB               LD (DE),A:INC E:DJNZ LDB:RET
HFRD DEFB 2,0,1,#1F,26,#FF,1,#F0,2,0,255
VFRD DEFB 2,0,1,16,2,0,1,16,2,0,1,16,6,0,1,16,6,0,1,16,6,0,1,16,2,0,255
ATD DEFB 3,#47,2,#43,1,#47,2,#43
DEFB 1,#47,6,#43,1,#47,6,#43,1,#47
DEFB 6,#43,3,#47,255
ATD2 DEFB 3,#47,2,#46,1,#47,2,#46
DEFB 1,#47,6,#46,1,#47,6,#46,1,#47
DEFB 6,#46,3,#47,255
PRMENU            PUSH DE:CALL ATHL
                  EX (SP),HL:INC H:DEC H
                  JR NZ,SUSR:LD HL,RADR
SUSR              LD (USR),HL
                  CALL CLCA:POP HL:CALL PRS
                  LD DE,VARS+2:LD BC,8:LDIR
                  LD DE,MT
PRM0              LD A,(HL)
                  CP 10:JR C,ENDF
                  CP 255:JR Z,EM
                  CP "|:JR Z,PRM1
                  CP #D:JR Z,PRM1
                  CP "\:JR Z,PRM2
                  OR A:PUSH HL:JP M,TAB
                  CALL CATAD0
NOF EQU $+1:LD (HL),0:POP HL:LD A,(HL)
PRM1              CALL PRCH
                  INC HL:JR PRM0
PRM2              LD A," :JR PRM1
EM                LD BC,(VARS+6):LD L,B
                  LD H,C:CALL CATADR
                  LD A,(HL):LD BC,32
                  SBC HL,BC:LD (HL),A
                  LD HL,(VARS+5)
                  LD A,(VARS+8):LD H,A
                  CALL CATADR:LD A,(HL)
                  ADD HL,BC:LD (HL),A
                  RES 7,(IX+10)
                  LD A,#FF:LD (DE),A
PRDAT             LD HL,MT
PRD               BIT 7,(HL):JP NZ,PRCUR
                  PUSH HL:LD C,0
                  CALL CDAT0:LD DE,9
                  POP HL:ADD HL,DE:JR PRD
ENDF              LD BC,7:LDIR:PUSH IX
                  EX (SP),HL:LDI:LDI
                  LD HL,NOF:INC (HL)
                  POP HL:JR PRM0
TAB               AND #7F:PUSH AF:CALL CATAD0
                  POP AF:LD B,A:ADD A,(IX)
                  LD (IX),A:LD A,(NOF)
TAB0              LD (HL),A:INC L:DJNZ TAB0
                  POP HL:JR PRM1+3
PSGN              LD A,(DE):AND #7F:LD (DE),A
PSGNL             BIT 6,A:LD B,"+
                  JR Z,PSGN0:LD B,"-:CPL:AND #3F:INC A
PSGN0             LD C,A:LD A,B
                  CALL PRCH:LD A,C:JR P630
P63               CALL PFLC:LD (HL),A
                  BIT 7,B:JR NZ,P6300:INC A
P6300             LD C,A:LD A,B:AND #7F:CP 100
                  LD A,C:JR NC,P2552:INC (IX)
P630              CALL BBD:LD B,A:JR P2551
P255              LD HL,0
                  LD A,(DE):CP (HL)
                  JR Z,P255L:RET NC
P255L             INC A:JR NZ,P2552
                  LD A,2:LD B,#56:JR P2550
PSW               LD A,"+:BIT 3,D:JR Z,PSW0
                  PUSH HL:LD HL,0:OR A
                  SBC HL,DE:EX DE,HL:POP HL:LD A,"-
PSW0              CALL PRCH:LD A,D
                  AND #F:LD D,A:EX DE,HL
                  CALL BBD2:EX DE,HL:LD B,A
                  CALL P2551:LD B,D:JR P2551-1
BBD2              ADD HL,HL:ADD HL,HL
                  ADD HL,HL:XOR A:LD B,14
BBD20             DEC B:JR Z,BBD21:ADD HL,HL
                  ADC A,A:DAA:JR NC,BBD20
                  INC HL:JR BBD20
BBD21             LD H,A:LD A,L:CP 16
                  JR C,BBD22:ADD A,6
BBD22             OR A:DAA:RET
P2552             CALL BBD
                  LD B,A:LD A,C:OR A
                  JR NZ,P2550:LD A,#F0
P2550             CALL PRDIG:LD A,B
P2551             RRA:RRA:RRA:RRA:AND #F
                  CALL PRDIG:LD A,B
                  AND #F:JR PRDIG
BBD               LD C,A:XOR A:LD B,9
BBD0              DEC B:RET Z:SLA C
                  ADC A,A:DAA:JR NC,BBD0
                  INC C:JR BBD0
PFLC              EX DE,HL:LD A,(HL):OR A:INC HL
                  LD B,(HL):PUSH BC:RES 7,B
                  DEC HL:JP P,PFL0
                  LD A,B:DEC A
PFL0              CP B:POP BC:RET C:XOR A:RET
PFL               CALL PFLC
                  LD (HL),A:LD B,A:INC B
                  INC HL:INC HL:LD E,(HL)
                  LD A,(IX):SUB E:JR NC,PFL2
                  AND #1F:DEC (IX+1)
PFL2              LD (IX),A:LD D,0
                  SBC HL,DE:INC HL
PFL20             ADD HL,DE:DJNZ PFL20:LD B,E
PFL3              LD A,(HL):INC HL
                  CALL PRCH:DJNZ PFL3:RET
P15               LD A,(DE):AND #F
                  LD (DE),A:OR A:JR NZ,PHEX+2
                  LD A,"-:JR PRCH
PHEX              AND #F:CP 10:JR C,PRDIG:ADD A,7
PRDIG             ADD A,#30
PRCH              PUSH HL:PUSH DE:PUSH BC:PUSH AF
                  CP #0D:JR Z,NEXTLIN
                  CP "_:JR Z,NOPR
                  CP "|:JR Z,PRV
                  CP "~:JR Z,CBACK
                  OR A:JP M,TABC
                  LD C,A:LD B,0:LD H,B
                  ADD A,A:LD L,A
                  LD DE,FNT:ADD HL,HL
                  ADD HL,BC:ADD HL,DE
                  CALL SCRAD:LD B,5:INC D
                  XOR A:LD (DE),A
PRC               INC D:LD A,(HL):INC HL
                  LD (DE),A:DJNZ PRC
                  INC D:XOR A
PRZL              LD (DE),A
PRATR             BIT 7,(IX+10):JR Z,NOPR
                  LD A,D:RRA:RRA:RRA
                  AND 3:OR #58:LD D,A
                  LD A,(IX+4):LD (DE),A
NOPR              INC (IX):BIT 5,(IX):JR Z,NONL
NEXTLIN INC (IX+1)
XPOS              LD (IX),0
NONL              POP AF:POP BC:POP DE:POP HL:RET
TABC              AND #7F:ADD A,(IX)
                  LD (IX),A:JR NONL
CBACK             DEC (IX):JR NONL
PRV               CALL SCRAD:LD A,#10:LD B,7
PRV0              INC D:LD (DE),A:DJNZ PRV0
                  JR PRATR
SCRAD             LD DE,(VARS)
                  LD A,D:RRCA:RRCA:RRCA
                  AND #E0:OR E:LD E,A
                  LD A,D:AND #18:OR #40
                  LD D,A:RET
PRS               LD A,(HL):INC HL:LD (IX+4),A
                  SET 7,(IX+10)
PRS0              LD A,(HL):INC HL:CP #FF
                  RET Z:CALL PRCH:JR PRS0
PRSTR             EX (SP),HL
PRSTR0            LD A,(HL):INC HL
                  CP #FF:CALL NZ,PRCH
                  JR NZ,PRSTR0
                  EX (SP),HL:RET
TXTMAIN DEFB 0,0,#45," CACOFONY PRO V0.0            (C) S.T.A.S."
COORDS DEFB 255,30,5,#47,1,1,30,6,#67
DEFB "|POSITION",#83,0+5,4,"I:DEFW NPOS,SELPOS
DEFB "|PLAY SONG",4,1,"P:DEFW 0,PLAY
DEFB "|A_",2,1,"A:DEFW AMODE,INCDAT
DEFB "|B_",2,1,"B:DEFW BMODE,INCDAT
DEFB "|C_",2,1,"C:DEFW CMODE,INCDAT
DEFB "||EDIT PAT.",#82,4+5,1,"E:DEFW NPAT,EDITPAT
DEFB "|PLAY ",#87,2,5," :DEFW PMODE,PLPAT;+5
DEFB "|CLEAR|",1,5,"R:DEFW 0,CLEAR
DEFB "|PAT.SIZE ",#82,4+5,7,"Z:DEFW PSIZE,0
DEFB "|SAMPLE _",1,1,"S:DEFW NSAM,EDSAM
DEFB "|COPY TEXT|",4,8,"X:DEFW 0,COPY
DEFB "|DELAY _",1,5,"Y:DEFW DELAY,0
DEFB "/_",2,1,"/:DEFW CALLS,0
DEFB "|ORNAMENT _",1,1,"O:DEFW NORN,EDORN
DEFB "|TRANSPOSE|",4,1,"T:DEFW 0,TRANSP
DEFB "|A",#83,3,0,#FF:DEFW DISPA,0
DEFB "|B",#83,3,0,#FF:DEFW DISPB,0
DEFB "|C",#83,3,0,#FF:DEFW DISPC,0
DEFB "|OCTAVE",58,#83,2,5,"V:DEFW NOCT,0
DEFB "|DISK|",2,1,"D:DEFW 0,DMENU
DEFB "|LEN.",#83,0+5,3,"N:DEFW MLEN,0
DEFB "|LP.",#83,0+5,1,"L:DEFW LOOP,0
DEFB "|ENV.",#83,2,4,".:DEFW ENVPROP,0
DEFB "|COMPILE|",4,3,"M:DEFW 0,COMPILE
DEFB 255
NPOS DEFB 0;255.POS. NUMBER
PSIZE DEFB 63,64;PAT. SIZE
LOOP EQU BASE;LOOP
MLEN EQU BASE+1;LENGHT
DISPA DEFB 0;SGN.DISP. OF CHANNEL
DISPB DEFB 0
DISPC DEFB 0
DELAY DEFB 5;15.DELAY
NPAT DEFB 0,40;PATTERN
AMODE DEFB 0,2,1,"+-"
BMODE DEFB 0,2,1,"+-"
CMODE DEFB 0,2,1,"+-"
NSAM DEFB 0;15.SAMPLE
NORN DEFB 0;ORNAMENT
NOCT DEFB 4,8,3,"SUBCONLRGSML1ST2ND3RD4TH"
ENVPROP DEFB 0,9,3,"---1/13/41/21/43/15/22/13/2"
CALLS DEFB 0,2,1,"12"

GKEY              LD IY,#5C3A
                  XOR A:BIT 5,(IY+1):RET Z
                  LD A,(23560):RES 5,(IY+1):RET
KW                CALL GKEY:JR Z,KW
BEEP              BIT 3,(IX+11):RET NZ
                  PUSH AF:LD A,#30:DI
BP0               OUT (#FE),A:LD B,A
                  XOR A:DJNZ $
                  OUT (#FE),A:POP AF:EI:RET
KEYSM             LD HL,MNUKEYS
KEYS              DEC D:INC D:JR Z,KEYS0:PUSH DE
KEYS0             CP (HL):INC HL:JR Z,GO
                  INC HL:INC HL:BIT 7,(HL):JR Z,KEYS0:RET
GO                LD A,(HL):INC HL:LD H,(HL):LD L,A:JP (HL)
CONTR             HALT:CALL GKEY
                  RES 4,(IX+10)
                  RES 5,(IX+10):PUSH AF
                  CALL OCTSEL:POP AF:PUSH AF
USR EQU $+1
                  CALL 0
                  LD BC,#7FFD:LD D,#10:OUT (C),D
                  POP AF
                  BIT 5,(IX+10):PUSH AF
                  CALL NZ,PLAYN:POP AF:JR NZ,CONTR
                  OR A:JR Z,CONTR:CALL NZ,BEEP
                  PUSH AF:LD DE,PRCUR
                  LD HL,MLEN:LD (P255+1),HL
                  CALL KEYSM:POP AF
                  CALL QKEYS:BIT 0,(IX+10)
                  JR Z,CONTR:RET
PLAY              SET 5,(IX+11):JR PLPAT+3
PLPAT             RES 5,(IX+11)
                  BIT 3,(IX+11):RET NZ
                  SET 3,(IX+11):LD A,1
                  LD (PLD+1),A:RET
QUIT              BIT 3,(IX+11):JR Z,QUITE
SPLM              RES 3,(IX+11):JP MUTE
QUITE             LD HL,QTXT
W1                LD D,0
W2                BIT 2,(IX+10):JR NZ,W2E
                  SET 2,(IX+10):PUSH HL:PUSH DE
                  CALL STCOOR:SCF:POP DE
                  POP HL:CALL PRMENU
                  JP CONTR
W2E               POP HL:RET
QTXT DEFB 12,2,6,5,2*8+#46
DEFB " QUIT?",#D,#D
DEFB 255,16,4,2*8+#47,12,4,17,4,7*8+#42
DEFB "YES\",4,1,"y:DEFW 0,QUIT0
DEFB "NO",4,1,"n:DEFW 0,NQ:DEFB 255
MCCS              LD A,2:LD (MCC+1),A
NQ                SET 0,(IX+10):RET
QUIT0             LD SP,0:IM 1:EI:RET
MNUKEYS DEFB #7:DEFW QUIT
DEFB "a:DEFW CDOWN
DEFB "q:DEFW CUP
DEFB "p:DEFW CRIGHT
DEFB "o:DEFW CLEFT
DEFB #E:DEFW HELP
DEFB #4:DEFW DECDAT
DEFB #5:DEFW INCDAT
DEFB #D:DEFW ENTER
DEFB "<:DEFW SLN1
DEFB ">:DEFW SLN2
DEFB 255
SLN1              LD A,(LINE):LD (LN1),A
                  LD (LN3),A:RET
SLN2 LD A,(LINE):LD (LN2),A:RET
FTC               CALL CATAD2
FT00              LD A,(HL):INC L:OR A
                  JR Z,FT00:DEC L
FT01              LD HL,MT-9
                  LD DE,9:LD B,A
FTC0              ADD HL,DE:DJNZ FTC0:RET
QKEYS             OR A:RET Z:LD HL,MT
                  LD DE,7:LD C,1
QK0               BIT 7,(HL):RET NZ
                  INC HL:INC HL
                  CP (HL):JR Z,QKF
                  ADD HL,DE:INC C:JR QK0
QKF               CALL CATAD2
QK3               LD A,(HL):INC L:OR A:JR Z,QK3
                  CALL CATAD5:LD A,C
                  LD BC,0:CPIR
                  LD A,L:AND #1F:LD (IX+2),A
                  LD A,H:AND 3:RL L:RLA
                  RL L:RLA:RL L:RLA
                  LD (IX+3),A:CALL PRCUR
ENTER             CALL FTC:LD E,5:ADD HL,DE
                  LD E,(HL):INC HL:LD D,(HL)
                  LD A,E:OR D:RET Z
                  BIT 1,(IX+10):EX DE,HL:JP (HL)
OCTSEL            CP "@:LD C,1:JR Z,OS
                  CP "!:RET C:CP "):RET NC
                  SUB #21:LD C,A
OS                LD HL,NOCT:LD (HL),C
                  BIT 2,(IX+10):RET NZ
                  LD HL,9*18+MT
                  LD C,0:JR CDAT0
INCDAT            LD C,1:JR CDAT
DECDAT            LD C,#FF
CDAT              CALL FTC
CDAT0             LD A,(HL):INC HL:INC HL:INC HL
                  CP 5:JR C,CD1:SUB 5
                  SET 3,(IX+10)
CD1               LD E,(HL):INC HL
                  LD D,(HL):INC D:DEC D:RET Z
                  EX DE,HL:LD B,A:ADD A,A:ADD A,B:LD B,A
                  LD A,(HL):ADD A,C:PUSH HL
                  LD (HL),A:EX DE,HL
                  INC HL:INC HL:INC HL
                  LD E,(HL):INC HL:LD D,(HL)
                  LD L,B:LD H,0
                  LD BC,DCTAB:ADD HL,BC
                  LD A,E:SUB (HL):JR NC,CD0
                  AND #1F:DEC D
CD0               LD E,A
                  LD (VARS),DE:INC HL
                  POP DE:JP GO
DCTAB             DEFB 3:DEFW P255
DEFB 1:DEFW P15
DEFB 0:DEFW PFL
DEFB 3:DEFW PSGN
DEFB 3:DEFW P63
CDOWN             INC (IX+3):LD A,(IX+8)
                  CP (IX+3):RET NC
                  LD A,(IX+6)
C0                LD (IX+3),A:RET
CUP               DEC (IX+3):LD A,(IX+3)
                  CP (IX+6):RET NC
                  LD A,(IX+8):JR C0
CRIGHT            CALL CATAD2
CRR               LD A,(HL):OR A:INC HL:JR Z,CRR
CR0               CPI:JR Z,CR0:XOR A
CR1               CP (HL):INC HL:JR Z,CR1:DEC HL:LD A,L
                  LD A,L:AND #1F:LD (IX+2),A:RET
CLEFT             CALL CATAD2
CL2               LD A,(HL):DEC HL:OR A:JR Z,CL2
CL0               CPD:JR Z,CL0:XOR A
CL1               CP (HL):DEC HL:JR Z,CL1:INC HL
                  LD A,L:AND #1F:LD (IX+2),A:RET
CATAD5            LD HL,(VARS+5):JR CATADR
CATAD2            LD HL,(VARS+2):JR CATADR
CATAD0            LD HL,(VARS+0)
CATADR            LD A,H:RRCA:RRCA:RRCA
                  LD H,A:AND #E0:OR L:LD L,A
                  LD A,H:AND 3:OR #FC:LD H,A:RET
STCOOR            LD HL,(VARS+2)
                  LD (COORDS+1),HL
                  SET 1,(IX+10)
CLCUR             LD A,(IX+4):LD (PRCU+1),A
                  LD A,#FF:LD (OLDC+1),A
                  CALL PRCUR0
                  LD A,#FF:LD (OLDC+1),A:RET
PRCUR             BIT 0,(IX+10):RET NZ
                  LD A,(IX+9):LD (PRCU+1),A
PRCUR0            CALL CATAD2
OLDC              LD DE,0:SBC HL,DE:RET Z
                  ADD HL,DE:LD (OLDC+1),HL
                  LD A,L:AND #E0:LD C,A
                  LD D,H:LD E,L
PRC01             LD A,(HL):OR A:LD B,L:JR NZ,PRC00
                  LD A,(DE):OR A:LD B,E:JR NZ,PRC00
                  DEC E:LD A,E:AND #1F
                  OR C:LD E,A
                  INC L:LD A,L:AND #1F
                  OR C:LD L,A:JR PRC01
PRC00             LD (FBYTE+1),A
               LD A,B:AND #1F:LD (IX+2),A
                  CALL CATAD5
                  LD A,(IX+7):SUB (IX+5)
                  INC A:LD (MNUWDT+1),A
                  LD A,(IX+8):SUB (IX+6)
                  INC A:LD B,A
PRC0              PUSH BC:PUSH HL:LD A,H
                  SUB 164:LD D,A:LD E,L
MNUWDT            LD BC,0:DEC E
FBYTE             LD A,0:INC E:CPI
                  JR Z,PRCU:LD A,(IX+4)
PRC1              LD (DE),A:JP PE,FBYTE
                  POP HL:LD BC,32:ADD HL,BC
                  POP BC:DJNZ PRC0:RET
PRCU              LD A,0:JR PRC1
HELP              LD HL,MT
                  LD E,1:LD A,(IX+9)
                  SUB (IX+4)
                  CPL:AND #38:LD (HC+1),A
HE0               LD A,(HL):CP #FF:JR Z,HEX
                  INC HL:LD A,(HL)
                  OR A:JR Z,NEXTH
                  PUSH HL:PUSH AF
                  CALL CATAD5:LD A,E
                  LD B,H:CPIR
                  LD A,H:SUB 164:LD B,A
                  LD C,L:POP AF:LD L,A
                  LD H,0:ADD HL,BC:DEC HL
                  DEC HL:LD A,(HL):AND #57
HC                OR 0:LD (HL),A:POP HL
NEXTH             LD BC,8:ADD HL,BC
                  INC E:JR HE0
HEX               LD A,#FF:LD (OLDC+1),A
HEX0              XOR A:IN A,(#FE):OR #E0
                  INC A:RET Z:JR HEX


