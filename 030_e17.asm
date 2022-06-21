;INCLUDE M7
OCGNT             LD E,A:PUSH AF
PREL1             LD (IX),2:LD (IX+1),15
                  LD (IX+4),6
                  LD A,1:LD (GN0-1),A
                  LD A,#D3:LD (ADCR+1),A
                  LD HL,(LNADR):PUSH HL
                  LD HL,TNT:LD (LNADR),HL
                  LD HL,TAMC2
                  LD (ATAMC+1),HL
                  LD HL,LINE:LD A,(HL)
                  PUSH AF:PUSH HL:LD (HL),0
                  LD A,4:LD (MCCX+1),A
                  LD A,E:CALL CGNT
                  POP HL:POP AF:LD (HL),A
                  LD A,10:LD (MCCX+1),A
                  LD HL,TAMC
                  LD (ATAMC+1),HL
                  LD A,64:LD (GN0-1),A
                  LD A,#60:LD (ADCR+1),A
                  POP HL:LD (LNADR),HL
                  LD HL,TNT
                  XOR A:LD (PRZL),A
                  LD A,(HL):CALL PRENV
                  INC HL:LD A,(HL)
                  CALL PRNOTE:INC HL
                  LD A,(HL):CALL PRENV
                  LD A,18:LD (PRZL),A
                  POP AF:RET
NHP               RRC C:RET NC:INC E:RET
VVR               DJNZ VVR0:RET
VVR0              LD A,(DE):OR C
XX                NOP:LD (DE),A
                  CALL UPDE:DJNZ VVR0:RET
UPDE              LD A,D:DEC D:AND 7:RET NZ
                  LD A,E:SUB 32:LD E,A:RET C
                  LD A,D:ADD A,8:LD D,A:RET
CLEARM            CALL CLRSAMS:CALL CLRORNS
CLPATS            LD HL,BASE:LD DE,BASE+1
                  LD BC,PATTERNS-BASE-1
                  LD (HL),0:LDIR
                  LD A,(DELAY)
                  LD (POSTAB+3),A
                  SET 3,(IX+10)
                  LD A,(NPAT+1):LD B,A
                  XOR A:LD (NPOS),A
CLPATS0 PUSH BC:PUSH AF
                  CALL CLRPAT:POP AF
                  POP BC:INC A:DJNZ CLPATS0:RET
CLRORNS LD HL,CLRORN:JR CLRIN
CLRSAMS LD HL,CLRSAM
CLRIN             LD (CSO+2),HL:LD A,15
CSO               PUSH AF:CALL 0:POP AF
                  DEC A:JR NZ,CSO:RET
CLRSAM            CALL GSAMAD+3:LD HL,(SAMAD)
                  LD BC,482
CLRR              LD D,H:LD E,L:INC DE
                  LD (HL),0:LDIR:RET
CLRORN            CALL GORNAD+3:LD HL,(ORNAD)
                  LD BC,122:JR CLRR
CLRPAT            CALL GPATAD
                  LD A,(PSIZE):LD (HL),A
                  INC HL:LD D,H:LD E,L
                  INC DE:LD BC,64
                  LD (HL),B:LDIR:INC DE
                  LD (HL),104:INC HL
                  LD (HL),B:LD BC,382
                  DEC HL:LDIR:RET
SPOS              LD A,(NPOS):LD (NP+1),A
TPOS              CP #FF:RET Z
                  LD (TPOS+1),A
                  CALL GPOSAD
                  LD (TPOSAD+1),HL
                  LD DE,DISPA
                  LD BC,5:LDIR:RET
GPOSAD            LD C,A:LD B,0
                  LD L,A:LD H,B
                  ADD HL,HL:ADD HL,HL
                  ADD HL,BC
                  LD BC,POSTAB
                  ADD HL,BC:RET
SPAT              LD A,(NPAT)
TPAT              CP #FF:RET Z
                  SET 3,(IX+10)
                  LD (TPAT+1),A
                  CALL GPATAD
                  LD (TPATAD+1),HL
                  LD A,(HL):INC HL
                  LD (PSIZE),A
                  BIT 4,(IX+10):RET NZ
                  LD (LNADR),HL
                  XOR A:LD (LINE),A:RET
MKEYS DEFB #A:DEFW NXTLIN
DEFB #B:DEFW PRVLIN
DEFB #9:DEFW MCR
DEFB #8:DEFW MCL
DEFB 255
SMUS              CALL CGNT
                  CALL SPL:CALL SPOS:CALL SPAT
                  LD A,(PSIZE)
TPATAD            LD (0),A
                 INC A:LD (MAXLN+1),A
                  LD HL,DISPA
TPOSAD            LD DE,0:LD BC,5:LDIR
PRFL              LD A,#FF:CP (IX+12)
                  JR Z,SM00:LD A,(IX+12)
                  LD (PRFL+1),A:JR PRPAT
SM00              BIT 3,(IX+10):RET Z
                  LD HL,TAD:LD BC,#700
PRDM              LD E,(HL):INC HL
                  LD D,(HL):INC HL
                  PUSH HL:PUSH BC
                  EX DE,HL:CALL CDAT0
                  POP BC:POP HL
                  DJNZ PRDM
                  RES 3,(IX+10)

PRPAT             LD (IX+1),7
                  LD A,(IX+12):PUSH AF
                  RES 0,(IX+12)
                  LD A,(LOOP)
                  LD (L00+1),A:LD C,A
                  LD A,(NPOS):CP C
                  JR NC,LINE-1
                  LD (LOOP),A
LINE EQU $+1
                  LD A,0
LNADR EQU $+1
                  LD HL,PATTERNS+1
                  CALL MAXLN
                  LD (LINE),A
                  LD (LNADR),HL
                  POP BC:LD (IX+12),B
                  CALL PRVLN:CALL PRVLN
                  CALL PRVLN:LD B,7
PRP0              PUSH BC:PUSH AF:PUSH HL
                  CALL PRPLIN:POP HL
                  POP AF:CALL NXTLN
                  POP BC:PUSH AF:LD A,4
                  CP B:JR NZ,PRP2
L00               LD A,0:LD (LOOP),A
PRP2              POP AF:DJNZ PRP0:RET
CDE               PUSH DE:RET
PRVLIN            LD DE,PRVLN:JR CLIN
NXTLIN            LD DE,NXTLN
CLIN              SET 4,(IX+11)
                  LD A,1:LD (PLD+1),A
CLIN0             BIT 2,(IX+10):RET NZ
                  LD HL,(LNADR)
                  LD A,(NPOS):LD (NP+1),A
                  LD A,(PSIZE):INC A
                  LD (MAXLN+1),A
                  SET 3,(IX+10)
                  LD A,(LINE):CALL CDE
                  LD (LINE),A
                  LD (LNADR),HL:RET C
                  LD A,B:LD (NPOS),A:RET
PRVLN             DEC HL:DEC A:SCF:RET P
                  LD A,(LOOP):LD C,A
                  LD A,(NP+1):INC C
                  BIT 0,(IX+12)
                  JR Z,PVL0:CP C
                  DEC A:JR NC,PVL0
                  LD A,(MLEN)
PVL0              CALL NXL0:LD A,E
                  LD D,0:ADD HL,DE:OR A:RET
NXTLN             INC HL:INC A
MAXLN             CP 64:RET C
                  BIT 0,(IX+12):JR NZ,NXLL
                  LD A,(NP+1):JR NXL1
NXLL              LD A,(MLEN):LD C,A
NP                LD A,0:CP C:INC A
                  JR C,NXL0:LD A,(LOOP)
NXL0              LD (NP+1),A
NXL1              PUSH AF:CALL GPOSAD
                  LD BC,4:ADD HL,BC
                  LD A,(HL):CALL GPATAD
                  LD A,(HL):POP BC:LD E,A
                  INC A:LD (MAXLN+1),A
                  SET 4,(IX+10)
                  INC HL:XOR A:RET
PRPLIN            INC (IX+1):LD (IX),3
                  LD E,A:CALL P630:LD A,E
PRPL              ADD A,64:LD D,0:LD E,A
                  LD A,(HL):CALL PRENV
                  LD B,3:ADD HL,DE:LD E,#7F
PRL0              LD A,(HL):PUSH BC
                  CALL PRNOTE:INC HL
                  LD A,(HL):CALL PRENV
                  ADD HL,DE:POP BC:DJNZ PRL0:RET
PRENV             INC (IX):OR A
                  JR NZ,PHW:LD A,"-
                  CALL PRCH:CALL PRCH
PRE               INC (IX):RET
PHW               LD C,A:RRA:RRA:RRA:RRA
                  CALL PHEX:LD A,C:CALL PHEX:JR PRE
PRNOTE            CP 96:JR NC,PRN0:PUSH HL
                  LD B,0:PUSH AF
                  AND #78:RRA:RRA
                  LD C,A:LD HL,NOTAB
                  ADD HL,BC:LD A,(HL)
                  CALL PRCH:INC HL
                  LD A,(HL):CALL PRCH
                  POP AF:AND 7:LD C,A
                  LD HL,OCTAB:ADD HL,BC
                  LD A,(HL):CALL PRCH
                  POP HL:RET
PRN0              CP 104:JR NC,PRN01
                  LD A,"P:CALL PRCH
                  LD A,"S:CALL PRCH
                  LD A,"E:JP PRCH
PRN01             LD A,"-:CALL PRCH
                  CALL PRCH:JP PRCH
OCTAB             DEFMAC "UCLS1234"
NOTAB             DEFMAC "C-C#D-D#E-F-F#"
                  DEFMAC "G-G#A-A#B-"
GPATAD            LD C,A:LD B,0:ADD A,A
                  LD D,A:LD E,B:LD L,A
                  LD H,E:ADD HL,HL
                  ADD HL,HL:ADD HL,HL
                  ADD HL,HL:ADD HL,HL
                  EX DE,HL:SBC HL,DE
                  ADD HL,BC
                  LD BC,PATTERNS
                  ADD HL,BC:RET
TAD DEFW MT,MT+45,MT+72,MT+99
DEFW MT+135,MT+144,MT+153
SPL               LD HL,MLEN:LD A,(HL)
                  DEC HL:CP (HL)
                  LD DE,MT+189
                  PUSH AF:CALL C,SPL0
                  LD HL,NPOS
                  POP AF:CP (HL):RET NC
SPL0              INC (HL):JR Z,SPL1+1
SPL1              LD (HL),A:EX DE,HL
                  LD C,0:JP CDAT0
MCR               LD C,1:JR MCC
MCL               LD C,#FF
MCC               LD A,2:ADD A,C
MCCX              LD B,10:JP P,MCC0:LD A,B
MCC0              INC B:CP B:JR C,MCC1:LD A,2
MCC1              LD (MCC+1),A:CP
MCC2              LD HL,0:LD D,0:LD (HL),D
MCC3              LD A,0:INC A:BIT 2,A
                  LD (MCC3+1),A:RET NZ
ATAMC             LD HL,TAMC
                  LD A,(MCC+1):LD E,A
                  ADD HL,DE:LD E,(HL)
ADCR              LD HL,#4F60:ADD HL,DE
                  LD (HL),#FF
                  LD (MCC2+1),HL:RET
CGNT              LD DE,MCC2:LD HL,MKEYS
                  PUSH AF:CALL KEYS:POP AF
GN                OR A:RET Z:LD C,A
                  LD A,(MCC+1):OR A:SCF
                  LD HL,(LNADR):JR Z,SSAM
                  DEC A:JR Z,SORN
                  LD DE,(LINE):LD D,0
                  ADD HL,DE:LD E,64
GN0               ADD HL,DE
                  DEC A:JR Z,SNOTE
                  DEC A:JR Z,SSAM
                  DEC A:JR Z,SORN
                  LD E,128:JR GN0
TAMC              DEFB 6,7,9,13,14,16,20,21,23,27,28
TAMC2             DEFB 16,17,19,23,24
SORN              LD DE,#00F0:JR SINS
SSAM              LD DE,#010F
SINS              JR C,SINS1
                  LD A,(HL):CP 96:RET NC
                  INC HL
SINS1             LD A,C:CP "9"+1:JR C,SINS10
                  CP "a":RET C
                  SUB "a"-"9"-1
SINS10            SUB #30:RET M
                  CP #10:RET NC
                  SET 5,(IX+10)
                  DEC D:JR NZ,SINS0
                  ADD A,A:ADD A,A
                  ADD A,A:ADD A,A
SINS0             LD C,A:LD A,(HL):AND E
                  OR C:LD (HL),A:JR GE3
SNOTE             EX DE,HL:LD HL,NOKEYS-1
                  LD B,14:LD A,C
GNO0              INC HL:CP (HL):INC HL
                  JR Z,GNO1:DJNZ GNO0:RET
GNO1              LD A,(NOCT):OR (HL)
                  SET 5,(IX+10)
                  LD (DE),A:INC DE
                  CP 96:JR C,GNO2
                  XOR A:JR PREL-1
GNO2              LD A,(NSAM):OR A
                  JR Z,NOSAM:ADD A,A
                  ADD A,A:ADD A,A:ADD A,A
                  LD C,A:LD A,(DE):AND #F
                  OR C:LD (DE),A
NOSAM             LD A,(NORN):OR A:JR Z,PREL
                  LD C,A:LD A,(DE)
                  AND #F0:OR C:LD (DE),A
PREL              LD A,(ENVPROP):OR A:LD C,A
                  JR Z,GE3:DEC DE:LD A,(DE)
                  CP 96:LD D,A:LD A,0:JR NC,GE4
                  LD H,A:LD A,D:CALL DCN
                  ADD A,A:LD L,A:LD B,H
                  LD DE,SOUNDS:ADD HL,DE
                  LD E,(HL):INC HL:LD D,(HL)
GE0               LD HL,EPT-1:ADD HL,BC
                  LD A,(HL):EX DE,HL:LD DE,0
                  ADD HL,HL:LD B,4
GE1               ADD HL,HL:RRA:JR NC,GE2
                  EX DE,HL:ADD HL,DE:EX DE,HL
GE2               DJNZ GE1:LD A,E:SUB #80
                  LD A,D:INC D:JR Z,GE4
                  SBC A,B:ADC A,1
GE4               LD HL,(LNADR):LD (HL),A
GE3               BIT 2,(IX+10):RET NZ
              ;             SET 3,(IX+10)
                  BIT 3,(IX+11):JP Z,PRPAT
              ;             RES 3,(IX+10)
                  LD HL,(LNADR)
                  LD A,(LINE):LD (IX),5
                  LD (IX+1),11:JP PRPL
EPT DEFB 4,3,2,1,12,10,8,6
NOKEYS DEFB "z",0,"s",8,"x",16,"d",24
DEFB "c",32,"v",40,"g",48,"b",56
DEFB "h",64,"n",72,"j",80,"m",88
DEFB "k",96,"l",104
INT PUSH IY:LD IY,#5C3A:RST #38
              PUSH AF:PUSH HL:PUSH DE:PUSH BC
              PUSH IX:EX AF,AF':PUSH AF
              LD IX,VARS:CALL PLAYM
             ;LD A,(XX);PUSH AF
              BIT 6,(IX+11):CALL Z,INDI
             ;POP AF;LD (XX),A:POP AF:EX AF,AF'
POP IX:POP BC:POP DE:POP HL:POP AF
              RES 6,(IX+11):POP IY:RET

GORNAD            LD A,(NORN)
                  SUB 1:ADC A,1
                  LD HL,O-123:LD BC,123
                  LD DE,ORNAD:JR MUL
GADS              CALL GORNAD
GSAMAD            LD A,(NSAM)
                  SUB 1:ADC A,1
                  LD HL,S-483:LD BC,483
                  LD DE,SAMAD
MUL               ADD HL,BC:DEC A:JR NZ,MUL
                  LD (SOAD+1),HL
                  EX DE,HL:LD (HL),E:INC HL
                  LD (HL),D:INC HL:EX DE,HL
                  LD BC,3:PUSH HL:LDIR
                  POP HL:LD DE,XLEN
                  LD C,3:LDIR:RET
EDITPAT LD A,#FF:LD (TPAT+1),A:RET
XLEN DEFB 0;LENGHT
XLP DEFB 0;LOOP
XRL DEFB 0;REP LEN
SAMAD DEFW 0
SAMLEN DEFB 0
SAMLP DEFB 0
SAMRL DEFB 0
ORNAD DEFW 0
ORNLEN DEFB 0
ORNLP DEFB 0
ORNRL DEFB 0
TNT               DEFB 1,4,#11
EDSAM             LD A,(NSAM):OR A:RET Z:JR ED
EDORN             LD A,(NORN):OR A:RET Z
ED                SET 2,(IX+10)
                  CALL STCOOR
                  XOR A:LD (MCC2+2),A
                  LD HL,TNT+1:LD A,(HL)
                  CP 96:JR C,ED0:LD (HL),4
ED0               LD A,#FF:LD (TSAM),A
                  LD (TORN),A
                  LD A,(NSAM):ADD A,A
                  ADD A,A:ADD A,A:ADD A,A
                  LD C,A:LD A,(NORN)
                  OR C:LD (TNT+2),A
                  LD HL,MCC+1:LD A,(HL)
                  LD (MCCS+1),A:LD (HL),2
                  LD HL,#4000:LD DE,#4001
                  LD BC,#0FFF:LD (HL),0:LDIR

EORN              HALT
                  CALL GKEY:CP #7:JP Z,MCCS
                  SET 7,(IX+10)
                  RES 5,(IX+10):PUSH AF
                  CALL OCTSEL:POP AF
                  CALL OCGNT:PUSH AF
                  CALL SETNT
                  LD A,(NSAM)
TSAM EQU $+1
                  CP 0:LD (TSAM),A
                  CALL NZ,PRSCR
                  LD A,(NORN)
TORN EQU $+1
                  CP 0:LD (TORN),A
                  CALL NZ,PRSCR
                  CALL SOEC
;                 CALL PROPR

                  POP AF
                  BIT 5,(IX+10):PUSH AF
                  CALL NZ,PLAYN:POP AF
                  JR NZ,EORN
                  OR A:CALL NZ,BEEP
                  JR EORN

PROPR             LD HL,XLEN:PUSH HL
                  LD A,(HL):OR A:JP P,POPR4
                  LD (TORN),A:LD A,119
POPR4             CP 120:JR C,POPR3
                  LD (TORN),A:XOR A
POPR3             LD (HL),A:INC HL:BIT 7,(HL)
                  JR Z,POPR5:LD (HL),0
POPR5             CP (HL):JR NC,POPR0:LD (HL),A
POPR0             LD B,(HL):INC HL
                  BIT 7,(HL):JR NZ,POPR2-1
                  CP (HL):JR NC,POPR1:LD (HL),A
POPR1             LD A,(HL):CP B
                  JR NC,POPR2:LD (HL),B
POPR2             POP HL:LD (P255+1),HL:JP PRDAT

GOLAX             LD A,(OLN):LD HL,(SODU+1)
                  ADD A,L
GOLA              LD HL,(ORNAD):INC HL
                  INC HL:LD B,A:INC B
GOLA0             INC HL:DJNZ GOLA0:RET

GSLAX             LD A,(OLN):LD HL,(SODU+1)
                  ADD A,L
GSLA              LD HL,(SAMAD):DEC HL
                  LD B,A:INC B
GSLA0             INC HL:INC HL:INC HL
                  INC HL:DJNZ GSLA0:RET
GMSK              PUSH HL:LD HL,#F801:ADD HL,DE
                  LD A,H:AND #F:ADD A,A
                  OR L:CP 2:POP HL:RET C
                  LD A,2:RET
SOUP0             LD HL,OLN:DEC (HL)
                  JP P,MSOL0:INC (HL):RET
SODN0             LD HL,OLN
MSOL              LD A,0:SUB 15:RET M
                  CP (HL):RET C:INC (HL)
MSOL0             SET 1,(IX+11):RET


PRSCR             CALL GADS;SET 7,(IX+10)
OLN EQU $+1
PRSCRX            LD C,0:LD A,(SAMLEN)
                  LD B,A:LD A,(ORNLEN)
                  CP B:JR NC,PROR:LD A,B
PROR              LD (MSOL+1),A:LD B,A
                  LD A,(SODU+1):CP B
                  JR C,PROR00:LD A,B
                  LD (SODU+1),A
PROR00            LD A,C:ADD A,14
                  CP B:JR C,PROR0:LD A,B
                  SUB 14:JR NC,PROR01:XOR A
PROR01            LD (OLN),A:LD C,A
PROR0             LD (IX+1),0:LD A,C
                  LD B,15;SUB 2;7
PROR1             PUSH BC:PUSH AF:CALL PRORLN
                  INC (IX+1):POP AF:INC A
                  POP BC:DJNZ PROR1
STAT              LD HL,#5800:LD DE,#FC00
                  LD BC,#200:LDIR:JR PRSOC
SOSC DEFB 0
SOEC              LD A,#40:IN A,(#FE):OR #E0
                  RES 0,(IX+11)
                  RES 1,(IX+11)
                  LD HL,SOSC:INC A
                  LD B,(HL):LD (HL),A:RET Z
                  INC B:JR NZ,SOEC2:DEC B
SOEC2             LD A,7:LD (HL),B:LD C,B
                  SRL B:SRL B:SRL B:INC B
SOEC1             SRL A:DJNZ SOEC1
                  AND C:RET NZ

                  LD HL,TPK
SOEC0             LD A,(HL):INC A
                  JR Z,PRSOCX:DEC A:INC HL
                  LD B,(HL):INC HL
                  IN A,(#FE):AND B
                  LD E,(HL):INC HL
                  LD D,(HL):INC HL
                  JR NZ,SOEC0:PUSH HL
                  CALL OCTSEL-2:SET 0,(IX+11)
                  XOR A:LD (23554),A
                  LD (23558),A
                  POP HL:JR SOEC0

PRSOCX            BIT 0,(IX+11):RET Z
                  BIT 1,(IX+11)
                  JP NZ,PRSCRX
PRSOC             LD HL,#FC00:LD DE,#5800
                  LD BC,#200:LDIR
                  LD A,(HPSOC+1):OR A
                  LD C,A:JR Z,PRSOC1
                  SUB 7:LD A,B:RLA
                  LD (SOEF+1),A
PRSOC1            SLA C
                  LD HL,TACE:ADD HL,BC
                  LD A,(HL)
                  INC HL:LD B,(HL):LD L,A
                  LD A,(SODU+1):LD H,A
                  CALL CATADR
                  SUB #FC-#58:LD H,A
PRSO0             LD A,(HL):AND #C7:OR 16
                  LD (HL),A:INC L
                  DJNZ PRSO0:RET
TPK
              DEFW #08F7,INCP0
              DEFW #04F7,DECP0
              DEFW #04FB,INCP
              DEFW #02FB,DECP
              DEFW #047F,TGSP
              DEFW #017F,CLRP
              DEFW #01FB,SOUP
              DEFW #01FD,SODN
              DEFW #02DF,SOLE
              DEFW #01DF,SORI
DEFB 255

SOLE              LD C,#FF:JR HPSOC
SORI              LD C,1
HPSOC             LD A,0:ADD A,C
                  JP M,SOLR0:CP 10
                  JR C,SOLR1:XOR A
SOLR1             LD (HPSOC+1),A:RET
SOLR0             LD A,9:JR SOLR1

SOUP              LD C,#FF:JR SODUX
SODN              LD C,1
SODUX             LD A,(MSOL+1):LD B,14
                  CP B:JR NC,SODU:LD B,A
SODU              LD A,0:INC B:ADD A,C
                  JP M,SOUP0
                  CP B:JR C,SODU0:LD A,B:DEC A
SODU0             LD (SODU+1),A
                  JP NC,SODN0:RET

TGSP              LD A,#7F:IN A,(#FE):AND 2:RET NZ
                  LD HL,TTGS:JR IDP
CLRP              LD HL,TCLR:JR IDP
DECP0             LD A,#FE:IN A,(#FE):RRA:RET C
DECP              LD HL,TDEC:JR IDP
INCP0             LD A,#FE:IN A,(#FE):RRA:RET C
INCP              LD HL,TINC
IDP               LD A,(HPSOC+1):ADD A,A
                  LD DE,STAT:PUSH DE
                  LD DE,PRORLNX:PUSH DE
                  LD C,A:LD B,0:ADD HL,BC
                  LD E,(HL):INC HL
                  LD D,(HL):EX DE,HL:JP (HL)
DLEN              LD A,#FF:JR CSOL
ILEN              LD A,1
CSOL              LD IY,SAMAD
SOEF              LD C,0:DEC C:JR Z,CSOL0
                  LD IY,ORNAD
CSOL0             ADD A,(IY+2):CP 120:RET NC
                  LD (IY+2),A;INC A
                  CP (IY+4):JR NC,CSOL1
                  DEC (IY+4)
CSOL1             CP (IY+3):JR NC,EXCC
                  JR EXCCD
DORL              LD A,#FF:JR CORL
IORL              LD A,1
CORL              LD HL,ORNRL:JR CCCO
DOLP              LD A,#FF:JR COLP
IOLP              LD A,1
COLP              LD HL,ORNLP
CCCO              LD IY,ORNAD:JR CCHL
DSRL              LD A,#FF:JR CSRL
ISRL              LD A,1
CSRL              LD HL,SAMRL:JR CCCC
DSLP              LD A,#FF:JR CSLP
ISLP              LD A,1
CSLP              LD HL,SAMLP
CCCC              LD IY,SAMAD
CCHL              LD B,(HL):ADD A,B:RET M
;                 CP 120;RET NC
                  LD (HL),A:CP (IY+2)
                  JR Z,CCHL0
                  JR C,CCHL0:LD (HL),B:LD A,B
CCHL0             CP (IY+4):JR C,CCHL1
                  JR Z,CCHL1:INC (IY+4)
CCHL1             CP (IY+3):JR NC,EXCC
EXCCD             DEC (IY+3)
EXCC              EX (SP),IY:POP HL
                  LD E,(HL):INC HL
                  LD D,(HL):INC HL
                  LD BC,3:LDIR:POP HL
                  SET 1,(IX+11):RET

TINC              DEFW ILEN,ISLP,ISRL
                  DEFW INCT,INCE,INCV
                  DEFW INCN,IOLP,IORL
                  DEFW INCO
TDEC              DEFW DLEN,DSLP,DSRL
                  DEFW DECT,DECE,DECV
                  DEFW DECN,DOLP,DORL
                  DEFW DECO
TCLR              DEFW RADR,RADR,RADR
                  DEFW CLRT,CLRE,CLRV
                  DEFW CLRN,RADR,RADR
                  DEFW CLRO
TTGS              DEFW RADR,MTT,MTT
                  DEFW TGST,TGSE,TGSV
                  DEFW TGSN,RADR,RADR
                  DEFW TGSO
PRORLNX LD A,(SODU+1)
                  LD HL,OLN
                  LD (IX+1),A:ADD A,(HL)
;             LD (IX+4),#47
PRORLN            LD E,A
                  LD (IX),0;OR A;JP M,PRSPL
                  LD (IX+4),#47:LD D,A
                  LD A,(MSOL+1):CP E
                  LD B,31:JP C,PRSPL0
                  PUSH DE
                  LD A,E:CALL P255L
                  LD A,(SAMLEN):CP E
                  JP C,PRFSL:LD D," "
                  LD A,(SAMLP):CP E
                  JR NZ,POL0:LD D,"("
POL0              LD A,D:CALL PRCH:LD D," "
                  LD A,(SAMRL):CP E
                  JR NZ,POL1:LD D,")"
POL1              LD A,D:CALL PRCH
                  LD A,E:CALL GSLA
                  LD E,(HL):INC HL
                  LD D,(HL):INC HL:PUSH DE
                  CALL GMSK:PUSH AF
                  LD B,A:ADD A,A
                  ADD A,B:INC A:OR #40
                  LD (VCOL+3),A
                  POP AF:JR NC,POL10
                  LD A,"-:LD B,5
POL11             CALL PRCH:DJNZ POL11:JR POL12
POL10             CALL PSW
POL12             LD A,(HL):BIT 6,A:PUSH AF
                  INC HL:LD A,(HL):OR A
                  LD E,"+:JP P,POL13
                  LD E,"-:NEG
POL13             LD D,A:LD A,E:CALL PRCH
                  LD A,D:CALL PHW:DEC (IX)

                  POP AF:POP DE:PUSH DE:PUSH AF
;                 LD (IX+4),#45
VCOL              LD (IX+4),0
                  LD A,0:SCF:JR NZ,POL130+1:OR A
;                 LD (IX+4),#41
POL130            LD A,D:CALL DR16
                  LD (IX+4),#47
                  POP AF:POP DE:PUSH AF
                  LD A," :JR Z,POL14
                  LD A,D:RRA:RRA:RRA:RRA
                  AND #F:CALL P15+4:JR POL4P
POL14             CALL PRCH
POL4P             POP AF:BIT 5,A
                  LD (IX+4),#41:JR Z,POL4P0
                  LD (IX+4),#47
POL4P0            CALL DR32

POL4              POP AF:LD E,A:LD D," "
                  LD (IX+4),#47
;                LD (IX),26
                  LD A,(ORNLEN):CP E
                  JR C,PRFOL:LD A,E
                  LD A,(ORNLP):CP E
                  JR NZ,POL40:LD D,"("
POL40             LD A,D:CALL PRCH:LD D," "
                  LD A,(ORNRL):CP E
                  JR NZ,POL41:LD D,")"
POL41             LD A,D:CALL PRCH
                  LD A,E:CALL GOLA
                  LD A,(HL):AND #7F
                  JP PSGNL
PRFSL             LD B,23:CALL PRSPL0
                  JR POL4
PRFOL             LD B,5
PRSPL0            LD A," :CALL PRCH
                  DJNZ PRSPL0:RET

DR32H             PUSH AF:LD A,#48
                  CALL DR16S:POP AF:SUB #40
                  JR DR16S
DR32              AND #1F:ADD A,A:ADD A,A
                  CP #40:JR NC,DR32H
                  CALL DR16S:LD A,#44:JR DR16S
DR16              RRA:RRA
DR16S             AND #7C:LD E,A:LD D,0
                  LD HL,TABVL:ADD HL,DE
                  CALL SCRAD:LD C,4
DR160             LD B,7
DR161             LD A,(HL):INC D
                  LD (DE),A
                  DJNZ DR161:INC (IX)
                  LD A,#AA:LD (DE),A
                  LD A,D:SUB 7:LD D,A
                  LD B,D:LD A,D
                  RRA:RRA:RRA:AND 3
                  OR #58:LD D,A
                  LD A,(IX+4):LD (DE),A
                  INC E:INC HL:DEC C
                  LD D,B:JR NZ,DR160:RET

TABVL             DEFB #AA,#AA,#AA,#AA
                  DEFB #EA,#AA,#AA,#AA
                  DEFB #FA,#AA,#AA,#AA
                  DEFB #FE,#AA,#AA,#AA
                  DEFB #FF,#AA,#AA,#AA
                  DEFB #FF,#EA,#AA,#AA
                  DEFB #FF,#FA,#AA,#AA
                  DEFB #FF,#FE,#AA,#AA
                  DEFB #FF,#FF,#AA,#AA
                  DEFB #FF,#FF,#EA,#AA
                  DEFB #FF,#FF,#FA,#AA
                  DEFB #FF,#FF,#FE,#AA
                  DEFB #FF,#FF,#FF,#AA
                  DEFB #FF,#FF,#FF,#EA
                  DEFB #FF,#FF,#FF,#FA
                  DEFB #FF,#FF,#FF,#FE
TABVE             DEFB 0,0,0,0
TABV0             DEFB #AA,#AA,#AA,#AA
                  DEFB #FF,#FF,#FF,#FF

TACE              DEFB 0,3,3,1,4,1,5,5
                  DEFB 10,3,13,5;17,1
                  DEFB 18,8,26,1,27,1,28,3

INCE              LD C,1:JR CE
DECE              LD C,#FF
CE                LD D,#10:CALL CSSH
                  CALL GSLAX
                  INC HL:INC HL:INC HL
                  LD A,(HL):ADD A,C
                  LD (HL),A:RET
GDE               PUSH BC:CALL GSLAX:LD E,(HL)
                  INC HL:LD D,(HL):POP BC:RET
MTT                CALL GDE:CALL GMSK
                  JR C,MT1:LD DE,#801
MT1               OR A:JR Z,MT2:DEC DE:JR CT1
MT2               LD DE,0:JR CT1
CSSH              LD A,#BF:IN A,(#FE)
                  RRA:RET C:XOR A
CSSH0             ADD A,C:DEC D:JR NZ,CSSH0:LD C,A:RET
INCT              LD BC,1:JR CT
DECT              LD BC,#FFFF
CT                LD D,40:CALL CSSH
                  CALL GDE:CALL GMSK:RET C
CT0               EX DE,HL
                  ADD HL,BC:EX DE,HL
                  CALL GMSK:JR C,CT0
CT1               LD A,(HL):AND #F0
                  LD C,A:LD A,D:AND #F:OR C
                  LD (HL),A:DEC HL
                  LD (HL),E:RET
CLRT              CALL GSLAX:LD (HL),0
                  LD A,#F0
CLRT0             INC HL:AND (HL)
                  LD (HL),A:RET
CLRV              CALL GSLAX:LD A,#F:JR CLRT0
CLRN              CALL GSLAX:INC HL
                  LD A,#E0:JR CLRT0
CLRE              CALL GSLAX:INC HL:INC HL
                  INC HL:LD (HL),0:RET
TGSE              CALL GDE:INC HL:INC HL
                  LD A,(HL):NEG:LD (HL),A:RET
TGST              CALL GDE
                  LD HL,0:SBC HL,DE:OR A
                  SBC HL,DE:LD C,L:LD B,H:JR CT
INCV              LD C,16:JR CV
DECV              LD C,#F0
CV                CALL GSLAX:INC HL
                  LD A,(HL):ADD A,C
                  LD (HL),A:RET
INCN              LD C,8:JR CN
DECN              LD C,#F8
CN                CALL GSLAX:INC HL:INC HL
                  LD A,(HL):RLCA:RLCA:RLA
                  ADD A,C:SCF:RRA:RRCA:RRCA
                  LD (HL),A:RET
TGSN              LD C,#20:JR TGSV+2
TGSV              LD C,#40
                  CALL GSLAX:INC HL:INC HL
                  LD A,(HL):XOR C:LD (HL),A:RET

INCO              LD C,1:JR CO
DECO              LD C,#FF
CO                LD D,12:CALL CSSH
                  CALL GOLAX:LD A,(HL):ADD A,C
CO0               ADD A,A:RL (HL):RRA
                  LD (HL),A:RET
CLRO              CALL GOLAX:LD (HL),0:RET
TGSO              CALL GOLAX:LD A,(HL)
                  NEG
                  AND #7F:LD (HL),A:RET
SETNT             LD HL,TNT+2
SETINS            LD A,(HL):AND #F
                  JR Z,SE0:LD (NORN),A
SE0               LD A,(HL):RRA:RRA:RRA
                  RRA:AND #F:RET Z
                  LD (NSAM),A:RET

PLAYN             LD DE,PLAYDAT
                  BIT 3,(IX+11):RET NZ
                  BIT 2,(IX+10)
                JP NZ,PLAYN1
PLAYNN            PUSH DE:POP IY
                  LD HL,(LNADR)
                  LD BC,(LINE):LD B,0
                  LD A,(HL):OR A:JR Z,PLAYN20
                  LD (ENVB+1),A
PLAYN20 ADD HL,BC:LD C,64:LD A,3
PLAYN2            PUSH AF:ADD HL,BC:PUSH HL
                  PUSH DE:LD A,(HL)
                  CP 104:JR NC,PLAYN3
                  INC DE:LDI:LD A,(HL)
                  PUSH AF:AND #F0:JR NZ,PLL0
                  LD A,(DE):AND #F0
PLL0              LD (PLL1+1),DE:INC DE
                  LD (PLL4+1),A
                  RRA:RRA:RRA:LD C,A
                  LD HL,SAMT
                  ADD HL,BC:LD A,(HL)
                  INC HL:LD H,(HL):LD L,A
                  LD (DE),A:INC DE
                  LD A,H:LD (DE),A:INC DE
                  LD C,3:LDIR:XOR A
                  LD (DE),A:INC DE
                  POP AF:AND #F:JR NZ,PLL3
PLL1              LD A,(0):AND #F
PLL3              LD (PLL5+1),A
                  ADD A,A:LD C,A
                  LD HL,ORNT:ADD HL,BC
                  LD A,(HL):INC HL
                  LD H,(HL):LD L,A
                  LD (DE),A:INC DE:LD A,H
                  LD (DE),A:INC DE
                  LD C,3:LDIR:XOR A
                  LD (DE),A:INC DE
PLL4              LD A,0
PLL5              OR 0:LD HL,(PLL1+1):LD (HL),A
PLAYN3            POP HL:LD C,18:ADD HL,BC
                  EX DE,HL:POP HL:POP AF
                  LD C,128
                  DEC A:JP NZ,PLAYN2
                  JR PLAYN0
PLAYN1            LD HL,TNT:LD BC,3
                  LD A,(HL):OR A:JR NZ,PLAYN10
                  LD A,(ENVB+1):LD (HL),A
PLAYN10 LDIR:LD (ENVB+1),A
                  LD HL,SAMAD:LD C,5
                  LDIR:XOR A:LD (DE),A
                  INC DE:LD C,5:LDIR
                  LD (DE),A:DEC A:INC DE
                  INC DE:INC DE:LD (DE),A
                  EX DE,HL:LD DE,16
                  ADD HL,DE:LD (HL),A
PLAYN0            BIT 3,(IX+11):RET NZ
PLA               LD A,(IX+10):LD (SIX+3),A
                  RES 2,(IX+10):HALT
SIX               LD (IX+10),0:CALL PLN
                  LD A,(CALLS):OR A
                  JR Z,PLA00
                  CALL PAUSE:CALL PLN
PLA00             XOR A
                  LD (23554),A:LD (23558),A
                  IN A,(#FE):OR #E0:INC A:JR NZ,PLA
MUTE              LD HL,PLAYDAT+3:LD DE,7
                  LD BC,FS:LD A,3
MU0               LD (HL),C:INC HL
                  LD (HL),B:INC HL
                  LD (HL),D:INC HL
                  LD (HL),D:INC HL
                  LD (HL),D:INC HL
                  LD (HL),D:INC HL
                  LD (HL),C:INC HL
                  LD (HL),B:INC HL
                  LD (HL),D:INC HL
                  LD (HL),D:INC HL
                  LD (HL),D:INC HL
                  LD (HL),D:ADD HL,DE
                  DEC A:JR NZ,MU0
MUT0              LD HL,TONE:LD DE,TONE+1
                  LD BC,10:LD (HL),B:LDIR
                  JP OUTS1
PAUSE             LD BC,1120:SET 6,(IX+11)
P00               DEC BC:LD A,B:OR C
                  JR NZ,P00:RET
PLAYM             BIT 2,(IX+10)
                  JP NZ,SPLM
                  BIT 3,(IX+11):RET Z
PLD               LD A,1:DEC A
                  LD (PLD+1),A:JR NZ,PLD0
                  LD A,(DELAY):OR A
                  JR NZ,PLD1
PLD2              LD A,5
PLD1              LD (PLD2+1),A:LD DE,PLAYDAT
                  LD (PLD+1),A:CALL PLAYNN
                  BIT 4,(IX+11):RES 4,(IX+11)
                  LD DE,NXTLN:CALL Z,CLIN0
PLD0              RES 5,(IX+10)
                  LD A,(CALLS):OR A:JR Z,PLN
                  CALL PLN
                  BIT 5,(IX+11):CALL NZ,PAUSE
PLN               LD IY,PLAYDAT+36:LD B,3
                  LD HL,TONE+4:LD DE,VOLM+2
PLNR              LD (PLN1+1),HL
                  LD (PLN2+1),DE
                  PUSH HL:PUSH DE:PUSH BC
                  LD HL,DISPA-VOLM:ADD HL,DE
                  LD (PLN01+1),HL
                  LD (IY+15),#FE
                  LD A,(IY+1):LD L,(IY+9)
                  CP 96:JR C,PLN00
                  LD HL,(PLN2+1):LD (HL),0
                  LD (IY+15),#FF:JP PLN3
DCN               LD E,A:RRA:RRA:RRA
                  AND #F:LD D,A:LD A,E
                  AND 7:ADD A,A:ADD A,A
                  LD E,A:ADD A,A:ADD A,E:ADD A,D:RET
PLN00             CALL DCN
                  LD H,(IY+10):LD E,(IY+14)
                  INC HL:INC HL:INC HL
                  LD D,0:ADD HL,DE:LD L,(HL)
                  SLA L:SRA L:ADD A,L
                  BIT 2,(IX+10):JR NZ,PLN03
PLN01             LD HL,0:ADD A,(HL):AND #7F
PLN03             CP 96:JR C,PLN02:LD A,97
PLN02             ADD A,A:LD L,A:LD H,D
                  LD BC,SOUNDS:ADD HL,BC
                  LD C,(HL):INC HL
                  LD B,(HL):LD L,(IY+3)
                  LD H,(IY+4):LD E,(IY+8)
                  INC HL:INC HL:INC HL
                  ADD HL,DE:ADD HL,DE:ADD HL,DE
                  ADD HL,DE:LD E,(HL)
                  INC HL:LD D,(HL):PUSH HL
                  CALL GMSK:JR C,PLN0
                  EX DE,HL:ADD HL,BC:JR PLN1
PLN0              LD HL,0:OR A:JR NZ,PLN1
                  SET 0,(IY+15)
PLN1              LD (0),HL:POP HL
                  LD A,(HL):INC HL
                  RRA:RRA:RRA:RRA:AND #F
                  LD C,(HL):BIT 6,C
                  JR Z,PLN2:LD (ENVM),A
                  INC HL
ENVB              LD A,0:OR A
                  JR Z,PLN6:ADD A,(HL)
                  LD (ENV),A
PLN6              LD A,#10
PLN2              LD (0),A
                  BIT 5,C:JR Z,PLN3
                  LD A,C:LD (NOISE),A
                  RES 3,(IY+15)
PLN3              LD A,(IY+8):CP (IY+5)
                  LD (IY+16),A
                  JR C,PLN4:LD A,(IY+7)
                  LD (IY+5),A
                  LD A,(IY+6):DEC A
PLN4              INC A:LD (IY+8),A
                  LD A,(IY+14):CP (IY+11)
                  LD (IY+17),A
                  JR C,PLN5:LD A,(IY+13)
                  LD (IY+11),A
                  LD A,(IY+12):DEC A
PLN5              INC A:LD (IY+14),A
                  LD BC,65518:ADD IY,BC
                  POP BC:POP DE:POP HL
                  DEC DE:DEC HL:DEC HL
                  DEC B:JP NZ,PLNR
OUTS              LD HL,CMODE:LD B,0
                  LD A,(IY+15+54)
                  BIT 0,(HL):JR Z,OU0
                  LD (IY+72+10),B
OU0               RLCA:AND (IY+15+36)
                  LD HL,BMODE
                  BIT 0,(HL):JR Z,OU1
                  LD (IY+72+9),B
OU1               RLCA:AND (IY+15+18)
                  LD HL,AMODE
                  BIT 2,(IX+10):JR NZ,OU2
                  BIT 0,(HL):JR Z,OU2
                  LD (IY+72+8),B
OU2               LD (MIX),A
OUTS1             LD A,#D:LD HL,ENVM
                  LD DE,#FFBF:LD C,#FD
                  INC (HL):DEC (HL)
                  JR NZ,OUTS0:DEC A:DEC HL
OUTS0             LD B,D:OUT (C),A
                  LD B,E:OUTD:DEC A
                  JP P,OUTS0:RET
SAMT DEFW FS,S,483+S,2*483+S
DEFW 3*483+S,4*483+S
DEFW 5*483+S,6*483+S
DEFW 7*483+S,8*483+S
DEFW 9*483+S,10*483+S
DEFW 11*483+S,12*483+S
DEFW 13*483+S,14*483+S
DEFW 15*483+S
ORNT DEFW FS,O,123+O,2*123+O
DEFW 3*123+O,4*123+O
DEFW 5*123+O,6*123+O
DEFW 7*123+O,8*123+O
DEFW 9*123+O,10*123+O
DEFW 11*123+O,12*123+O
DEFW 13*123+O,14*123+O
DEFW 15*123+O
;INCLUDE R13


