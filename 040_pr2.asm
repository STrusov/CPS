TL EQU 1
NL EQU 22
                  ORG #FC00
TXT EQU 25000
END EQU 30000
                  LD HL,#4000:LD DE,#4001
                  LD BC,#1800:LD (HL),0
                  LDIR:LD BC,#2FF
                  LD (HL),#46:LDIR
                  XOR A:OUT (#FE),A
                  LD HL,END:LD (HL),#D
                  INC HL:LD (HL),#D
                  LD IX,COORD
                  LD IY,#5C3A
                  LD HL,TXT:LD (TLN),HL
                  CALL PRPAGE
LOOP              BIT 5,(IY+1):JR Z,LOOP
                  HALT
                  LD A,#10:OUT (#FE),A
                  LD B,#40:DJNZ $:XOR A:OUT (#FE),A
                  LD A,(23560):RES 5,(IY+1)
                  LD HL,KEYT
KSCAN             BIT 7,(HL):JR NZ,LOOP
                  CP (HL):INC HL
                  LD E,(HL):INC HL
                  LD D,(HL):INC HL
                  JR NZ,KSCAN
                  EX DE,HL:CALL JPH:JR LOOP
JPH               JP (HL)

TABAD             DEFW #0004,#0102,#0200
                  DEFW #0206,#0304,#0402
                  DEFW #0500,#0506,#0604
                  DEFW #0702,#0800,#0806
                  DEFW #0904,#0A02,#0B00
                  DEFW #0B06,#0C04,#0D02
                  DEFW #0E00,#0E06,#0F04
                  DEFW #1002,#1100,#1106
                  DEFW #1204,#1302,#1400
                  DEFW #1406,#1504,#1602
                  DEFW #1700,#1706,#1804
                  DEFW #1902,#1A00,#1A06
                  DEFW #1B04,#1C02,#1D00
                  DEFW #1D06,#1E04
APR               DEFW PRCH0,PRCH2,PRCH4,PRCH6

PRCH0             LD A,(DE):LD C,0
                  RRA:RR C
                  OR (HL):LD (HL),A
                  INC L:LD (HL),C:DEC L
                  INC DE:INC H:DJNZ PRCH0
                  POP BC:POP HL:RET
PRCH2             LD A,(DE):LD C,0
                  RRA:RR C:RRA:RR C
                  RRA:RR C
                  OR (HL):LD (HL),A
                  INC L:LD (HL),C:DEC L
                  INC DE:INC H:DJNZ PRCH2
                  POP BC:POP HL:RET
PRCH4             LD A,(DE):LD C,0
                  RLA:RL C:RLA:RL C
                  RLA:RL C
                  INC L:LD (HL),A:DEC L
                  LD A,C:OR (HL):LD (HL),A
                  INC DE:INC H:DJNZ PRCH4
                  POP BC:POP HL:RET
PRCH6             LD A,(DE):LD C,0
                  RLA:RL C
                  INC L:LD (HL),A:DEC L
                  LD A,C:OR (HL):LD (HL),A
                  INC DE:INC H:DJNZ PRCH6
                  POP BC:POP HL:RET

COORD             DEFW 0
PRCH              PUSH HL:PUSH BC
                  LD L,A:LD H,0
                  LD BC,#AB78:ADD HL,HL
                  ADD HL,HL:ADD HL,HL
                  ADD HL,BC:EX DE,HL
                  LD A,(IX):ADD A,A
                  LD HL,TABAD:ADD A,L
                  LD L,A:LD C,(HL)
                  INC HL:LD B,(IX+1)
                  LD A,B:RRCA:RRCA:RRCA
                  AND #E0:OR (HL):LD L,A
                  LD A,B:AND #18:OR #40
                  LD B,H:LD H,A
                  LD A,0-APR/256*256+APR
                  ADD A,C:LD C,A:LD A,(BC)
                  LD (JPA+1),A:INC C
                  LD A,(BC):LD (JPA+2),A
                  LD B,8:INC (IX)
JPA               JP 0
KEYT              DEFB #7:DEFW EXIT
                  DEFB #4:DEFW PGUP
                  DEFB #8:DEFW PGUP
                  DEFB "6:DEFW PGUP
                  DEFB "o:DEFW PGUP
                  DEFB #5:DEFW PGDN
                  DEFB #9:DEFW PGDN
                  DEFB "7:DEFW PGDN
                  DEFB "p:DEFW PGDN
                  DEFB #B:DEFW LNUP
                  DEFB "9:DEFW LNUP
                  DEFB "q:DEFW LNUP
                  DEFB #A:DEFW LNDN
                  DEFB "8:DEFW LNDN
                  DEFB "a:DEFW LNDN
                  DEFB 255
EXIT              POP HL:RET
PGDN              LD HL,(TLN):LD B,22
PGDN0             CALL ADDN:LD (TLN),HL
;                 RET C:JR PRPAGE
ADDN              LD A,#D:CP (HL):JR NZ,ADDN0
                  INC HL
ADDN0             OR A:LD C,#FF:CPIR:JR NZ,ADDN1
                  DJNZ ADDN0:DEC HL
                  LD BC,END-3:SBC HL,BC
                  ADD HL,BC:RET C
ADDN1             LD HL,END-3:RET

PGUP              LD HL,(TLN):LD B,23
                  CALL ADUP:JR PGDN0+3
LNDN              LD HL,(TLN):LD B,1
                  CALL ADDN:LD E,L:LD D,H
                  LD B,21:CALL ADDN:RET NC
                  PUSH HL:LD (TLN),DE
                  CALL SCUP:POP HL
                  LD (IX+1),NL+TL-1
                  JR LNPP
PRPAGE            LD (IX+1),TL
TLN EQU $+1:LD HL,0
PRP               LD C,NL:CALL PRLINE
                  INC (IX+1):DEC C
                  JR NZ,PRP+2:RET
PRLINE            LD (IX),0
                  LD A,(IX+1):AND #18
                  OR #40:LD D,A:LD A,(IX+1)
                  RRCA:RRCA:RRCA:AND #E0
                  LD E,A:LD B,8:XOR A
PRL0              LD (DE),A:INC D:DJNZ PRL0
PRL               LD A,(HL):INC HL:CP 13
                  JR Z,FL:CALL PRCH:JR PRL
FL                LD A,(IX):CP 41:RET NC
                  LD A,32:CALL PRCH:JR FL
LNUP              LD HL,(TLN):LD B,2
                  CALL ADUP:RET C:PUSH HL
                  LD (IX+1),TL:CALL SCDN
                  POP HL:LD (TLN),HL
LNPP              LD A,(HL):CP #D:JR NZ,PRLINE
                  INC HL:JR PRLINE
ADUP              LD A,#D
LNUP0             OR A:LD C,#FF:CPDR:JR NZ,LNUP1
                  DJNZ LNUP0:INC HL
                  LD BC,TXT:SBC HL,BC
                  ADD HL,BC:RET NC
LNUP1             LD HL,TXT:CCF:RET

SCUP              LD HL,#4040:LD DE,#4020
                  LD A,NL
SCUP0             PUSH AF:CALL CPLN
                  LD A,L:ADD A,32:LD L,A
                  JR NC,SCUP1:LD A,H
                  ADD A,8:LD H,A
SCUP1             LD A,E:ADD A,32:LD E,A
                  JR NC,SCUP2:LD A,D
                  ADD A,8:LD D,A
SCUP2             POP AF:DEC A:JR NZ,SCUP0:RET
SCDN              LD HL,#50A0:LD DE,#50C0
                  LD A,NL
SCDN0             PUSH AF:CALL CPLN
                  LD A,L:SUB 32:LD L,A
                  JR NC,SCDN1:LD A,H
                  SUB 8:LD H,A
SCDN1             LD A,E:SUB 32:LD E,A
                  JR NC,SCDN2:LD A,D
                  SUB 8:LD D,A
SCDN2             POP AF:DEC A:JR NZ,SCDN0:RET
CPLN              LD BC,#900
                  PUSH HL:PUSH DE
CPLN0             PUSH HL:PUSH DE
                  LDI:LDI:LDI:LDI
                  LDI:LDI:LDI:LDI
                  LDI:LDI:LDI:LDI
                  LDI:LDI:LDI:LDI
                  LDI:LDI:LDI:LDI
                  LDI:LDI:LDI:LDI
                  LDI:LDI:LDI:LDI
                  LDI:LDI:LDI:LDI
                  POP DE:POP HL
                  INC H:INC D:DJNZ CPLN0
                  POP DE:POP HL:RET


