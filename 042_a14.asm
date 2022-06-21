                  ORG #D280
                  LD A,I:PUSH IX
                 LD (QUIT0+1),SP
                  DI:LD A,#3B:LD I,A:IM 2
                  LD HL,#FFFF:LD (HL),#18
                  LD L,#F4:LD (HL),#C3
                  LD HL,INT:LD (#FFF5),HL
                  LD A,(#5CF6):LD (DRV),A
                  LD (IY-48),1
                  LD (IY-49),35
                  LD (IY+48),0
                  LD IX,VARS:LD (IX+12),1
;                 CALL CTEST
;                 CALL Z,MSTORE
                  LD HL,0:LD (COMLEN),HL
                LD HL,SBUFF
CLB             LD (HL),0:INC L:JR NZ,CLB
                  XOR A:LD (CLB),A
                LD DE,53524
                LD HL,FMS:LD B,3
                PUSH DE:PUSH HL
CMS             LD A,(DE):CP (HL)
                INC HL:INC DE:SCF:JR NZ,CMS1
                OR A:DJNZ CMS
CMS1            POP HL:POP DE:JR NC,MMM
                  RES 0,(IX+12)
                PUSH HL:LD BC,18:LDIR
                POP HL:LD C,18:LDIR
MMM               CALL CLS:EI
                  LD HL,CMN
                  RES 0,(IX+10)
                  CALL PRMENU
MMM1              LD HL,DMN
                  RES 0,(IX+10)
                  CALL PRMENU
                  RES 1,(IX+10)
                  RES 2,(IX+10)
                  CALL CONTR:JR MMM1
RELN EQU M3-PLAYER
PFLN EQU PCA-INSTALL+RELN
PLLN EQU CSOUNDS-INSTALL+RELN
BASE EQU 25000
POSTAB            EQU BASE+2
PATTERNS EQU BASE+1282
S EQU 17960+PATTERNS
O EQU 483*15+S
SOUNDS EQU 123*15+O
END EQU SOUNDS+192
VARS DEFS 13
MT EQU #FF00
FNT EQU 53433
CLCA              LD A,1:LD (NOF),A
                  LD HL,#FC00:LD DE,#FC01
                  LD BC,#1FF:LD (HL),L:LDIR:RET
ATHL              LD E,(HL):INC HL
                  LD D,(HL):INC HL
                  LD (VARS),DE:LD A,E
                  LD (XPOS+3),A
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
PRMENU            CALL ATHL:PUSH HL
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
BBD2              ADD HL,HL:ADD HL,HL
                  ADD HL,HL:XOR A:LD B,14
BBD20             DEC B:JR Z,BBD21:ADD HL,HL
                  ADC A,A:DAA:JR NC,BBD20
                  INC HL:JR BBD20
BBD21             LD H,A:LD A,L:CP 16
                  JR C,BBD22:ADD A,6
BBD22             OR A:DAA:RET
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
KW                CALL GKEY:JR Z,KW:RET
GKEY              LD IY,#5C3A
                  XOR A:BIT 5,(IY+1):RET Z
                  LD A,(23560):RES 5,(IY+1)
BEEP              PUSH AF:LD A,#31:DI
BP0               OUT (#FE),A:LD B,A
                  XOR #30:DJNZ $
                  OUT (#FE),A:POP AF:EI:RET
KEYSM             LD HL,MNUKEYS
KEYS              DEC D:INC D:JR Z,KEYS0:PUSH DE
KEYS0             CP (HL):INC HL:JR Z,GO
                  INC HL:INC HL:BIT 7,(HL):JR Z,KEYS0:RET
GO                LD A,(HL):INC HL:LD H,(HL):LD L,A:JP (HL)
CONTR             HALT:CALL GKEY
                  RES 4,(IX+11):PUSH AF
                  CALL PRLW:POP AF
                  PUSH AF:LD DE,PRCUR
                  CALL KEYSM:POP AF
                  CALL QKEYS:BIT 0,(IX+10)
                  JR Z,CONTR:RET
QUIT              LD HL,QTXT
W1                LD D,0
W2                BIT 2,(IX+10):JR NZ,W2E
                  SET 2,(IX+10):PUSH HL:PUSH DE
                  CALL STCOOR:SCF:POP DE
                  POP HL:CALL PRMENU
                  JP CONTR
W2E               POP HL:RET
QTXT DEFB 12,4,6,5,#57
DEFB " QUIT?",#D,#D
DEFB 255,16,6,#57,12,6,17,6,7*8+#42
DEFB "YES\",4,1,"y:DEFW 0,QUIT0
DEFB "NO",4,1,"n:DEFW 0,NQ:DEFB 255
NQ                SET 0,(IX+10):RET
QUIT0             LD SP,0:CALL SU
;                 CALL CTEST:POP IX
IM 1
;                 JR Z,MREST:RET
CTEST             LD HL,CTEST0
CTES              LD DE,#5B80:PUSH DE
                  LD BC,128:DI:LDIR:RET
CTEST0            LD BC,#7FFD:LD D,#10
                  OUT (C),D:LD A,(#D280)
                  INC D:OUT (C),D
                  CP #21:RET
MSTORE            LD HL,MST:JR CTES
MREST             LD HL,MRST:JR CTES
MST               LD BC,#7FFD:LD A,23
                  OUT (C),A:EXX
                  LD HL,#4000
                  LD DE,#C000
                  LD C,L:LD B,H
                  PUSH DE:PUSH BC
                  LDIR:EXX:DEC A
                  OUT (C),A:LD DE,#1011:EXX
                  POP BC:POP DE:LDIR
                  LD BC,#1280
MST1              EXX:OUT (C),D:EXX:LD A,(HL)
                  EXX:OUT (C),E:EXX:LD (HL),A
                  INC HL:DEC BC:LD A,B
                  OR C:JR NZ,MST1:RET
MRST              XOR A:OUT (#FE),A
                  LD BC,#7FFD:LD A,23
                  OUT (C),A:EXX
                  LD HL,#C000
                  LD DE,#4000
                  LD BC,#1B80:LDIR
                  LD HL,#DC00
                  LD DE,#5C00
                  LD BC,#2400:LDIR
                  EXX:DEC A
                  OUT (C),A:EXX
                  LD HL,#C000
                  LD BC,#4000
                  LDIR:EXX:LD A,#10
                  OUT (C),A:RET
SU                LD D,13:LD C,#FD:XOR A
SU0               LD B,#FF:OUT (C),D
                  LD B,#BF:OUT (C),A
                  DEC D:JP P,SU0:RET
MNUKEYS
DEFB "a:DEFW CDOWN
DEFB "q:DEFW CUP
DEFB "p:DEFW CRIGHT
DEFB "o:DEFW CLEFT
DEFB #E:DEFW HELP
DEFB #4:DEFW DECDAT
DEFB #5:DEFW INCDAT
DEFB #D:DEFW ENTER
DEFB 255
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
                  LD (VARS),DE:POP DE:JP PFL
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
HELP              LD HL,MT:LD E,1
HE0               LD A,(HL):CP #FF:JR Z,HEX
                  INC HL:LD A,(HL)
                  OR A:JR Z,NEXTH
                  PUSH HL:PUSH AF
                  CALL CATAD5:LD A,E
                  LD B,H:CPIR
                  LD A,H:SUB 164:LD B,A
                  LD C,L:POP AF:LD L,A
                  LD H,0:ADD HL,BC:DEC HL
                  DEC HL:LD A,(HL):AND #C7
HC                OR 4+8:LD (HL),A:POP HL
NEXTH             LD BC,8:ADD HL,BC
                  INC E:JR HE0
HEX               LD A,#FF:LD (OLDC+1),A
HEX0              XOR A:IN A,(#FE):OR #E0
                  INC A:RET Z:JR HEX
OBJ DEFB 0,7,9,"COMPOSE            SAMPLES            ORNAMENTSSOUNDS             POSITIONSORNAMENT SAMPLE                      "
ONAME DEFB 0,1,8,"                  "
DRV DEFB 1,4,1,"ABCD"
FAT DEFW BASE,28590,S,7248
DEFW O,1845,SOUNDS,192,BASE,19242
DEFW 0,123,0,483:DEFB 255
INF               LD HL,MF:LD (HL),1
                  PUSH AF:CALL PRLW
                  HALT:POP AF
                  LD HL,ONAME+3:LD BC,8
                  LD DE,#5CDD
                  LDIR:EX DE,HL:LD (HL),"C"
                  LD A,9:JR C,INF1
                  LD A,13:LD (HL),"M"
                  EX DE,HL:INC DE
                  LD HL,FAT:LD BC,4:LDIR
INF1              LD (#5D06),A
IDRV              XOR A:LD (#5D10),A
                  DEC A:LD (23610),A
                  LD A,(DRV):LD C,1
                  CALL DOS:LD C,#18
                  CALL DOS:LD C,#A
                  CALL DOS:INC C:RET
SAVE
COMLEN EQU $+1
                  LD HL,0:LD A,L
                  OR H:RET Z:PUSH HL:SCF
                  CALL INF:POP DE:JR NZ,FEX
                  LD HL,25000+PLLN+194
                  LD DE,(COMLEN)
                  LD A,(PIF):OR A:JR NZ,SAVE0
                  LD BC,PLLN+194
                  SBC HL,BC:EX DE,HL
                  ADD HL,BC:EX DE,HL
SAVE0             LD C,#B:CALL DOS:JR POK
LOAD              OR A:CALL INF:JR Z,NOFL
                  SET 0,(IX+12)
                  RES 1,(IX+12)
                  LD HL,0:LD (COMLEN),HL
                  CALL PRLLL:LD HL,25000
                  XOR A:LD (#5CF9),A
                  LD C,#E:DEC A
DOSM              CALL DOS
POK               LD HL,DOK
PRSS              LD D,12:LD E,(HL):INC HL
                  LD (VARS),DE:CALL PRS
                  BIT 4,(IX+11)
                  CALL Z,PRLW:LD (IX+1),12
KWP               LD B,10:HALT:DJNZ KWP+2
                  CALL KW:PUSH AF
                  LD HL,SPLN:LD (IX),6
                  CALL PRS:RES 7,(IX+10):POP AF:RET
NOFL              LD HL,NOFT:JP PRSS
FEX               LD HL,FEXT:JR NOFL+3
YN                CALL PRSS:CP "y:RET Z
                  POP HL:RET
PRSA              LD D,(HL):INC HL
                  LD E,(HL):INC HL
                  LD (VARS),DE:JP PRS
RSEC              LD D,0:LD BC,#105:LD HL,#5B00
DOS PUSH IY:LD IY,#5C3A:IM 1
              PUSH HL:LD HL,#5CC2
              PUSH AF:PUSH BC:PUSH DE
              CALL SU:POP DE:POP BC:POP AF
              LD (HL),#C3:LD HL,RAI
              LD (#5CC3),HL:LD HL,PRAD
              LD (#5C51),HL:LD HL,DRER
              EX (SP),HL:LD (23613),SP
              CALL #3D13:POP HL:IM 2:POP IY:RET
RAI               EX (SP),HL:PUSH AF
                  LD A,H:CP #D:JR NZ,RAIE
                  LD A,L:CP #6B:JR NZ,RAIE
                  LD HL,(VARS):LD (RAIX+1),HL
                  LD A,(#5CF5)
                  LD HL,RAIT+15:CALL PBN
                  LD A,(#5CF4)
                  LD HL,RAIT+18:PUSH HL
                  CALL PBN:POP HL:LD (HL),58
RAIW              LD HL,RAIT:CALL PRSS
RAIX              LD HL,0:LD (VARS),HL
                  SUB 32:CP "I:JR Z,RAIQ
                  CP "R:JR Z,RAIQ
                  CP 231:JR NZ,RAIW:LD A,"A
RAIQ              POP HL:POP HL:POP HL
                  POP HL:POP HL
                  LD HL,#3F7E:EX (SP),HL
                  JP #3D2F
RAIE              POP AF:EX (SP),HL
RADR              RET
PRAD DEFW RADR
RAIT DEFB 6,#56,"RETRY/IGNORE",58,"000",58,"00?",255
NOFT DEFB 10,#56,"NO SUCH FILE",255
FEXT DEFB 10,#56,"FILE EXISTS",255
DOK DEFB 8,#40+42,"OK. PRESS A KEY.",255
DERR DEFB 0,13,#56,"BREAK",255
               DEFB 3,12,#56,"NO SPACE",255
               DEFB 6,12,#56,"NO DISK",255
               DEFB 4,9,#56,"DIRECTORY FULL",255
DER1 DEFB 11,#56,"DISK ERROR",255
SPLN DEFB #40+40,"                              ",255
NOCOMT DEFB 11,#56,"NO COMPOSE",255
DONT DEFB 8,#56,"IT'S ALREADY DONE",255
WAITT DEFB 12,10,#44+40,"PLEASE WAIT",255
SH DEFB 7,#56,"ARE YOU SURE Y/N ?",255
INAME             CALL FTC
                  LD A,(HL):INC HL:INC HL:INC HL
                  LD E,(HL):INC HL
                  LD D,(HL):INC DE:INC DE
                  LD A,(DE):INC DE
                  LD (INH0+1),DE
                  LD (IKR0+2),A
                  INC HL:INC HL:INC HL
                  LD E,(HL):INC HL:LD H,(HL)
                  CPL:ADD A,E:DEC A:LD L,A
                  CALL CATADR:LD BC,#5C01
                  ADD HL,BC:LD (FLC+1),HL
                  XOR A:LD (INH0+4),A
                  LD A,(IX+9):PUSH AF:RES 7,(IX+11)
                  LD A,(IX+4):LD (IX+9),A
                  CALL CLCUR
INAM              CALL FLC:CALL KW:LD E,A:CALL INCH
                  LD A,E:LD DE,PRDAT:LD HL,IKS
                  CALL KEYS:JR INAM
IKS DEFB 8:DEFW IKL
DEFB 9:DEFW IKR
DEFB #D:DEFW EXI
DEFB #7:DEFW EXI
DEFB #C:DEFW DEL
DEFB 4:DEFW UDL
DEFB 5:DEFW INS
DEFB 255
EXI POP HL:POP HL:POP AF:LD (IX+9),A:JP CLCUR
INS               LD HL,INH0+4:LD A,(IKR0+2)
                  LD D,0:LD E,A:SUB (HL):RET Z
                  DEC A:JR Z,UDL:LD B,D:LD C,A
                  LD HL,(INH0+1):ADD HL,DE
                  DEC HL:LD E,L:LD D,H:DEC HL
                  LDDR:EX DE,HL:JR UDL0+1
DEL               CALL IKL:RET M
UDL               LD HL,INH0+4:LD A,(IKR0+2)
                  LD D,0:LD E,(HL):SUB E:RET Z
                  LD B,D:LD C,A
                  LD HL,(INH0+1):ADD HL,DE
                  LD D,H:LD E,L:INC HL
                  LDIR:EX DE,HL
UDL0              DEC HL:LD (HL)," :SET 7,(IX+11):RET
IKL               LD HL,INH0+4:LD A,(HL)
                  DEC A:RET M:LD (HL),A:RET
INCH              CP 32:RET C:OR A:RET M
                  CP #60:JR C,INH0:SUB 32
INH0              LD HL,ONAME+3
                  LD BC,0:ADD HL,BC:LD (HL),A:SET 7,(IX+11)
IKR               LD HL,INH0+4:LD A,(HL)
IKR0              INC A:CP 8:RET NC:LD (HL),A:RET
FLC               LD BC,#58AC:LD HL,(INH0+4)
                  ADD HL,BC:RES 7,(HL):INC L
                  SET 7,(HL):INC L:RES 7,(HL):RET
CAT               LD A,#FF:LD (GFN+1),A
                  CALL IDRV:LD E,8:CALL RSEC
                  DEC (IX+5):SET 4,(IX+11)
                  LD DE,TIT+8:LD HL,#5BF5
                  CALL C8:LD HL,(#5BE5)
                  CALL BBD2:EX DE,HL:INC HL:INC HL
                  CALL P2N:LD A,D:CALL P2N
                  LD HL,TIT+30:LD A,(#5BE4)
                  LD (MAXF+1),A
                  LD DE,(#5BF4):SUB E
                  CALL PBN:LD HL,TIT+41
                  LD A,E:CALL PBN
                  LD HL,TIT:CALL PRSA
                  INC (IX+8)
CAT2              LD HL,#406:LD (VARS),HL
                  SET 7,(IX+10):LD C,9
CLW0              LD B,20:LD A,32
CLW1              CALL PRCH:DJNZ CLW1:LD A,#D
                  CALL PRCH:DEC C:JR NZ,CLW0
                  LD (IX+1),4
CAT0              CALL GFN:JR C,CATE
                  LD DE,FNAM:PUSH DE
                  INC DE:CALL C8:LD A,"."
                  LD (DE),A:INC DE:CALL C1
                  INC DE:PUSH DE:EX DE,HL
                  LD HL,FAT:LD IY,OBJ+3
CAT3              LD A,(HL):INC A:JR Z,CAT6
                  PUSH DE:PUSH HL:LD B,4
CAT4              LD A,(DE):CP (HL):INC HL:INC DE
                  JR Z,CAT5:POP HL:POP DE
                  LD BC,4:ADD HL,BC
                  LD C,9:ADD IY,BC:JR CAT3
CAT5              DJNZ CAT4:POP HL:POP DE
CAT6              PUSH IY:POP HL:POP DE
                  LD BC,9:LDIR
                  POP HL:CALL PRS
                  LD A,#D:CALL PRCH
                  LD A,(IX+1):CP 12:JR C,CAT0
                  LD HL,MAXF+1:LD A,(GFN+1)
                  INC A:CP (HL):JR NC,CATE
                  LD HL,SCYN:CALL PRSS
                  CP 7:JP NZ,CAT2
CAT1              SET 0,(IX+10):RET
CATE              CALL POK:JR CAT1
DRER              LD IX,VARS
                  LD SP,(QUIT0+1)
                  DEC SP:DEC SP
                  LD HL,DERR:IM 2
                  LD A,(23610):CP #14
                  LD A,0:JR Z,DRER1
                  LD A,(#5D0F)
DRER1             LD BC,DER1-DERR:CPIR:XOR A
                  CALL PRSS
                  BIT 4,(IX+11)
                  RES 4,(IX+11):JR Z,DRER2
                  SET 0,(IX+10)
DRER2             JP CONTR
C8                LD B,7:CALL C1:DJNZ C8+2
C1                LD A,(HL):CP 32:JR C,C10
                  OR A:JP M,C10:CP #60
                  JR C,C11:SUB 32
C11               LD (DE),A:INC HL:INC DE:RET
C10               LD A,"?:JR C11
GFN               LD A,0:INC A:LD (GFN+1),A
MAXF              CP 0:CCF:RET C:LD L,A
                  LD H,0:ADD HL,HL:ADD HL,HL
                  ADD HL,HL:ADD HL,HL
                  LD E,H:LD H,#5B:PUSH HL
                  LD A,(#5CF4):DEC A:CP E:CALL NZ,RSEC
                  POP HL:LD A,(HL):CP 32
                  RET NC:JR GFN
PBN               CALL BBD:LD B,A:CALL P2N2+1
                  LD A,B
P2N               LD C,A:RRA:RRA:RRA:RRA
                  AND #F:OR #30:LD (HL),A
P2N2              INC HL:LD A,C:AND #F
                  OR #30:LD (HL),A:INC HL:RET
TIT DEFB 2,6,#43+40,"NAME",58,"NAMELESS (0000)",#D
DEFB "FILES",58,"000 ERASED",58,"000",255
FNAM DEFB #40+40:DEFS 10:DEFB 32:DEFS 9:DEFB 255
SCYN DEFB 6,#42+40,"MORE...",255
INT               PUSH IY:LD IY,#5C3A:RST #38
PUSH IX:LD IX,VARS
PUSH HL:PUSH DE:PUSH BC
PUSH AF:EX AF,AF':PUSH AF
CALL SCROLL:CALL MUS
POP AF:EX AF,AF'
POP AF:POP BC:POP DE:POP HL
POP IX:POP IY:RET
MUS               BIT 1,(IX+12):RET Z
                  LD A,(MF):PUSH AF
MUS0              CP 1:LD (MUS0+1),A
                  CALL NZ,DOUBLE
                  POP AF:OR A:JP Z,DOUBLE+#E:RET
PIF DEFB 0,2,14,"+ PLAY ROUTINE- PLAY ROUTINE"
MF DEFB 1,2,9,"MUSIC ON MUSIC OFF"
MFS LD HL,MF:JR PIFS+3
PIFS              LD HL,PIF:LD A,(HL):XOR 1
                  LD (HL),A:RET
DMN DEFB 6,2,20,13,40+#43
DEFB #81,"CACOFONY            COMPILER",#D,#D
COORDS
DEFB 255,8,6,#40+40,7,4,24,11,56+#41
DEFB #82,"COMPILE MODULE ",2,10,"m:DEFW 0,COMPILE
DEFB #D,#82+14,"~\",2,10,"r:DEFW PIF,PIFS
DEFB #D,#82,"NAME",58,#88,2,3,"n:DEFW ONAME,INAME
DEFB #D,#82,"LOAD COMPOSE ",4,3,"l:DEFW 0,LOAD
DEFB #D,#82,"SAVE MODULE ",4,3,"s:DEFW 0,SAVE
DEFB #D,#82,"CATALOG ",4,3,"c:DEFW 0,CAT
DEFB " DRIVE",58,#82,"~",2,2,"d:DEFW DRV,0
DEFB #D,#82+9,"~\",2,4,"u:DEFW MF,MFS
DEFB #D,#82,"EXIT COMPILER ",0,0,7:DEFW 0,QUIT:DEFB 255
CMN DEFB 2,18,28,5,48+#42
DEFB 255,20,20,48+#42,20,20,20,20,48+#42
DEFB "MUSIC'S NAME",58,#8F,2,1,0:DEFW 53524,0
DEFB #D,"AUTHOR(S)",58,#8F,2,6,0:DEFW 53542,0
DEFB #D,"LENGHT OF MODULE",58,4,1,0:DEFW 0,0:DEFB 255
FMS DEFB 0,1,#F,"                         "
PRCHAD DEFW PRCH
PRLW              LD HL,PRCHAD
                  BIT 1,(IX+12):JR NZ,PRLW2
                  LD A,1:LD (MF),A
PRLW2             RES 7,(IX+10)
                  LD (#5C51),HL
                  LD HL,#120F
                  LD (VARS),HL
                  LD HL,53527
                  CALL PRLW0
                  LD HL,#1309+3
                  LD (VARS),HL
                  LD HL,53545
                  CALL PRLW0
PRLLL             LD HL,PRCHAD
                  LD IY,#5C3A
                  LD (#5C51),HL
                  LD HL,#1413
                  LD (VARS),HL
                  LD HL,(COMLEN)
                  LD A,(PIF):OR A:JR NZ,PRLLL0
                  LD BC,PLLN+194
                  ADD HL,BC
PRLLL0            LD C,L:LD B,H
                  CALL #2D2B:CALL #2DE3
                  LD HL,BT:JP PRS0
BT DEFB "              ",255
PRLW0             LD B,15
PRLW1             LD A,(HL):RST #10
                  INC HL:DJNZ PRLW1:JP PRDAT
CLS               LD HL,#5AFF:LD DE,#5AFE
                  LD BC,#1AFF:LD (HL),0:LDDR
CLS0              LD B,24:LD DE,GFX:LD C,H
CLS1              LD A,(DE):LD (HL),A
                  INC H:INC DE:DJNZ CLS1
                  LD H,C:INC L:JR NZ,CLS0
                  LD H,#58:LD DE,#5801
                  LD BC,#2FF:LD (HL),8+5
                  LDIR:LD A,1:OUT (#FE),A:RET
GFX
 DEFB 0,#7E,#40,#40,#40,#40,#40,0
 DEFB 0,#7E,#40,#40,#40,#40,#40,0
 DEFB 0,#7E,#40,#40,#40,#40,#40,0
SBUFF EQU #FB00
SCROLL            LD A,0:INC A:AND 15
                  LD (SCROLL+1),A
                  JR NZ,SCR0
TXA               LD HL,TXT:LD A,(HL)
                  OR A:JR NZ,SCR1
                  LD HL,TXT:LD A,(HL)
SCR1              INC HL:LD (TXA+1),HL
                  LD C,A:ADD A,A
                  LD L,A:LD H,0:LD B,H
                  ADD HL,HL:ADD HL,BC
                  LD BC,FNT:ADD HL,BC
                  EX DE,HL:LD HL,SBUFF+32
                  LD B,5
SCR10             PUSH BC
                  LD A,(DE):LD B,4
                  LD C,A:RRA:OR C
SCR11             RLCA:RL C:RRCA:RL C
                  RLA:DJNZ SCR11
                  LD (HL),C:INC L:LD B,4
SCR12             RLCA:RL C:RRCA:RL C
                  RLA:DJNZ SCR12
                  LD (HL),C:LD BC,33
                  INC DE:ADD HL,BC
                  POP BC:DJNZ SCR10
SCR0              LD HL,JTAB:LD DE,#503F
                  LD IY,GFX+24
                  LD A,(HL):OR A:JR NZ,SCR00
                  LD HL,JTAB:LD A,(HL)
SCR00             INC HL:LD (SCR0+1),HL:LD B,A
SCR01             CALL UPDE:DJNZ SCR01
                  LD HL,5*34+SBUFF-1
                  CALL SSLN:LD B,5
SCR2              SLA (HL):DEC L
RL (HL):DEC L:LD C,(IY)
PUSH HL:PUSH DE
RL (HL):EX AF,AF':LD A,(HL):OR C:LD (DE),A:DEC E:DEC L:EX AF,AF'
RL (HL):EX AF,AF':LD A,(HL):OR C:LD (DE),A:DEC E:DEC L:EX AF,AF'
RL (HL):EX AF,AF':LD A,(HL):OR C:LD (DE),A:DEC E:DEC L:EX AF,AF'
RL (HL):EX AF,AF':LD A,(HL):OR C:LD (DE),A:DEC E:DEC L:EX AF,AF'
RL (HL):EX AF,AF':LD A,(HL):OR C:LD (DE),A:DEC E:DEC L:EX AF,AF'
RL (HL):EX AF,AF':LD A,(HL):OR C:LD (DE),A:DEC E:DEC L:EX AF,AF'
RL (HL):EX AF,AF':LD A,(HL):OR C:LD (DE),A:DEC E:DEC L:EX AF,AF'
RL (HL):EX AF,AF':LD A,(HL):OR C:LD (DE),A:DEC E:DEC L:EX AF,AF'
RL (HL):EX AF,AF':LD A,(HL):OR C:LD (DE),A:DEC E:DEC L:EX AF,AF'
RL (HL):EX AF,AF':LD A,(HL):OR C:LD (DE),A:DEC E:DEC L:EX AF,AF'
RL (HL):EX AF,AF':LD A,(HL):OR C:LD (DE),A:DEC E:DEC L:EX AF,AF'
RL (HL):EX AF,AF':LD A,(HL):OR C:LD (DE),A:DEC E:DEC L:EX AF,AF'
RL (HL):EX AF,AF':LD A,(HL):OR C:LD (DE),A:DEC E:DEC L:EX AF,AF'
RL (HL):EX AF,AF':LD A,(HL):OR C:LD (DE),A:DEC E:DEC L:EX AF,AF'
RL (HL):EX AF,AF':LD A,(HL):OR C:LD (DE),A:DEC E:DEC L:EX AF,AF'
RL (HL):EX AF,AF':LD A,(HL):OR C:LD (DE),A:DEC E:DEC L:EX AF,AF'
RL (HL):EX AF,AF':LD A,(HL):OR C:LD (DE),A:DEC E:DEC L:EX AF,AF'
RL (HL):EX AF,AF':LD A,(HL):OR C:LD (DE),A:DEC E:DEC L:EX AF,AF'
RL (HL):EX AF,AF':LD A,(HL):OR C:LD (DE),A:DEC E:DEC L:EX AF,AF'
RL (HL):EX AF,AF':LD A,(HL):OR C:LD (DE),A:DEC E:DEC L:EX AF,AF'
RL (HL):EX AF,AF':LD A,(HL):OR C:LD (DE),A:DEC E:DEC L:EX AF,AF'
RL (HL):EX AF,AF':LD A,(HL):OR C:LD (DE),A:DEC E:DEC L:EX AF,AF'
RL (HL):EX AF,AF':LD A,(HL):OR C:LD (DE),A:DEC E:DEC L:EX AF,AF'
RL (HL):EX AF,AF':LD A,(HL):OR C:LD (DE),A:DEC E:DEC L:EX AF,AF'
RL (HL):EX AF,AF':LD A,(HL):OR C:LD (DE),A:DEC E:DEC L:EX AF,AF'
RL (HL):EX AF,AF':LD A,(HL):OR C:LD (DE),A:DEC E:DEC L:EX AF,AF'
RL (HL):EX AF,AF':LD A,(HL):OR C:LD (DE),A:DEC E:DEC L:EX AF,AF'
RL (HL):EX AF,AF':LD A,(HL):OR C:LD (DE),A:DEC E:DEC L:EX AF,AF'
RL (HL):EX AF,AF':LD A,(HL):OR C:LD (DE),A:DEC E:DEC L:EX AF,AF'
RL (HL):EX AF,AF':LD A,(HL):OR C:LD (DE),A:DEC E:DEC L:EX AF,AF'
RL (HL):EX AF,AF':LD A,(HL):OR C:LD (DE),A:DEC E:DEC L:EX AF,AF'
RL (HL):EX AF,AF':LD A,(HL):OR C:LD (DE),A:DEC E:DEC L:EX AF,AF'
POP DE:CALL UPDE:EX (SP),HL
PUSH BC:LD B,2
SCRL              PUSH HL:PUSH DE:LD C,(IY)
LD A,(HL):OR C:LD (DE),A:DEC E:DEC L
LD A,(HL):OR C:LD (DE),A:DEC E:DEC L
LD A,(HL):OR C:LD (DE),A:DEC E:DEC L
LD A,(HL):OR C:LD (DE),A:DEC E:DEC L
LD A,(HL):OR C:LD (DE),A:DEC E:DEC L
LD A,(HL):OR C:LD (DE),A:DEC E:DEC L
LD A,(HL):OR C:LD (DE),A:DEC E:DEC L
LD A,(HL):OR C:LD (DE),A:DEC E:DEC L
LD A,(HL):OR C:LD (DE),A:DEC E:DEC L
LD A,(HL):OR C:LD (DE),A:DEC E:DEC L
LD A,(HL):OR C:LD (DE),A:DEC E:DEC L
LD A,(HL):OR C:LD (DE),A:DEC E:DEC L
LD A,(HL):OR C:LD (DE),A:DEC E:DEC L
LD A,(HL):OR C:LD (DE),A:DEC E:DEC L
LD A,(HL):OR C:LD (DE),A:DEC E:DEC L
LD A,(HL):OR C:LD (DE),A:DEC E:DEC L
LD A,(HL):OR C:LD (DE),A:DEC E:DEC L
LD A,(HL):OR C:LD (DE),A:DEC E:DEC L
LD A,(HL):OR C:LD (DE),A:DEC E:DEC L
LD A,(HL):OR C:LD (DE),A:DEC E:DEC L
LD A,(HL):OR C:LD (DE),A:DEC E:DEC L
LD A,(HL):OR C:LD (DE),A:DEC E:DEC L
LD A,(HL):OR C:LD (DE),A:DEC E:DEC L
LD A,(HL):OR C:LD (DE),A:DEC E:DEC L
LD A,(HL):OR C:LD (DE),A:DEC E:DEC L
LD A,(HL):OR C:LD (DE),A:DEC E:DEC L
LD A,(HL):OR C:LD (DE),A:DEC E:DEC L
LD A,(HL):OR C:LD (DE),A:DEC E:DEC L
LD A,(HL):OR C:LD (DE),A:DEC E:DEC L
LD A,(HL):OR C:LD (DE),A:DEC E:DEC L
LD A,(HL):OR C:LD (DE),A:DEC E:DEC L
LD A,(HL):OR C:LD (DE),A:DEC E:DEC L
POP DE:POP HL:CALL UPDE
DEC B:JP NZ,SCRL:POP BC
POP HL:DEC B:JP NZ,SCR2
SSLN              LD A,(IY):LD B,8:PUSH DE
SSLN0             LD (DE),A:DEC E
                  LD (DE),A:DEC E
                  LD (DE),A:DEC E
                  LD (DE),A:DEC E
                  DJNZ SSLN0:POP DE
UPDE              DEC IY
                  LD A,D:DEC D:AND #7:RET NZ
                  LD A,E:SUB 32:LD E,A:RET C
                  LD A,D:ADD A,8:LD D,A:RET
JTAB DEFB 1,2,3,4,5,5,6,6,6,7,7,7,7,8,8,8,8,8,8
DEFB 7,7,7,7,6,6,6,5,5,4,3,2,0
TXT DEFB "                 "
DEFB "YOU ARE WELCOME TO CACOFONY "
DEFB "COMPILER V0.02 WRITTEN BY "
DEFB "S.T.A.S. (C) 29.06.1996. "
DEFB "             SPECIAL THANKS TO",58
DEFB " SEV (MUSIC MAKING AND BE"
DEFB "TA-TESTING), PACS (TESTIN"
DEFB "G), SVE (MORAL SUPPORT). "
DEFB "              ATTENTION! IT IS THE "
DEFB "FIRST RELEASE OF THIS SYS"
DEFB "TEM AND IF YOU HAVE GOT "
DEFB "SOME OFFERS OR ADVICE YOU"
DEFB " MIGHT CONTACT ME BY WRIT"
DEFB "TING TO",58," RUSSIA, 6810"
DEFB "32 KOMSOMOLSK-NA-AMURE, PI"
DEFB "ROGOVA STREET 15/2-6.            ALL"
DEFB " YOUR IDEAS WILL BE CONSID"
DEFB "ERED AND WILL BE ABLE TO "
DEFB "BE USED IN NEXT VERSIONS "
DEFB "OF THE CACOFONY.               I HO"
DEFB "PE YOU ENJOY WORKING "
DEFB "WITH THIS CACOFONY SYSTEM"
DEFB ".              HAVE A NICE COMPOSIN"
DEFB "G AND WAIT FOR NEXT PROGR"
DEFB "AMS FROM ME!               YOURS "
DEFB "FAITHFULLY, S.T.A.S.             ",0

TL EQU #86-11
COMADR EQU 24900
COMA             EQU COMADR+32
CDON              LD HL,DONT:JR NOCOM+3
NOCOM             LD HL,NOCOMT:JP PRSS
COMPILE BIT 0,(IX+12):JR Z,NOCOM
                  BIT 1,(IX+12):JR NZ,CDON
                  LD HL,WAITT:CALL PRSA
                  LD A,(BASE+1):INC A
                  LD (DFP+2),A
                  LD (DFPP+2),A:XOR A
DFP               LD BC,0:LD HL,POSTAB+4
                  LD DE,5
DFP0              CP (HL):JR NZ,DFP1:LD C,1
DFP1              ADD HL,DE:DJNZ DFP0
                  DEC C:JR Z,DFP2
DFPP              LD BC,0
DFP3              OR A:SBC HL,DE
                  CP (HL):JR NC,DFP4
                  DEC (HL):LD C,1
DFP4              DJNZ DFP3
                  DEC C:JP NZ,DFP5
                  PUSH AF
                  PUSH AF:CALL GPATAD
                  POP AF:PUSH HL
                  INC A:CALL GPATAD
                  PUSH HL:LD DE,S
                  EX DE,HL:OR A:SBC HL,DE
                  LD B,H:LD C,L
                  POP HL:POP DE:LDIR:POP AF:DEC A
DFP2              INC A:JR DFP
DFP5              LD HL,UST:LD DE,UST+1
                  LD BC,31:LD (HL),0:LDIR
                 LD HL,MSG:LD DE,COMADR
                 LD BC,16:LDIR
                 LD HL,#D116:LD C,16:LDIR
                 INC HL:INC HL:LD C,16:LDIR
                 LD A,(53562)
                 LD (COMA+#12),A
                  LD HL,BASE
                  LD A,(HL):LD C,A
                 LD (COMA+#10),A
                  INC HL:LD A,(HL):INC A
                 LD (COMA+#11),A:INC HL
                  LD B,A:LD DE,COMA+#1A
                  XOR A:LD (MAXPAT+1),A
GPOST             CP C:JR NZ,NLAD
                  PUSH HL:LD HL,0-COMADR
                  ADD HL,DE
                 LD (COMA+#13),HL:POP HL
NLAD              PUSH AF:PUSH BC:PUSH HL
                  LD A,(HL):INC HL
                  OR (HL):INC HL
                  OR (HL):INC HL:PUSH AF
                  LD A,(HL):INC HL
                  OR A:JR Z,GG1
                  OR #40:LD (DE),A:INC DE
GG1               POP AF:CP 1:LD A,0:CCF:RRA
                  OR (HL):LD (DE),A
                  PUSH AF:LD A,(HL)
MAXPAT            CP 0:JR C,NMP
                  LD (MAXPAT+1),A
NMP               POP AF:INC DE:POP HL
                  RLA:JR NC,NCDS
                  PUSH HL:LD BC,3
                  LDIR:POP HL
NCDS              LD BC,5:ADD HL,BC:POP BC
                  POP AF:INC A:DJNZ GPOST
                  PUSH HL:LD HL,0-COMADR
                  ADD HL,DE
                 LD (COMA+#15),HL:POP HL
                  LD (ADSPC+1),DE
                  LD A,(MAXPAT+1):INC A
                  LD L,A:LD C,L:LD H,B
                  ADD HL,HL:ADD HL,HL
                  ADD HL,HL
                  ADD HL,BC:ADD HL,DE
                  LD (PAAD+1),HL
                 LD (COMA+#19),A
                  LD B,A:XOR A:EX DE,HL
GPATAB            PUSH AF:PUSH BC
                  PUSH DE:CALL GPATAD
                  LD A,(HL):INC A
                  LD (PATL1+1),A
                  LD (PATL2+1),A
                  POP DE:PUSH HL
ADSPC             LD HL,0:DEC A
                  LD (HL),A:INC HL
                  LD (ADSPC+1),HL
                  CALL LDDE
                  POP HL:INC HL:PUSH HL
PATL1             LD B,0
GEP               LD A,(HL):OR A
                  JR Z,ZE:INC HL
                  LD (DE),A:INC DE
                  DJNZ GEP:JR G3C
ZE                LD C,0
ZE1               CP (HL):JR NZ,NZE
                  INC C:INC HL:DJNZ ZE1
NZE               LD (DE),A:LD A,C:DEC A
                  JR Z,OZE
NZE1              LD A,C:INC DE
                  LD (DE),A:INC DE
                  INC B:DJNZ GEP:JR G3C
OZE               LD A,(PATL1+1):DEC A
                  CP B:JR Z,NZE1
                  DEC DE:LD A,(DE)
                  JR NZE1+1
G3C               CALL PACK:POP HL
                  LD BC,64:ADD HL,BC:LD B,3
CCNS              PUSH BC:PUSH HL:PUSH HL
                  LD HL,(ADSPC+1)
                  CALL LDDE:POP HL
PATL2             LD B,0
GCP               LD A,(HL):INC HL
                  LD C,(HL):INC HL
                  CP 104:JR NC,FREP
                  CP 96:JR NC,FPSE
                  PUSH DE:CALL DCN:POP DE
                  LD (DE),A:DEC C:INC C
                  JR Z,ZS0
GCP0              OR #80
                  LD (DE),A:INC DE
                  PUSH HL:PUSH BC:LD B,0
                  LD A,C:PUSH AF:AND #F0
                  RRA:RRA:RRA:RRA:LD C,A
                  LD HL,UST:ADD HL,BC
                  LD (HL),C:POP AF
                  AND #F:LD C,A
                  LD HL,UOT:ADD HL,BC
                  LD (HL),C:POP BC:POP HL
                  LD A,C:LD (DE),A
ZS0               INC DE:DJNZ GCP:JR CG3C
FPSE              XOR A:LD C,A:JR GCP0
FREP              LD C,0:LD A,103
                  DEC HL:DEC HL
FR1               CP (HL):JR NC,NFR
                  INC C:INC HL
                  INC HL:DJNZ FR1
NFR               LD A,C:ADD A,95
                  JP P,NOA:ADD A,96
NOA               LD (DE),A:INC DE
                  INC B:DJNZ GCP
CG3C              CALL PACK:POP HL
                  LD BC,128:ADD HL,BC
                  POP BC:DJNZ CCNS
                  POP BC:POP AF:INC A
                  DEC B:JP NZ,GPATAB
                  EX DE,HL
                  PUSH DE:LD DE,0-COMADR
                  PUSH HL:ADD HL,DE
                 LD (COMA+#17),HL:POP HL
                  POP DE:LD (ASTX+1),HL
                  LD BC,32:ADD HL,BC:EX DE,HL
                  LD HL,32:ADD HL,DE:EX DE,HL
                  LD B,16:XOR A
GOS               PUSH BC:PUSH AF
                  CALL LDDE:PUSH HL
                  LD HL,ORNT:CALL GA
                  SRL C:PUSH HL:LD A,C
                  LD HL,UOT:ADD HL,BC
                  CP (HL):POP HL:JR NZ,GOS1
                  LD B,(HL):INC B:LD C,H
                  LDI:LD A,(HL):DEC A
                  LD (DE),A:INC HL
                  INC DE:LDI
GOS0              LD A,(HL):SLA A:SRA A
                  LD (DE),A:INC DE
                  INC HL:DJNZ GOS0
GOS1              POP HL:POP AF:POP BC
                  INC A:DJNZ GOS
ASTX              LD HL,0
                  LD B,16:XOR A
GSS               PUSH BC:PUSH AF
                  CALL LDDE:PUSH HL
                  LD HL,SAMT:CALL GA
                  SRL C:PUSH HL:LD A,C
                  LD HL,UST:ADD HL,BC
                  CP (HL):POP HL:JR Z,GSS00
                  POP HL:POP AF:POP BC
                  DEC HL:LD (HL),#FF
                  DEC HL:LD (HL),0
                  INC HL:INC HL
                  INC A:DJNZ GSS:JR GSS5
GSS00             LD B,(HL):INC B:LD C,H
                  LDI:LD (AORA+1),DE
                  LD A,(HL):INC HL
                  LD (SLP+1),A
                  INC DE:INC DE:LD C,A:DEC C
                  LD A,(HL):SUB C:LD (DE),A
                  INC DE:INC HL:XOR A
GSS0              PUSH AF:LD C,H
SLP               CP 0:JR NZ,GSS1:PUSH HL
AORA              LD HL,0:CALL LDDE:POP HL
GSS1              LDI:LDI:LD A,(HL):AND #7F
                  INC HL:INC (HL):DEC (HL)
                  JR Z,GSS2:SET 7,A
GSS2              LD (DE),A:INC DE
                  JR Z,GSS3:LD A,(HL)
                  LD (DE),A:INC DE
GSS3              INC HL:POP AF
                  INC A:DJNZ GSS0
GSS4              POP HL:POP AF:POP BC
                  INC A:DJNZ GSS
GSS5              PUSH DE:PUSH DE
                  LD HL,COMADR:EX DE,HL
                  XOR A:SBC HL,DE
                  LD (COMLEN),HL
                  LD C,L:LD B,H
                  LD HL,PLLN+294:POP DE
                  ADD HL,DE:EX DE,HL
                  POP HL:INC BC:LDDR
                  LD DE,BASE:CALL MPL
                  LD DE,DOUBLE:CALL MPL
                  LD HL,25000+PLLN+194
                  LD DE,38704
                  LD BC,(COMLEN):LDIR
                LD HL,38700-DOUBLE
                  LD (DOUBLE+RELN+1),HL
                  CALL DOUBLE:LD IX,VARS
                  SET 1,(IX+12):JP POK
MPL               LD HL,PLAYER
                  LD BC,PLLN:LDIR
                  LD HL,SOUNDS
                  LD C,#C0:LDIR
                  LD (DE),A:INC DE
                  LD (DE),A:INC DE:RET
GA                ADD A,A:LD C,A:LD B,0
                  ADD HL,BC:LD A,(HL)
                  INC HL:LD H,(HL)
                  LD L,A:RET
LDDE              PUSH DE:PUSH HL:LD HL,0-COMADR
                  ADD HL,DE:EX DE,HL:POP HL
                  LD (HL),E:INC HL
                  LD (HL),D:INC HL:POP DE:RET
PACK              PUSH HL:PUSH DE
                  LD (LADR+1),DE
PAAD              LD HL,0:LD BC,0
PAC0              PUSH HL:LD HL,(ADSPC+1)
                  LD E,(HL):INC HL
                  LD D,(HL):LD HL,COMADR
                  ADD HL,DE:EX DE,HL:POP HL
                  LD A,(DE):LD (OAD+1),DE
                  CPIR:PUSH HL
PAC1              INC DE:LD A,(DE)
                  CPI:EX AF,AF':PUSH HL
LADR              LD HL,0:OR A:SBC HL,DE
                  POP HL:JR Z,PAC2
                  EX AF,AF':JR Z,PAC1
PAC2              POP HL:JR NZ,PAC0
                  EX DE,HL:DEC DE
OAD               LD HL,0:OR A:SBC HL,DE
                  JR Z,PAC3:POP AF
                  ADD HL,DE:PUSH HL
PAC3              LD HL,(ADSPC+1)
                  CALL LDDE
                  LD (ADSPC+1),HL
                  POP DE:POP HL:RET
FS                DEFS 7
UST               DEFS 16
UOT               DEFS 16
MSG DEFB "CACOFONY SC V0.0"
SAMT            DEFW FS,S,483+S,2*483+S
DEFW 3*483+S,4*483+S
DEFW 5*483+S,6*483+S
DEFW 7*483+S,8*483+S
DEFW 9*483+S,10*483+S
DEFW 11*483+S,12*483+S
DEFW 13*483+S,14*483+S
DEFW 15*483+S
ORNT            DEFW FS,O,123+O,2*123+O
DEFW 3*123+O,4*123+O
DEFW 5*123+O,6*123+O
DEFW 7*123+O,8*123+O
DEFW 9*123+O,10*123+O
DEFW 11*123+O,12*123+O
DEFW 13*123+O,14*123+O
DEFW 15*123+O
DCN               LD E,A:RRA:RRA:RRA
                  AND #F:LD D,A:LD A,E
                  AND 7:ADD A,A:ADD A,A
                  LD E,A:ADD A,A:ADD A,E:ADD A,D:RET
GPOSAD            LD C,A:LD B,0
                  LD L,A:LD H,B
                  ADD HL,HL:ADD HL,HL
                  ADD HL,BC
                  LD BC,POSTAB
                  ADD HL,BC:RET
GPATAD            LD C,A:LD B,0:ADD A,A
                  LD D,A:LD E,B:LD L,A
                  LD H,E:ADD HL,HL
                  ADD HL,HL:ADD HL,HL
                  ADD HL,HL:ADD HL,HL
                  EX DE,HL:SBC HL,DE
                  ADD HL,BC
                  LD BC,PATTERNS
                  ADD HL,BC:RET

PLAYER            HALT:CALL #007C
M2                DEC SP:DEC SP:POP BC
                  PUSH BC:LD L,C:LD H,B
                  LD IX,M1-M2
                  ADD IX,BC
                  LD A,M3-M1
REL0              LD E,(IX):INC IX
                  LD D,0:ADD HL,DE
                  LD E,(HL):INC HL
                  LD D,(HL):EX DE,HL
                  ADD HL,BC:EX DE,HL
                  LD (HL),D:DEC HL
                  LD (HL),E:DEC A
                  JR NZ,REL0
                  PUSH IX:POP HL
                  POP DE:DEC DE:DEC DE
                  DEC DE:DEC DE:PUSH DE
                  LD BC,DISPA-INSTALL:JP #33C3
M1 DEFB TL,3,3,8,3,#12,8,3,3,#A
DEFB 3,3,7,#1C,6,6,3,#B,#14,4,7,7,7
DEFB #16,6,4,4,5,3,3,4,#A,#20,#F
DEFB #7B,#13,#20,#1E,#11,#D,#44,14
DEFB 5,#15,4,22,5,5,5,#1F,6,3,6,4,4,3,3
DEFB 3,16,6,8,5,11,7,18,#C,6,8,3,9,18,2,2
M3 DISPLAY #FFFC
INSTALL LD DE,PCA+RELN:JP INST
MUTE              JP MUT0
EFPL              RET:DEFW 0
QPLAY             JR PLAYM
IM2PL             CALL PLAYM
                  LD A,(CALLS):OR A:RET Z
                  LD BC,1120
IPL               DEC BC:LD A,B:OR C
                  JR NZ,IPL
PLAYM
PLA               LD A,#FF:INC A
CALLS EQU $+1:AND 1
                  LD (PLA+1),A:JR NZ,PLDX
PLD               LD A,1:DEC A
                  LD (PLD+1),A
PLDX              LD (FL+1),A:JP NZ,PLD0
DELAY EQU $+1
                  LD A,0:OR A
                  JR NZ,PLD1
PLD2              LD A,8
PLD1              LD (PLD2+1),A
                  LD (PLD+1),A
PLAYNN            LD HL,PATLEN:LD A,(HL)
                  DEC HL:INC (HL):CP (HL)
                  JP NC,NONLN:LD (HL),0
MLEN EQU $+1:LD A,0:DEC HL
POSAD EQU $+1:LD DE,0
                  INC (HL):CP (HL)
                  JR NZ,NOLP
LOOP EQU $+1:LD (HL),0
LPADR EQU $+1:LD DE,0
NOLP              EX DE,HL
                  LD A,(HL):BIT 6,A
                  JR Z,NDL:AND #3F
                  LD (DELAY),A
                  INC HL:LD A,(HL)
NDL               INC HL:LD (POSAD),HL
                  ADD A,A:JR C,NGDS
                  LD HL,Z3
NGDS              LD DE,DISPA
                  LDI:LDI:LDI
                  JR NC,NRWA
                  LD (POSAD),HL
NRWA              LD L,A:LD H,0
                  ADD HL,HL:ADD HL,HL
                  RRC A:ADD A,L
                  LD L,A:JR NC,APATAB:INC H
APATAB            LD DE,0:ADD HL,DE
                  LD A,(HL):LD (PATLEN),A
                  INC HL
                  LD DE,EADR:LDI:LDI
                  LD DE,AADR:LDI:LDI
                  LD DE,BADR:LDI:LDI
                  LD DE,CADR:LDI:LDI
NONLN
EREP              LD A,1:DEC A:JR NZ,NGNE
EADR EQU $+1:LD HL,0:LD A,(HL)
                  INC HL:LD B,(HL)
                  OR A:JR Z,NSEN:DEC HL
                  LD (ENVV),A:LD B,1
NSEN              INC HL:LD (EADR),HL
                  LD A,B
NGNE              LD (EREP+1),A
PLD0
PLN               LD IX,PLDAT:LD B,3
                  LD HL,TONE+4:LD DE,VOLM+2
PLNR              LD (PLN1+1),HL
                  LD (PLN2+1),DE
                  PUSH HL:PUSH DE:PUSH BC
                  LD HL,DISPA-VOLM:ADD HL,DE
                  LD (PLN01+1),HL
                  LD (IX+12),#FE:LD B,0
FL                LD A,0:OR A:JR NZ,NONN
                  DEC (IX-4):JR NZ,NONN
                  LD H,(IX-5):LD L,(IX-6)
                  LD A,(HL):INC HL:LD C,A
                  AND #7F:CP 96
                  JP NC,NGNN
                  LD (IX-3),A:LD (IX-4),1
                  LD (IX-1),#FF
                  INC C:JP P,NOSO
                  LD B,(HL):INC HL
                  OR B:JR NZ,NOSO
                  LD (IX-3),#FF
NOSO              LD (IX-6),L:LD (IX-5),H
NONN              LD C,(IX-2)
                  LD A,B:AND #F:JR NZ,NOOR
                  LD A,C:AND #F
NOOR              LD E,A
                  LD A,B:AND #F0:JR NZ,NOSM
                  LD A,C:AND #F0
NOSM              LD B,A:OR E
                  LD (IX-2),A
                  INC (IX-1):JR NZ,NGOR
                  LD A,B:RRA:RRA:RRA:PUSH DE
                  LD C,A:LD B,0
AST EQU $+1:LD HL,0:ADD HL,BC
                  LD A,(HL):INC HL
                  LD H,(HL):LD L,A
                  DEFB #DD:LD E,L
                  DEFB #DD:LD D,H
                  LDI:LDI:LDI:LDI
                  EX DE,HL:LD (HL),E
                  INC HL:LD (HL),D:INC HL
                  EX (SP),HL:ADD HL,HL
                  LD H,0
AOT EQU $+1:LD DE,0
                  ADD HL,DE:POP DE
                  LD A,(HL):INC HL
                  LD H,(HL):LD L,A
                  LDI:LDI:LDI
                  EX DE,HL:LD (HL),E
                  INC HL:LD (HL),D:INC HL
                  LD (HL),0
NGOR              LD L,(IX+9):LD H,(IX+10)
                  LD E,(IX+11):LD D,0
                  ADD HL,DE
                  LD A,(IX-3):OR A
                  JP M,PSE:ADD A,(HL)
PLN01             LD HL,0:ADD A,(HL):AND #7F
PLN03             CP 96:JR C,PLN02:LD A,97
PLN02             ADD A,A:LD L,A:LD H,D
                  LD BC,CSOUNDS:ADD HL,BC
                  LD C,(HL):INC HL
                  LD B,(HL):LD L,(IX+4)
                  LD H,(IX+5):LD E,(HL)
                  INC HL:LD D,(HL):PUSH HL
GMSK              LD HL,#F801:ADD HL,DE
                  LD A,H:AND #F:ADD A,A
                  OR L:CP 2:JR C,PLN0
                  EX DE,HL:ADD HL,BC:JP PLN1
PLN0              LD HL,0:OR A:JR NZ,PLN1
                  SET 0,(IX+12)
PLN1              LD (0),HL:POP HL
                  LD A,(HL):INC HL
                  RRA:RRA:RRA:RRA:AND #F
                  LD C,(HL):BIT 6,C
                  JR Z,PLN2:LD (ENVM),A
                  LD A,C:ADD A,A
                  JR NC,PLN111:INC HL
PLN111
ENVV EQU $+1:LD A,0:DEC A
                  INC A:JR Z,PLN6
                  JR NC,PLN60
                  ADD A,(HL)
PLN60             LD (ENV),A
PLN6              LD A,#10
PLN2              LD (0),A
                  BIT 5,C:JR Z,PLN3
                  LD A,C:LD (NOISE),A
                  RES 3,(IX+12)
PLN3              INC HL:EX DE,HL
                  PUSH IX:POP HL
                  LD A,(HL):DEC HL:SCF
                  SBC A,(HL)
                  JR NC,PLN4:LD (HL),0
                  INC HL:LD A,(IX+3)
                  LD (HL),A:INC HL
                  LD E,(HL):INC HL
                  LD D,(HL)
PLN4              LD (IX+4),E:LD (IX+5),D
                  LD A,(IX+11):CP (IX+6)
                  JR C,PLN5:LD A,(IX+8)
                  LD (IX+6),A
                  LD A,(IX+7)
PLN5              INC A:LD (IX+11),A
PLNE              LD BC,19:ADD IX,BC
                  POP BC:POP DE:POP HL
                  DEC DE:DEC HL:DEC HL
                  DEC B:JP NZ,PLNR
OUTS              LD A,(IX-45)
                  RLCA:AND (IX-26)
                  RLCA:AND (IX-7)
                  LD (MIX),A
OUTS1             LD A,#D:LD HL,ENVM
                  LD DE,#FFBF:LD C,#FD
                  INC (HL):DEC (HL)
                  JR NZ,OUTS0:DEC A:DEC HL
OUTS0             LD B,D:OUT (C),A
                  LD B,E:OUTD:DEC A
                  JP P,OUTS0:RET
INST              LD HL,LDSP:LD BC,6
                  LD (HL),E:ADD HL,BC
                  LD (HL),D:LD C,#B7-#2D+16
                  ADD HL,BC:LD (HL),E
                  LD C,7:ADD HL,BC
                  LD (HL),D
                  LD HL,48:ADD HL,DE
                  LD A,(HL):LD (LOOP),A:INC HL
                  LD A,(HL):LD (MLEN),A:INC HL
                  LD A,(HL):LD (CALLS),A:INC HL
                  LD IX,ATAT:LD B,3
INS00             LD E,(IX):INC IX
                  LD D,(IX):INC IX
LDSP EQU $+2
                  LD A,(HL):ADD A,0
                  LD (DE),A:INC HL:INC DE
HDSP EQU $+2
                  LD A,(HL):ADC A,0
                  LD (DE),A:INC HL:INC DE
                  DJNZ INS00
                  PUSH HL:INC HL
                  LD (POSAD),HL
                  LD A,(HL):AND #3F
                  LD (PLD2+1),A
                  LD HL,(AST):LD C,32
                  ADD HL,BC:LD (AOT),HL
                  XOR A:LD (ENVV),A
                  INC A:LD (PLD+1),A
                  LD (EREP+1),A
                  LD (PLA+1),A
                  LD HL,NPOS
                  LD (HL),255:INC HL
                  LD (HL),B:INC HL
                  LD (HL),B
                  POP HL:LD A,(HL)
                  LD (HL),B:OR A
                  JR Z,MUTE0:LD B,A
                 LD HL,(APATAB+1)
CAD0              INC HL:LD C,4
CAD1              CALL CADHL:DEC C:JR NZ,CAD1
                  DJNZ CAD0
                LD HL,(AST):LD B,32
CAD2              CALL CADHL:DEC B:BIT 4,B
                  JR Z,CAD3:INC DE:PUSH HL
                  EX DE,HL:CALL CADHL:POP HL
CAD3              INC B:DJNZ CAD2
MUTE0             LD HL,PLDAT-4:LD DE,18
                  LD BC,#FF01:LD A,3
MU0               LD (HL),C:INC HL
                  LD (HL),B:ADD HL,DE
                  DEC A:JR NZ,MU0
MUT0              LD HL,TONE:LD D,H:LD E,L:INC DE
                  LD BC,12:LD (HL),B:LDIR
                  JP OUTS1
NGNN              LD A,C:SUB 95:JP P,NGNN1
                  SUB 96
NGNN1             LD (IX-4),A:JP NOSO
PSE               LD HL,(PLN2+1):LD (HL),0
                  LD (IX+12),#FF:JP PLNE
CADHL             LD A,(HL)
                  ADD A,0:LD (HL),A:LD E,A
                  INC HL:LD A,(HL):LD D,A
                  ADC A,0:LD (HL),A
                  INC HL:INC D:RET Z:LD D,A:RET
ATAT              DEFW LPADR,APATAB+1,AST
Z3                DEFS 3

CSOUNDS DEFS 194
PCA
DISPA DEFB 255,255,255
NPOS DEFB 255
LINE DEFB 0
PATLEN DEFB 0
CADR              DEFS 6
PLDAT             DEFS 13
BADR              DEFS 19
AADR              DEFS 39
TONE              DEFS 6
NOISE             DEFB 0
MIX               DEFB #FF
VOLM              DEFS 3
ENV               DEFW 0
ENVM              DEFB 0
ENDMAC 
DOUBLE

ORG #8000;#5D5D
                  XOR A:LD HL,#5AFF
                  LD DE,#5AFE:LD B,#1B
                  LD (HL),A:LDDR:OUT (254),A
                  LD HL,#C9F1:LD (#5CC2),HL
                  LD HL,#8000:LD B,6
                  CALL LSEC:CALL #8000:DI
                  LD BC,#7FFD
                  LD A,#10:OUT (C),A
                  LD HL,#D280:LD (HL),A
                  INC A:OUT (C),A
                  LD B,27
                  CALL LSEC
                  LD HL,0:PUSH HL
                  LD HL,#3D2F:PUSH HL
                  PUSH HL:JP #D280
LSEC              LD DE,(#5CF4)
                  LD C,5:JP #3D13

ORG #D280



