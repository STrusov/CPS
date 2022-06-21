 INCLUDE M7
 INCLUDE E17
DQ                LD A,(53562):LD (CALLS),A:JP NQ
PRMM              LD HL,#516:LD (VARS),HL:LD A,"M:JP PRCH
DMENU             LD A,(CALLS):LD (53562),A
                  LD (CAT1+1),SP
                  CALL SBS:LD HL,DMN:LD DE,PRMM:JP W2
DMN DEFB 6,2,20,13,#4E
DEFB #83,"DISK OPTIONS",#D,#D
DEFB 255,8,5,#4F,7,4,24,10,5*8+#47
DEFB #82,"OBJECT",58,#8A,"~",2,3,"b:DEFW OBJ,0
DEFB #D,#82,"NAME",58,#88,".M~~",2,3,"n:DEFW ONAME,INAME
DEFB #D,#82,"LOAD \\\",4,3,"l:DEFW 0,LOAD
DEFB " RENAME ",#D,4,2,"r:DEFW 0,REN
DEFB #82,"SAVE \\\",4,3,"s:DEFW 0,SAVE
DEFB " ERASE ",#D,4,2,"e:DEFW 0,ERASE
DEFB #82,"CATALOG ",4,3,"c:DEFW 0,CAT
DEFB " DRIVE",58,#82,"~",2,2,"d:DEFW DRV,0
DEFB #D,#82,"IMPORT ST 1.0 ",#D,4,3,"i:DEFW 0,IMPST
DEFB #82,"EXIT MENU ",0,0,7:DEFW 0,DQ:DEFB 255
OBJ DEFB 0,7,9,"COMPOSE            SAMPLES            ORNAMENTSSOUNDS             POSITIONSORNAMENT SAMPLE                      "
ONAME DEFB 0,1,8,"                  M"
DRV DEFB 1,4,1,"ABCD"
FAT DEFW BASE,28590,S,7248
DEFW O,1845,SOUNDS,192,BASE,19242
DEFW 0,123,0,483:DEFB 255
INF               LD A,"M:LD (ONAME+11),A
                  LD A,13
INFI              LD HL,ONAME+3:LD BC,9
                  LD DE,#5CDD
                  LDIR:LD (#5D06),A
                  LD A,(OBJ):ADD A,A:ADD A,A
                  LD C,A:LD B,0:LD HL,FAT
                  ADD HL,BC:LD C,4:LDIR
IDRV              XOR A:LD (#5D10),A
                  DEC A:LD (23610),A
                  LD A,(DRV):LD C,1
                  CALL DOS:LD C,#18
                  CALL DOS:LD C,#A
                  CALL DOS:PUSH BC
                  LD HL,(#5CE6):DEC H:INC H:JR NZ,INFE
                  LD A,(OBJ):CP 5:PUSH AF
                  CALL Z,GORNAD:POP AF
                  CALL NZ,GSAMAD
SOAD              LD HL,0
INFE              POP BC:INC C:LD (AFS+1),HL:RET
REN               CALL INF:JR Z,NOFL:LD A,C
                  DEC A:LD C,8:CALL DOS:DEC (IX+3)
                  CALL INAME:INC (IX+3):BIT 7,(IX+11):RET Z
                  LD HL,ONAME+3:LD DE,#5CDD
                  LD BC,8:LDIR
REN0              LD A,(#5D1E):LD C,9:JR DOSM
NOFL              LD HL,NOFT:JP PRSS
ERASE             CALL INF:JR Z,NOFL
                  LD HL,SH:CALL YN
                  LD C,#12:JR DOSM
SAVE              CALL INF:JR NZ,FEX
                  LD DE,(#5CE8):PUSH HL
                  LD HL,(#5CE6):EX (SP),HL
                  LD C,#B:CALL DOS
                  POP HL:LD DE,(#5CE6):OR A
                  SBC HL,DE:ADD HL,DE:JR Z,POK
                  LD (#5CE6),HL:LD C,#A
                  CALL DOS:JR REN0
LOAD              CALL INF:JR Z,NOFL
                  XOR A:LD (#5CF9),A
                  LD C,#E:DEC A
DOSM              CALL DOS
POK               LD HL,DOK
PRSS              LD D,12:LD E,(HL):INC HL
                  LD (VARS),DE:CALL PRS
KWP               LD B,20:HALT:DJNZ KWP+2
                  CALL KW:PUSH AF
                  LD HL,SPLN:LD (IX),6
                  CALL PRS:RES 7,(IX+10):POP AF:RET
FEX               LD HL,FEXT:CALL YN:LD C,8
                  LD A,(#5D1E):CALL DOS
                  LD DE,(#5CEB):LD BC,(#5CE9)
AFS               LD HL,0:LD C,6:JR DOSM
YN                CALL PRSS:CP "y:RET Z
                  POP HL:RET
PRSA              LD D,(HL):INC HL
                  LD E,(HL):INC HL
                  LD (VARS),DE:JP PRS
RSEC              LD D,0:LD BC,#105:LD HL,#5B00
DOS PUSH IY:LD IY,#5C3A:IM 1
              PUSH HL:LD HL,#5CC2
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
RAIE              POP AF:EX (SP),HL:RET
PRAD DEFW RADR
RAIT DEFB 6,#56,"RETRY/IGNORE",58,"000",58,"00?",255
NOFT DEFB 10,#56,"NO SUCH FILE",255
FEXT DEFB 9,#56,"OVERWRITE Y/N?",255
DOK DEFB 8,#4D,"OK. PRESS A KEY.",255
DERR DEFB 0,13,#56,"BREAK",255
               DEFB 3,12,#56,"NO SPACE",255
               DEFB 6,12,#56,"NO DISK",255
               DEFB 4,9,#56,"DIRECTORY FULL",255
DER1 DEFB 11,#56,"DISK ERROR",255
SPLN DEFB #4F,"                              ",255
SH DEFB 9,#56,"ARE YOU SURE ?",255
INAME             CALL FTC
                  LD A,(HL):INC HL:INC HL:INC HL
                  LD E,(HL):INC HL
                  LD D,(HL);INC D;DEC D;RET Z
                  INC DE:INC DE
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
EXI POP HL:POP HL:POP AF:LD (IX+9),A
                  LD A,#FF:LD (OLDC+1),A:JP PRCUR
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
CAT1              LD SP,0:LD HL,DMN:LD DE,PRMM
CAT19             SCF:JP W2E-6
CATE              CALL POK:JR CAT1
DRER              LD IX,VARS
                  LD SP,(CAT1+1)
                  LD HL,DERR:IM 2
                  LD A,(23610):CP #14
                  LD A,0:JR Z,DRER1
                  LD A,(#5D0F)
DRER1             LD BC,DER1-DERR:CPIR:XOR A
                  CALL PRSS
                  BIT 4,(IX+11)
                  RES 4,(IX+11):JP Z,CONTR
                  JR CAT1
ABOUT             LD HL,ATX:LD DE,0
                  SCF:CALL PRMENU:LD A,#10
                  LD BC,#7FFD:OUT (C),A:CALL KW
                  POP HL:LD HL,CMN:LD DE,0:JR CAT19
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
TIT DEFB 2,6,#4D,"NAME",58,"                   (0000)",#D
DEFB "FILES",58,"000 ERASED",58,"000",255
FNAM DEFB #4F:DEFS 10:DEFB 32:DEFS 9:DEFB 255
SCYN DEFB 6,#4E,"MORE...",255
WT             DEFB 12,13,#4F,"WAIT",255

IMPST             LD A,"S:LD (ONAME+11),A
                  LD HL,#516:LD (VARS),HL:CALL PRCH
                  LD A,9:CALL INFI
                  JP Z,NOFL
                  XOR A:LD (FORN+1),A
                  LD HL,WT:CALL PRSA
                  LD A,(#5D1E):LD C,8:CALL DOS
                  LD HL,26282:PUSH HL
                  LD DE,(#5CEB):LD BC,#C05:CALL DOS
                  POP HL:LD A,1:PUSH IX
IMS0              PUSH AF:PUSH HL:CALL GSAMAD+3
                  POP DE:LD IX,32:ADD IX,DE
                  LD IY,64:ADD IY,DE:LD HL,(SAMAD)
                  PUSH HL:INC HL:INC HL:INC HL
                  LD B,32
IMS1              PUSH BC:LD C,(IY):INC IY
                  LD B,(IY):PUSH DE:LD E,C:LD A,B
                  AND #F:LD D,A:CALL GMSK
                  POP DE:JR NC,IMS11:LD BC,#17FE
IMS11             INC IY:BIT 6,(IX)
                  JR Z,IMS2:LD BC,#17FF
IMS2              BIT 4,B:JR NZ,IMS3:DEC BC
                  LD A,C:CPL:LD C,A:LD A,B
                  CPL:LD B,A
IMS3              LD A,B:AND #F:LD B,A:LD A,(DE)
                  ADD A,A:ADD A,A:ADD A,A
                  ADD A,A:OR B;LD B,A
                  LD (HL),C:INC HL:INC DE
                  LD (HL),A:INC HL:LD A,(IX)
                  INC IX:LD C,A:CPL:AND #80
                  RRA:RRA:LD B,A:LD A,C:AND #1F
                  OR B:LD (HL),A:INC HL
                  LD (HL),0:INC HL:POP BC
                  DJNZ IMS1:LD (HL),B
                  INC HL:LD (HL),B:INC HL
                  LD (HL),B:INC HL:LD (HL),B
                  POP HL:LD (HL),31
                  LD A,(IY):DEC A:JP M,IMS4
                  INC HL:LD (HL),A:INC HL
                  ADD A,(IY+1):LD (HL),A:JR IMS5
IMS4              INC (HL):INC HL:LD (HL),32
                  INC HL:LD (HL),32;1
IMS5              LD HL,98:ADD HL,DE:POP AF
                  INC A:CP 16:JP C,IMS0
                  EX DE,HL:LD HL,25002
IMS6 LD A,(DE):DEC A:PUSH AF:INC DE
                  LD A,(DE):INC DE:LD (HL),A
                  INC HL:LD (HL),A:INC HL
                  LD (HL),A:INC HL:LD (HL),0
                  INC HL:POP AF:LD (HL),A
                  INC HL:DJNZ IMS6:LD A,(DE)
                  LD (25001),A:LD HL,33
                  XOR A:LD (25000),A
                  ADD HL,DE:LD A,1:EX DE,HL
IMS7              PUSH AF:PUSH DE:CALL GORNAD+3
                  XOR A:LD (BOR+1),A
                  POP DE:LD HL,(ORNAD)
                  LD (HL),31:INC HL
                  LD (HL),0:INC HL
                  LD (HL),31:INC HL:LD B,32
IMS8              LD A,(DE):OR A:RLA:RLA
                  RRCA:RRCA:LD (HL),A
BOR               OR 0:LD (BOR+1),A
                  INC HL:INC DE:DJNZ IMS8
                  LD A,(BOR+1):OR A:JR NZ,IMS80
                  POP AF:LD (FORN+1),A:PUSH AF
IMS80             POP AF:INC A:CP 16:JR C,IMS7
                  LD HL,32:ADD HL,DE
                  LD A,(HL):LD (25005),A
                  INC HL:LD A,(HL):DEC A:LD (PATL+1),A
                  INC HL:LD DE,#5C00-63
                  LD BC,63:PUSH DE:LDIR
                  LD A,(#5CEA):SUB 12
                  LD (SEC+1),A:POP DE:XOR A
PCONV             PUSH AF:PUSH DE:CALL GPATAD
PATL              LD (HL),0:INC HL:POP DE
                  LD B,64
PCV00             PUSH BC:PUSH HL:PUSH HL:POP IY
                  LD A,B:DEC A:CPL:AND #3F
                  ADD A,64:LD C,A:LD B,0
                  ADD HL,BC:LD (IY),B:LD B,3
PCV0              PUSH BC:CALL GNB:LD C,A
                  CP #F0:LD A,96:JR NC,PCV2
                  PUSH HL:LD A,C:SRL C
                  SRL C:SRL C:LD B,0
                  LD HL,STNT:ADD HL,BC
                  AND #7:OR (HL):POP HL
PCV2              LD (HL),A:INC HL
                  CALL GNB:LD C,A:AND #F0:LD B,A
                  LD A,C:OR #F0:LD C,A
                  CALL GNB:INC C:JR Z,PCV1
                  LD (PCV3+1),A:DEC C:LD A,C:AND #F
PCV3              LD A,0:JR Z,PCV11:LD (IY),A
                  PUSH HL:PUSH DE:PUSH BC
                  LD A,C:AND #F:LD (EFS+1),A
                  LD A,B:RRA:RRA:RRA:RRA
                  AND #F:JR Z,PCV4:CALL GSAMAD+3
                  LD E,(HL):INC HL
                  LD D,(HL):CALL GMSK
                  JR NC,PCV4:LD DE,(SAMAD)
EFS               LD A,0:ADD A,A:ADD A,A:ADD A,A
                  ADD A,A:OR 7:LD (ESAMP+4),A
                  LD HL,ESAMP:LD BC,10:LDIR
PCV4              POP BC:POP DE:POP HL:XOR A
PCV1              AND #F:JR NZ,PCV11
                  DEC B:INC B:JR Z,PCV11
FORN              OR 0
PCV11             OR B:LD (HL),A
                  LD BC,127:ADD HL,BC
                  POP BC:DEC B:JP NZ,PCV0:POP HL
                  INC HL:POP BC:DEC B:JP NZ,PCV00
                  POP AF:INC A:CP 32:JP C,PCONV:POP IX
                  XOR A:LD (53562),A

                  XOR A
DFP               LD BC,0:LD HL,POSTAB+4
                  LD DE,5
DFP0              CP (HL):JR NZ,DFP1
                  LD C,1
DFP1              ADD HL,DE:DJNZ DFP0
                  DEC C:JR Z,DFP2
DFPP              LD BC,0
DFP3              OR A:SBC HL,DE
                  CP (HL):JR NC,DFP4
                  DEC (HL):LD C,1
DFP4              DJNZ DFP3
                  DEC C:JP NZ,POK
DFP2              INC A:JR DFP

SELPOS
SBS               XOR A:LD (NPOS),A:DEC A
                  LD (TPOS+1),A:LD (TPAT+1),A:RET
GNB               LD A,(DE):INC E:RET NZ:PUSH AF
SEC               LD A,0:DEC A:JP M,GNBZ
                  LD (SEC+1),A:PUSH HL
                  PUSH DE:PUSH BC
                  LD DE,(#5CF4):CALL RSEC+2
                  POP BC:POP DE:POP HL:POP AF:RET
GNBZ              XOR A:LD (DE),A:INC E
                  JR NZ,GNBZ+1:POP AF:RET
STNT DEFB 104,104,72,80,88,104,0
DEFB 8,16,24,32,104,40,48,56,64
ESAMP DEFB 1,1,1,#FF,7,#40,0,#FF,7,#40

COMPILE LD HL,CMN:LD DE,0
                  LD A,(CALLS):LD (53562),A
                  JP W2
CMN DEFB 2,4,28,10,#4E
DEFB #86+4,"COMPILE",#D,#D
DEFB 255,3,6,#4F,2,6,29,10,5*8+#47
DEFB "CACOFONY COMPILER",#D,1,1,"c:DEFW 0,COMCOM
DEFB "MUSIC'S NAME",58,#8F,2,1,"m:DEFW 53524,INAME
DEFB #D,"AUTHOR(S)",58,#8F,2,6,"r:DEFW 53542,INAME
DEFB #D,"ABOUT",#D,4,2,"b:DEFW 0,ABOUT
DEFB "EXIT MENU",4,0,#7:DEFW 0,NQ:DEFB 255
INDI              LD IY,CINM:PUSH IY:LD B,4
RESTI             PUSH BC
                  LD L,(IY):LD C,(IY+1):DEC L
                  LD E,(IY+2):LD D,(IY+3)
REST1             PUSH DE:LD B,8
REST0             CALL UPDE:LD A,(DE)
                  OR C:XOR C:LD (DE),A
                  CALL UPDE:DJNZ REST0
                  POP DE:CALL NHP
                  DEC L:JR NZ,REST1
                  LD A,#A9:LD (XX),A
                  LD B,16:CALL VVR
                  LD BC,4:ADD IY,BC
                  POP BC:DJNZ RESTI:POP IY
                  XOR A:LD (XX),A
                  LD A,(VOLM)
                  LD HL,(TONE):CALL TIND
                  LD A,(VOLM+1)
                  LD HL,(TONE+2):CALL TIND
                  LD A,(VOLM+2)
                  LD HL,(TONE+4):CALL TIND

                  LD A,(MIX):LD HL,VOLM
                  LD D,0:RRA:RRA:RRA
                  LD C,A:LD B,3
INDN              RR C:JR C,INDN0:LD A,(HL)
                  CP D:JR C,INDN0:LD D,A
INDN0             INC HL:DJNZ INDN:LD H,D
                  LD L,3:LD A,(NOISE)
                  CPL:AND #1F:LD C,A:ADD A,A
                  ADD A,C:LD DE,#574A
DRIND             LD C,A:AND 7:LD B,A:INC B
                  LD A,C:RRA:RRA:RRA:AND #1F
                  ADD A,E:LD E,A:LD A,1:INC H
                  LD (IY+2),E:LD (IY+3),D
IND0              RRCA:DJNZ IND0:LD B,L:LD C,A
                  LD (IY),L:LD (IY+1),A
IND1              PUSH BC:PUSH DE:LD B,H:CALL VVR
                  POP DE:POP BC:CALL NHP:DJNZ IND1
                  LD C,4:ADD IY,BC:RET
TIND              LD B,A:LD A,H:OR L:JR NZ,TIND0
                  LD A,B:CP 16:LD B,0:JR C,TIND0
                  LD HL,(ENV):ADD HL,HL
                  ADD HL,HL:ADD HL,HL:ADD HL,HL
                  LD B,15:LD A,(ENVM):OR A
                  JR Z,TIND2:LD (TIND2+1),A
TIND2             LD A,0:AND 2
                  JR Z,TIND0:ADD HL,HL
TIND0             LD A,B:CP 16:JR C,TIND1:DEC B
TIND1             PUSH BC
                  LD A,H:AND #F:LD H,A:ADD HL,HL
                  LD A,247:EX AF,AF'
NORMF             SRL H:RR L:EX AF,AF':SUB 31
                  EX AF,AF':LD A,H:OR A
                  JR NZ,NORMF:LD A,L
                  CP #37:JR NC,NORMF
                  CP #1C:LD L,11:JR C,NOR0
                  LD BC,TFN-#1C:LD L,A
                  ADD HL,BC:LD L,(HL)
NOR0              LD H,0:LD BC,ADRN
                  ADD HL,HL:ADD HL,BC
                  EX AF,AF':ADD A,(HL)
                  INC HL:LD L,(HL)
                  POP DE:LD H,D
                  LD DE,#57A0:JP DRIND

CINM DEFW 2,#3FFF,2,#3FFF,2,#3FFF,2,#3FFF
TFN DEFB 11,10,10,9,9,8,7,7,6,6
DEFB 5+1,5,5,4,4,4,3,3,2,2,2,1,1,1,0,0,0
ADRN DEFB 0,3,3,2,5,3,8,2,10,3
DEFB 13,3,16,2,18,3,21,2,23,3,26,2,28,3
COPY              LD HL,CLRM
                  LD A,(LINE):LD (LN1),A
;                 LD BC,CPY-TCX-2*256+#18
                  LD A,H:JR CCPM
TRANSP            LD HL,TRM:XOR A
                  LD BC,TRAN-TCX-2*256+#18
                  JR CCPM
CLEAR             LD HL,CLM
                  LD BC,#6836:LD A,B
CCPM              LD (NCM+1),A:LD DE,0
                  LD A,(NPAT):LD (NPAT1),A
                  LD (TCX),BC
                  LD A,(MCC+1):INC A:LD C,#FF
CCC               INC C:SUB 3:JR NC,CCC
                  LD A,C:LD (CNL),A:JP W2
CLM DEFB 6,3,20,11,#4B
DEFB #87,"CLEAR",#D,#D
DEFB 255,17,9,#4F,6,5,25,10,6*8+#42
DEFB "COMPOSE",1,1,"c:DEFW 0,CCOMP
DEFB "\\\PATTERNS",2,0,"T:DEFW 0,CPATS
DEFB #D,"SAMPLES",2,1,"s:DEFW 0,CSAMS
DEFB "\\\SAMPLE",58,"_",1,3,"m:DEFW NSAM,CSAM
DEFB #D,"ORNAMENTS",4,2,"r:DEFW 0,CORNS
DEFB "\ORNAMENT",58,"_",1,3,"n:DEFW NORN,CORN
DEFB #D,"PATTERN",58,#82,4,3,"t:DEFW NPAT1,CPAT
DEFB "\CHANNEL",58,#81,2,2,"h:DEFW CNL,CLCNL
DEFB #D,"LINES",58,#82,4,1,"l:DEFW LN1,CLLN
DEFB "\--",#83,4,0,"L:DEFW LN2,CLLN
DEFB #D,"\\\\\EXIT MENU",4,0,#7:DEFW 0,NQ:DEFB 255
NPAT1 DEFB 0,40
LN1 DEFB 0,192
LN2 DEFB 0,192
CNL DEFB 1,4,1,"EABC"
OK                LD HL,DOK:LD D,11:JP PRSS+2
TRPAT             LD HL,NCM+1:PUSH HL
                  LD A,(HL):PUSH AF
                  LD (HL),H:LD HL,TPATX
                  CALL CLR:POP AF:POP HL
                  LD (HL),A:RET
CPOR              LD A,(NORN):OR A:RET Z
                  LD A,(NO2):OR A:RET Z
                  LD HL,CPORN:JR CLR
COPAT             LD HL,COPAT1:JR CLR
CLCNL             XOR A:LD (LN10+1),A
                  LD (LN30),A
                  LD A,63:LD (LN20+2),A
CHNLX             LD HL,CLCNL0:JR CLR
CCOMP             LD HL,CLEARM
CLR               LD (JPA+2),HL:LD HL,SH:EX AF,AF'
NCM               LD B,0:LD A,(CNL):OR B:RET Z
                  CALL OK+3:CALL YN+3:LD DE,OK:PUSH DE
                  XOR A:LD (YFL+1),A
JPA               EX AF,AF':JP 0
CPATS             LD HL,CLPATS:JR CLR
CPAT              LD A,(NPAT1):LD HL,CLRPAT:JR CLR
CORNS             LD HL,CLRORNS:JR CLR
CSAMS             LD HL,CLRSAMS:JR CLR
CSAM              LD HL,CLRSAM:LD A,(NSAM)
                  OR A:RET Z:JR CLR
CPSM              LD A,(NSAM):OR A:RET Z
                  LD A,(NS2):OR A:RET Z
                  LD HL,CPSAM:JR CLR
CORN              LD HL,CLRORN:LD A,(NORN):JR CSAM+6
CLLN              LD A,(LN1):LD (LN10+1),A
                  LD L,A:LD A,(LN3)
                  SUB L:LD (LN30),A
                  LD A,(LN2):JR CLCNL+9
CL000             CALL GPATAD:EX AF,AF'
                  LD BC,1:OR A:RET Z
                  LD C,64:SBC HL,BC:LD C,128
CNTS              ADD HL,BC:DEC A:JR NZ,CNTS
                  DEC HL:LD C,2:RET
CPY1
BA2               LD DE,#5B00:LD A,(DE)
                  LD (HL),A:INC E:LD A,(DE)
                  INC E:LD (BA2+1),DE
                  LD D,A:LD A,C:DEC A
                  RET Z:INC HL:LD (HL),D:RET
CLCNL0            LD A,(CNL2):EX AF,AF'
                  LD A,(NPAT2):CALL CL000
                  LD (GLNA2+1),HL
                  LD A,(CNL):EX AF,AF'
                  LD A,(NPAT1):CALL CL000
                  LD (GLNA+1),HL
LN10              LD A,0:PUSH AF:CALL GLNA
LN20              POP AF:CP 0:RET Z
                  INC A:AND #3F:JR LN10+2
GLN               INC A:ADD HL,BC:DEC A
                  JP P,GLN+1:SBC HL,BC:RET
GLNA              LD HL,0:PUSH AF:CALL GLN:POP AF
TCX               DEFW 0:DEC HL
                  ADD HL,BC:LD (HL),B:RET
TRAN              LD A,(HL):CP 96:RET NC
                  CALL DCN:LD E,A
                  LD A,(SMT+1):ADD A,A
                  SRA A:ADD A,E
                  CP 96+64:JR C,TRA2:SUB 160
TRA2              CP 96:JR C,TRA0:SUB 96
TRA0              LD E,#FF
TRA1              SUB 12:INC E:JR NC,TRA1
                  ADD A,12:ADD A,A:ADD A,A
                  ADD A,A:OR E:LD (HL),A
                  ADD HL,BC:RET
CPY
LN30 EQU $+1:ADD A,0:AND #3F
GLNA2             LD HL,0:CALL GLN:LD E,(HL)
                  INC HL:LD D,(HL)
BA1               LD HL,#5B00:LD (HL),E
                  INC L:LD (HL),D:INC L
                  LD (BA1+1),HL:RET
TRM DEFB 6,4,20,10,#4B
DEFB #85,"TRANSPOSE",#D,#D
DEFB 255,12,8,#4F,9,6,22,10,6*8+#42
DEFB #83,"PATTERN",58,#82,4,3,"t:DEFW NPAT1,TRPAT
DEFB #D,#83,"CHANNEL",58,"_",2,5,"h:DEFW CNL,CLCNL
DEFB #D,#83,"LINES",58,#82,4,4,"l:DEFW LN1,CLLN
DEFB "\--",#83,4,0,"L:DEFW LN2,CLLN
DEFB #D,#83,"SEMITONES",58,#83,3,4,"s:DEFW SMT+1,SMT
DEFB #D,#83,"\\EXIT MENU",4,0,#7:DEFW 0,NQ:DEFB 255
SMT            LD A,0:NEG:LD (SMT+1),A:JP PRDAT
TPATX             LD HL,CNL:LD A,(HL):PUSH AF
                  LD B,3:LD A,#C3
                  LD (CHNLX),A
TPAT0             LD (HL),B
                  PUSH HL:PUSH BC
                  CALL CLCNL:POP BC:POP HL
                  DJNZ TPAT0
                  LD A,#21:LD (CHNLX),A
                  POP AF:LD (HL),A:RET

CLRM DEFB 6,2,20,12,#4B
DEFB #87,"COPY",#D,#D
DEFB 255,12,6,#4F,6,4,25,10,6*8+#42
DEFB "PATTERN",58,#82,4,3,"t:DEFW NPAT2,COPAT
DEFB "\>>",#82,4,0,"T:DEFW NPAT1,COPAT
DEFB #D,"CHANNEL",58,"_",2,2,"h:DEFW CNL2,CLCNLX
DEFB "\>>",2,0,"H:DEFW CNL,CLCNLX
DEFB #D,"LINES",58,#82,4,1,"l:DEFW LN3,CLLNX
DEFB "\--",#83,4,0,"L:DEFW LN2,CLLNX
DEFB "\>>",#82,4,0,"=:DEFW LN1,CLLNX
DEFB #D,"SAMPLE",58,"_",1,3,"m:DEFW NS2,CPSM
DEFB "\>>_",1,0,"M:DEFW NSAM,CPSM
DEFB #D,"ORNAMENT",58,"_",1,3,"n:DEFW NO2,CPOR
DEFB "\>>_",1,0,"N:DEFW NORN,CPOR
DEFB #D,"CREATE INSTRUMENT _",1,1,"c:DEFW NI,CINS
DEFB #D,"\\\\\EXIT MENU",4,0,#7:DEFW 0,NQ:DEFB 255
NS2 DEFB 0
NO2 DEFB 0
LN3 DEFB 0,192
NPAT2 DEFB 0,40
CNL2 DEFB 1,4,1,"EABC"
CPSAM             CALL GSAMAD+3
                  LD HL,(SAMAD):PUSH HL
                  CALL GSAMAD:LD DE,(SAMAD)
                  POP HL:LD C,(HL):INC C
                  INC C:XOR A:SLA C:SLA C:RLA
                  LD B,A:DEC BC:LDIR:RET
CPORN             CALL GORNAD+3
                  LD HL,(ORNAD):PUSH HL
                  CALL GORNAD:LD DE,(ORNAD)
                  POP HL:LD A,(HL):LD B,0
                  ADD A,4:LD C,A:JR CPORN-3
COPAT1            LD A,(NPAT1):CALL GPATAD
                  PUSH HL:LD A,(NPAT2)
                  CALL GPATAD:POP DE
                  LD BC,449:JR CPORN-3
CLCNLX            LD HL,CLCNL:JR COPYL
CLLNX             LD HL,CLLN
COPYL             LD (JPA0+1),HL
                  LD A,H:LD (YFL+1),A
                  LD A,(CNL):LD C,A
                  LD A,(CNL2):LD B,A:OR C
                  JR Z,CPYL:DEC B:RET M
                  DEC C:RET M
CPYL              LD A,(LN2):PUSH AF
                  LD B,A:LD A,(LN3):LD C,A
                  LD A,(LN1):ADD A,B:SUB C
                  AND #3F:LD (LN2),A
                  XOR A:LD (BA1+1),A
                  LD (BA2+1),A
                  LD HL,CPY-TCX-2*256+#18
                  LD (TCX),HL
JPA0              CALL 0
                  LD HL,CPY1-TCX-2*256+#18
                  LD (TCX),HL
YFL               LD A,0:OR A
                  CALL Z,CLCNL0:POP AF:LD (LN2),A:RET
ATX DEFB 2,4,28,10,#4D
DEFB #82,"THIS SYSTEM IS DESIGNED",#D
DEFB #83,"& WRITTEN BY S.T.A.S.             ",#D,#D
DEFB 255,4,8,#4C,2,6,28,9,#4C
DEFB #84,"CURRENT VERSION",58,"0.10",#D
DEFB #82,"RELEASE DATE",58," 18.02.1996",#D,#D
DEFB #84,"(C) S.T.A.S. 1995,96",#D,255
COMCOM            LD DE,#5B00
                  LD HL,PAGCALL:LD BC,EEE-PAGCALL
                  PUSH DE:LDIR:RET
PAGCALL LD BC,#7FFD:PUSH BC:DI
                  LD A,#11:OUT (C),A
                  LD A,(#D280):CP #ED
                  CALL Z,#D280:POP BC
                  LD A,#10:OUT (C),A
                  EI:RET
EEE
CINS              LD A,(DELAY):PUSH AF:LD B,A
                  LD A,(PLD2+1):PUSH AF:LD C,A
                  LD HL,CALLS:LD A,(HL)
                  LD (HL),0:PUSH AF
SINSNN            OR A:JR Z,SINSN
                  SLA C:SLA B:DEC A:JR SINSNN
SINSN             LD A,B:LD (DELAY),A
                  LD A,C:LD (PLD2+1),A
                  CALL CINSX:POP AF
                  LD (CALLS),A:POP AF
                  LD (PLD2+1),A:POP AF
                  LD (DELAY),A:RET

ORG #FE00
CINSX
NI EQU $+1:LD A,0:OR A:RET Z
                  PUSH AF:CALL GSAMAD+3
                  POP AF:CALL GORNAD+3
                  LD A,(CNL):OR A:RET Z:PUSH AF
                  DI:SET 3,(IX+11)
                  RES 2,(IX+10)
;                 RES 5,(IX+10)
                  LD HL,PLD+1:LD (HL),1
                  LD HL,OUTS:LD (HL),#C9
                  CALL PLD
                  POP AF:LD IY,PLAYDAT-18
                  LD BC,18
SINC2             ADD IY,BC:DEC A:JR NZ,SINC2
                  LD A,(IY+1):CP 96:JP NC,CINSE
                  CALL DCN:LD (LASTN+1),A
                  LD A,(ENVB+1):LD (LASTE+1),A
                  PUSH IY:LD IY,#5C3A:IM 1
                  LD HL,SH:EI:CALL OK+3
                  DI:IM 2:POP IY
                  CP "y:RLA:JR NZ,CINSE
                  LD HL,(SAMAD):LD BC,119
                  LD (HL),C:INC HL
                  LD (HL),B:INC HL
                  LD (HL),C:INC HL
                  LD (SIAD+1),HL
                  LD HL,(ORNAD)
                  LD (HL),C:INC HL
                  LD (HL),B:INC HL
                  LD (HL),C:INC HL
                  LD (OIAD+1),HL
                  LD B,120
CINS0             PUSH BC
                  LD A,(IY+1):CP 96:JR NC,SISPC
                  CALL DCN:LD H,(IY+10)
                  LD L,(IY+9):LD E,(IY+17)
                  INC HL:INC HL:INC HL
                  LD D,0:ADD HL,DE:LD L,(HL)
                  SLA L:SRA L:ADD A,L
LASTN             SUB 0:AND #7F
OIAD              LD DE,0:LD (DE),A
                  INC DE:LD (OIAD+1),DE

                  LD L,(IY+3):LD H,(IY+4)
                  LD E,(IY+16):INC HL:LD D,0
                  INC HL:INC HL:ADD HL,DE
                  ADD HL,DE:ADD HL,DE:ADD HL,DE
SIAD              LD DE,0:LD BC,3:LDIR
                  LD A,(ENVB+1):OR A:JR Z,LASTE+2
LASTE             SUB 0:LD (DE),A:INC DE
CINS1             LD (SIAD+1),DE
                  PUSH IY:CALL PLD:POP IY
                  POP BC:DJNZ CINS0:SCF
CINSE             LD HL,OUTS:LD (HL),#21
                  SET 2,(IX+10)
                  RES 3,(IX+11):EI
                  CALL C,OK:RET
SISPC             LD HL,(OIAD+1):XOR A
                  LD (HL),A:INC HL
                  LD (OIAD+1),HL
                  LD HL,FS:JR SIAD

ORG #5D54
PLAYDAT DEFS 54
TONE              DEFS 6
NOISE             DEFB 0
MIX               DEFB #FF
VOLM              DEFS 3
ENV               DEFW 0
ENVM              DEFB 0
FS                DEFS 7

ORG #8000;            #5D54
                  XOR A:LD HL,#5AFF
                  LD DE,#5AFE:LD B,#1B
                  LD (HL),A:LDDR:OUT (254),A
                  LD HL,#C9F1:LD (#5CC2),HL
                  LD HL,#8000:LD B,7
                  CALL LSEC:CALL #8000:DI
                  LD HL,#D280:LD BC,#7FFD
                  LD A,#11:OUT (C),A
                  LD (HL),A:DEC A
                  OUT (C),A:LD (HL),A
                  INC A:OUT (C),A:SUB (HL)
                  PUSH AF:PUSH BC
                  LD HL,(#5CF4):PUSH HL
                  JR NZ,NLCOM
                  LD DE,#5CDD:LD HL,COMNAME
                  LD BC,9:LDIR:LD C,#A
                  CALL #3D13:LD A,C:INC C
                  JR Z,NLCOM:LD C,#8
                  CALL #3D13:LD HL,#8000
                  LD DE,(#5CEB):LD B,1
                  CALL LSEC1:LD HL,#8005
                  LD A,#D2:SUB (HL)
                  LD H,A:LD L,#80
                  LD BC,(#5CE9):DEC B
                  CALL LSEC
NLCOM             POP DE
                  POP BC:LD A,#10:OUT (C),A
                  LD HL,#8000:LD B,49
                  CALL LSEC1:CALL #8000
                  POP AF:LD (CH128+1),A
ORG #5B00
                  DI:LD A,#3B:LD I,A:IM 2
                  LD HL,#FFFF:LD (HL),#18
                  LD L,#F4:LD (HL),#C3
                  LD HL,INT:LD (#FFF5),HL
                  LD A,(#5CF6):LD (DRV),A
                  LD (IY-48),1
                  LD (IY-49),35
                  LD (IY+48),0
LD (QUIT0+1),SP
                  LD HL,#5D3B:LD DE,#5D3C
                  XOR A;LD (HL),A:LD B,H;LDIR
                  LD IX,VARS;CALL CLEARM
                  CALL MUTE:EI:JP #D280
COMNAME DEFB "C.P.COMPB"
LSEC              LD DE,(#5CF4)
LSEC1             LD C,5:JP #3D13




