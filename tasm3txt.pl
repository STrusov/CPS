#!/usr/bin/perl
#
# Конвертор исходников формата TASM3s (Rst7, ZX Spectrum) в текстовый.
# По мотивам TASM2TXT.PAS
# https://zx-pk.ru/threads/1356-konverter-iz-tasm-i-alasm-v-tekst.html?p=23804#post23804
# Автор скрипта не знает Perl, читайте на свой страх и риск.
#

use warnings;
use strict;
use utf8;
use open ':std', ':encoding(UTF-8)';

# TASM2TXT.PAS транслитерирует строчные латинские в кириллицу.
# Сохраним возможность, но отключим, поскольку не используется.
my $transliterate = undef;

# Формат строки:
# 1 байт - длина содержимого в байтах;
#    содержимое,
#    ключевые слова и последовательности пробелов кодируются одним байтом;
# 1 байт - длина содержимого в байтах, дублирует начальный байт.
#

#           !"#$%& '()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{| ~
my $rus = ' !"#$%&\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]э_юабцдефгхийклмнопярстужвьызш|щч';

# INCLUDE в оригинальном конверторе нет. Сложно сказать, откуда она взялась, но используется.
my @mnemonic = ("A", "ADC ", "ADD ", "AF'", "AF", "AND ", "B", "BC", "BIT ",
    "C", "CALL ", "CCF", "CP ", "CPD", "CPDR", "CPI", "CPIR", "CPL", "D", "DAA",
    "DE", "DEC ", "DEFB ", "DEFMAC ", "DEFS ", "DEFW ", "DI", "DISPLAY ", "DJNZ ",
    "E", "EI", "ENDMAC ", "EQU ", "EX ", "EXX", "H", "HALT", "HL", "I", "IM ",
    "IN ", "INC ", "IND", "INDR", "INI", "INIR", "IX", "IY", "JP ", "JR ",
    "L", "LD ", "LDD", "LDDR", "LDI", "LDIR", "M", "NC", "NEG", "NOP", "NV", "NZ",
    "OR ", "ORG ", "OTDR", "OTIR", "OUT ", "OUTD", "OUTI", "P", "PE", "PO",
    "POP ", "PUSH ", "R", "RES ", "RET", "RETI", "RETN", "RL ", "RLA", "RLC ",
    "RLCA", "RLD", "RR ", "RRA", "RRC ", "RRCA", "RRD", "RST ", "SBC ", "SCF",
    "SET ", "SLA ", "SP", "SRA ", "SRL ", "SUB ", "V", "XOR ", "Z", "INCLUDE ");

foreach my $original (<*.A>) {

    print "\n Обрабатываем `$original'\n";
    open(my $in, '<:raw', $original) or die "  не могу открыть: $!";

    my $txtfile = lc($original) . 'sm';
    $txtfile =~ tr/ //ds;
    die "  $txtfile уже существует" if -e $txtfile;
    open(my $out, '>:utf8', $txtfile) or die "  не могу создать для записи: $!";

    my $inbytes = 0;
    my $lines = 0;
    while (!eof($in)) {
        my $len = ord(getc($in)); ++$inbytes;
        while ($len != 0xff && $len > 0) {
            my $octet = ord(getc($in)); ++$inbytes;
            if ($octet == 1) {
                print $out ' ' x ord(getc($in)); ++$inbytes;
            } elsif ($octet < 0x20) {
                print $out ' ' x $octet;
            } elsif ($octet < 0x80) {
                if ($transliterate) {
                    print $out substr($rus, $octet - 0x20, 1);
                } else {
                    print $out chr($octet);
                }
            } else {
                print $out $mnemonic[$octet - 0x80];
            }
            --$len;
        }
        getc($in); ++$inbytes;
        print $out "\n";
        ++$lines;
    }
    print "  $inbytes байт --> $lines строк.\n";
}
