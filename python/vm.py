#!/usr/bin/env python3
# to only get codes, call with  python3 vm.py steps 1>/dev/null
import struct
import sys
import re

filename = "../challenge.bin"
modulo = 32768
regSize = 8

trace = False
hack = False
output = ""
code = re.compile(r'\b(?=\w*[A-Z]\w*\b)(?![A-Z][a-z]{11})(\w{12})\b')
codesfound = 0
def mirror(s):
    r = s[::-1]
    m = ""
    for c in r:
        if c == "p": m += "q"
        elif c == "q": m += "p"
        else: m += c
    return m

mem = []
with open(filename, "rb") as f:
    while True:
        data = f.read(2)
        if not data: break
        s = struct.unpack('<H',data)[0]
        mem.append(s)

mem = mem + [0] * (modulo + regSize - len(mem))

ip = 0
inputBuf = ""
stack = []

if len(sys.argv) == 2:
    with open(sys.argv[1],"r") as f:
        for line in f:
            i = line.split("#")[0].strip()
            if len(i) > 0:
                inputBuf += i + "\n"


def v (x): return x if x < modulo else mem[x]
def add (b, c): return (b + c) % modulo
def mul (b, c): return (b * c) % modulo

while True:
    if trace: print (ip, mem[ip:ip+4], mem[32768:32776], stack, file=sys.stderr)
    if ip == 6027 and hack:
        # calculated with https://raw.githubusercontent.com/pankdm/synacor-challenge/master/teleport.cpp
        # changes from https://github.com/pankdm/synacor-challenge/blob/master/vm2.py#L194
        mem[32775] = 25734 # calculated value
        mem[32769] = 5     # target result
        ip = 6030 # jump to next instruction
        print("Teleporter hacked",file=sys.stderr)

    opcode,a,b,c = mem[ip:(ip+4)]
    va = v(a); vb = v(b); vc = v(c)
   
    if   opcode ==  0: break
    elif opcode ==  1: mem[a] = vb;                    ip += 3
    elif opcode ==  2: stack.append(va);               ip += 2
    elif opcode ==  3: mem[a] = stack.pop();           ip += 2
    elif opcode ==  4: mem[a] = 1 if vb == vc else 0;  ip += 4
    elif opcode ==  5: mem[a] = 1 if vb > vc  else 0;  ip += 4 
    elif opcode ==  6: ip = va
    elif opcode ==  7: ip = vb if va != 0 else            ip+3
    elif opcode ==  8: ip = vb if va == 0 else            ip+3
    elif opcode ==  9: mem[a] = add (vb, vc);          ip += 4
    elif opcode == 10: mem[a] = mul (vb, vc);          ip += 4
    elif opcode == 11: mem[a] = vb % vc;               ip += 4
    elif opcode == 12: mem[a] = vb & vc;               ip += 4
    elif opcode == 13: mem[a] = vb | vc;               ip += 4
    elif opcode == 14: mem[a] = vb ^ (modulo - 1);     ip += 3
    elif opcode == 15: mem[a] = mem[vb];               ip += 3
    elif opcode == 16: mem[va] = vb;                   ip += 3
    elif opcode == 17: stack.append(ip+2);             ip = va
    elif opcode == 18: ip = stack.pop()
    elif opcode == 19:
        if va == 10:
            res = code.search(output)
            if res:
                print("Code found: ",res.group(), file=sys.stderr)
                codesfound += 1
                if codesfound == 7:
                    print(" \u21b3 Mirrored:", mirror(res.group()), file=sys.stderr)
            output = ""
        else: output = output + chr(va)
        print (chr(va), end='');        ip += 2
    elif opcode == 20:
        if len(inputBuf) == 0:
            inputBuf = input() + "\n"

        (cmd,rest) = inputBuf.split("\n",maxsplit=1)
        if   cmd == "quit": break
        elif cmd == "trace":
            trace = not trace
            inputBuf = rest
        elif cmd.startswith ("_j"):
            ip = int(cmd[2:].strip())
            inputBuf = rest
        elif cmd.startswith ("_d"):
            addr = int(cmd[2:].strip())
            print(addr,mem[addr:addr+4])
            inputBuf = rest
        elif cmd == "hacktp":
            hack = True
            mem[32775] = 25734
            inputBuf = rest
            print("Teleporter hack enabled", file=sys.stderr)
        elif cmd.startswith("set"):
            mem[32775] = int(cmd[3:].strip())
            inputBuf = rest
        else: 
            (x,inputBuf) = (inputBuf[0],inputBuf[1:])
            mem[a] = ord(x);                           ip += 2
    elif opcode == 21:                                 ip += 1
    else: print ("opcode not implemented ", opcode); break
 

