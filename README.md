# Assembly Lisp

Assembly Lisp is a simple assembly-like programming using Common Lisp. 

## Requirements

* [CLisp](https://clisp.sourceforge.io/)

_Other Common Lisp compilers will also work._

## How To Use

To use, run the Main.lisp. A file prompt will appear. Insert the file location in quotes.

## Address and Positioning

* RAM works as an array. First box is address 0. 
* ROM addresses are made by line basis. Line 1 equals to the first line of the ROM. 
* ROM first character on a line is 0.

## Flags and Registers

Flags and registers are simply variables. Registers found in the functions can be assigned any variable. The following are the built in flags and registers.

```
*c* - Counter Register
*e* - Equivalence Flag
*h* - Halt Flag
*r* - RAM Register
*s* - Sum Register
*y* - Carry Flag
*z* - Zero Flag
*max-bit* - Carry Flag Trigger
```

## Functions

#### Characters

```
(CHR Register Line Character) - Reads ROM Nth Character of Line and Sets Register.
(CNT Register Character) - Get Integer Value of Character and Sets Register.
```

#### Input

```
(RD Register) - Reads Line and Sets Register.
```

#### Jumps

```
(JC Address) - Jumps to Address if Carry Flag is set.
(JRC Offset) - Jumps Relative to Current Address if Carry Flag is set.
(JE Address) - Jumps to Address if Equivalence Flag is set.
(JRE Address) - Jumps Relative to Current Address if Equivalence Flag is set.
(JMP Address &optional Register) - Jumps to Address and Set Return Address to Register.
(JMR Offset &optional Register) - Jumps Relative to Current Address and Set Return Address to Register.
(JR Register) - Jumps to Value Address of Register.
(JZ Address) - Jumps to Address if Zero Flag is set.
(JRZ Offset) - Jumps Relative to Current Address if Zero Flag is set.
```

#### Math

```
(ADD Register Address) - Adds ROM Address Value to Register.
(ADR Register Address) - Adds RAM Address Value to Register.
(ADI Register Register/Value) - Adds Register/Value to Register.

(SUB Register Address) - Substracts ROM Address Value from Register.
(SBR Register Address) - Substracts RAM Address Value from Register.
(SBI Register Register/Value) - Substracts Register/Value from Register.
```

#### Output

```
(FUT Register/Value Body) - Format Out Using Register/Value Template.
(OUT Register) - Outputs Value of Register.
(RUT) - Outputs RAM.
```

#### Program

```
(HLT) - Ends Program.
(MX Value) - Sets Max Amount for Carry Flag.
(NOP) - No Operation
```

#### RAM

```
(RTR RegisterOutput Register/Address) - Read RAM Register/Address Value to RegisterOutput.
(STR Register/Value Register/Address) - Stores Register/Value at RAM Register/Address.
```

#### ROM

```
(LDR Register Address) - Loads ROM Address Integer to Register.
(LDS Register Address) - Loads ROM Address Value to Register.
```

#### Register

```
(CPR Register Register/Value) - Compares Registers and/or Value and Set Equivalence Flag.
(LDI Register Value) - Loads Value to Register.
(MOV Register1 Register2) - Moves Value from Register1 to Register2.
```

#### String

```
(SNT Register String) - String to Int and Sets Register.
```

## Sample Code

```
(LDI *a* 0)
(LDI *b* 1)
(OUT *b*)
(STR *a* 0)
(MOV *b* *a*)
(ADR *b* 0)
(JRC 2)
(JMR -5)
(HLT)
```

Finds the Fibonacci Sequence.

## Example Code

* Fibonacci.lisp - Find Fibonacci Sequence.
* Math.lisp - Multiplication, Exponent, and Division of Whole Numbers Operation.
* Name.lisp - Asks for Name and Returns Name.
* RAM-Number.lisp - Numbers RAM from 0-256 and Multiplies by two.
* ROMMap-RAM.lisp - Gets Characters on a Line and Sets it to RAM.