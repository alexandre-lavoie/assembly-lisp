# Assembly Lisp

Assembly Lisp is a simple assembly-like programming using Common Lisp. 

## Requirements

* [CLisp](https://clisp.sourceforge.io/)

_Other Common Lisp compilers will also work._

## How To Use

To use, run the Main.lisp. A file prompt will appear. Insert the file location in quotes.

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

_Other sample codes in example folder._

## Flags and Registers

Flags and registers are simply variables. Registers found in the functions can be assigned any variable. The following are the built in flags and registers.

```
*c* - Counter Register
*h* - Halt Flag
*r* - RAM Register
*s* - Sum Register
*y* - Carry Flag
*z* - Zero Flag
*max-bit* - Carry Flag Trigger
```

## Functions

#### Math

```
(ADD Register Address) - Adds ROM Address Value to Register.
(ADR Register Address) - Adds RAM Address Value to Register.
(ADI Register Value) - Adds Value to Register.

(SUB Register Address) - Substracts ROM Address Value from Register.
(SBR Register Address) - Substracts RAM Address Value from Register.
(SBI Register Value) - Substracts Value from Register.
```

#### Output

```
(OUT Register) - Outputs Value of Register.
(RUT) - Outputs RAM.
```

#### Program

```
(HLT) - Ends Program.
(MX Value) - Sets Max Amount for Carry Flag.
```

#### RAM

```
(RTR Register Address) - Read RAM Address Value to Register.
(RRR RegisterOutput RegisterAddress) - Read RAM RegisterAddress Value to RegisterOutput.
(SRR RegisterValue RegisterAddress) - Stores RegisterValue at RAM RegisterAddress.
(STR Register Address) - Stores Register Value to RAM.
(SVR Value Address) - Stores Value to RAM.
```

#### Register

```
(LDI Register Value) - Loads Value to Register.
(LDR Register Address) - Loads ROM Adddress Value to Register.
(MOV Register1 Register2) - Moves Value from Register1 to Register2.
```

#### Jumps

```
(JC Address) - Jumps to Address if Carry Flag is set.
(JRC Offset) - Jumps Relative to Current Address if Carry Flag is set.
(JMP Address &optional Register) - Jumps to Address and Set Return Address to Register.
(JMR Offset &optional Register) - Jumps Relative to Current Address and Set Return Address to Register.
(JR Register) - Jumps to Value Address of Register.
(JZ Address) - Jumps to Address if Zero Flag is set.
(JRZ Offset) - Jumps Relative to Current Address if Zero Flag is set.
```