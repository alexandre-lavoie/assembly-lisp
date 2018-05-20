(JMP 42)
--MULTIPLICATION--
(LDI *x* 0)
(STR *a* 1)
(JRZ 4)
(SBI *b* 1)
(ADR *x* 1)
(JMR -3)
(JR *j*)
--EXPONENT--
(STR *j* 0)
(MOV *a* *x*)
(SBI *b* 1)
(STR *b* 2)
(JRZ 5)
(MOV *x* *b*)
(JMP 3 *j*)
(RTR *b* 2)
(JMR -6)
(RTR *j* 0)
(JR *j*)
--DIVISION--
(STR *j* 0)
(STR 0 3)
(STR *a* 4)
(STR *b* 5)
(RTR *x* 3)
(ADI *x* 1)
(STR *x* 3)
(MOV *x* *b*)
(RTR *a* 5)
(JMP 3 *j*)
(STR *x* 6)
(RTR *a* 4)
(SBR *a* 6)
(JRZ 2)
(JMR -10)
(RTR *x* 3)
(RTR *j* 0)
(JR *j*)
--CODE--
(LDI *j* 0)
(LDI *a* 2)
(LDI *b* 3)
(JMP 3 *j*)
(MOV *x* *a*)
(LDI *b* 2)
(JMP 11 *j*)
(MOV *x* *a*)
(LDI *b* 2)
(JMP 23 *j*)
(OUT *x*)
(HLT)
--Arithmatic Address--
3 - Multiplication
11 - Exponent
23 - Division
--Ram Address--
0 - Jump Return Address
1 - Register A for Multiplication
2 - Register B for Exponent
3 - Division Counter
4 - Register A for Division
5 - Register B for Division
6 - Temp Storage of Multiplication Answer For Division
----------------
Created by Alexandre Lavoie 2018