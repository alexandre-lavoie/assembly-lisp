(JMP 11)
-----------
(LDI *x* 0)
(STR *a* 1)
(JRZ 4)
(SBI *b* 1)
(ADR *x* 1)
(JMR -3)
(JR *j*)
-----------
(LDI *a* 0)
(LDI *j* 0)
(STR *a* *a*)
(ADI *a* 1)
(CPR *a* 256)
(JRE 3)
(JMR -4)
-----------
(LDI *a* 0)
(LDI *n* 0)
(LDI *b* 2)
(RTR *a* *n*)
(JMP 3 *j*)
(STR *x* *n*)
(CPR *n* 255)
(JRE 4)
(ADI *n* 1)
(JMR -7)
-----------
(STR 2 1)
(RUT)
(HLT)
----------------
Created by Alexandre Lavoie 2018