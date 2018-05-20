(JMP 5)
----
1001
----
(LDI *a* 0)
(LDI *b* 0)
(CHR *a* 3 *b*)
(CNT *a* *a*)
(SBI *a* 48)
(STR *a* *b*)
(CPR *b* 3)
(JRE 3)
(ADI *b* 1)
(JMR -7)
(RUT)
(HLT)