;1:
(SETQ *READ-UPCASE* NIL)

;2:
(DEFUN TABLEAU (FORMULA NO PILHA)
   (SETQ NR-NO 1)
   (PUT 'NO1 'FORMULA              FORMULA)
   (PUT 'NO1 'FILHOS               NIL)
   (PUT 'NO1 'PAI                  NIL)
   (PUT 'NO1 'INCONSISTENCIA (ESPECIAL FORMULA))
   (PUT 'NO1 'CONSTANTES      NR-INICIAIS)
   (PUT 'NO1 'INSTANCIAS       0)
   (SETQ NO 'NO1)
   (SETQ PILHA (LIST 'NO1))
   (PRINT '(NO1))
   (LOOP
      ((NULL PILHA) 'raiz-insatisfativel)
      ((EQL  (PROCESSE NO) 'satisfativel) 'raiz-satisfativel)
      (SETQ PILHA (APPEND (GET NO 'FILHOS) (CDR PILHA)))
      (PRINT PILHA)
      (SETQ NO (CAR PILHA))
   )

) 
;3:
(DEFUN TEOREMA (FORMULA RESULTADO)
   (INICIALIZE FORMULA)
   (PRINT SENTENCA)
   (SETQ RESULTADO (TABLEAU (LIST 'nf SENTENCA)))
   ((EQ RESULTADO 'raiz-insatisfativel) 'sim)
   ((EQ RESULTADO 'raiz-satisfativel) 'nao))

;4:
(DEFUN INICIALIZE (FORMULA PAR)
   (SETQ PAR (FECHE (COMPACTE (TRADUZA FORMULA))))
   (SETQ SENTENCA (CAR PAR))
   (SETQ NR-NOVAS-INICIAIS (CADR PAR))
   (SETQ LISTA-INICIAIS
      (REMOVE-DUPLICATES (RECOLHA-CONSTANTES SENTENCA)))
   (SETQ NR-INICIAIS (LENGTH LISTA-INICIAIS)))




;5:



(DEFUN P-LIVRE (x FORMULA y A)
   ((OR (CASA FORMULA '(QS y A))
         (CASA FORMULA '(EX y A)))
      (SETQ A (THIRD    FORMULA))
      (SETQ y (SECOND FORMULA))
      (AND (NEQ x y) (P-LIVRE x A)))
   ((OR (CASA FORMULA '(A eq B))
         (CASA FORMULA '(A ent B))
         (CASA FORMULA '(A e   B))
         (CASA FORMULA '(A ou  B)))
      (OR (P-LIVRE x (CAR FORMULA)) (P-LIVRE x (CADDR FORMULA))))
   ((OR (CASA FORMULA '(nf A))
         (CASA FORMULA '(co A))
         (CASA FORMULA '(n  A)))
      (P-LIVRE x (CADR FORMULA)))
   ((MEMBER x FORMULA) 'T))

;6:


(DEFUN RECOLHA-CONSTANTES (SENTENCA)
   ((OR (CASA SENTENCA '(QS x A))
         (CASA SENTENCA '(EX x A)))

      (REMOVE (CADR SENTENCA)
         (RECOLHA-CONSTANTES (CADDR SENTENCA))))
   ((OR (CASA SENTENCA '(A eq  B))
         (CASA SENTENCA '(A ent B))
         (CASA SENTENCA '(A e    B))
         (CASA SENTENCA '(A ou  B)))
      (APPEND (RECOLHA-CONSTANTES (CAR SENTENCA))
         (RECOLHA-CONSTANTES (CADDR SENTENCA))))
   ((OR (CASA SENTENCA '(nf  A))
         (CASA SENTENCA '(co A))
         (CASA SENTENCA '(n   A)))
      (RECOLHA-CONSTANTES (CADR SENTENCA)))
   ((ATOM SENTENCA) NIL)
   ('T (CDR SENTENCA)))

;7:

(DEFUN RECOLHA-VAR-QUANT (FORMULA LISTA)
   ((OR (CASA FORMULA '(QS x A))
         (CASA FORMULA '(EX x A)))
      (SETQ LISTA (RECOLHA-VAR-QUANT (CADDR FORMULA)))
      (PUSH (CADR FORMULA) LISTA))
   ((OR (CASA FORMULA '(A eq B))
         (CASA FORMULA '(A ent B))

         (CASA FORMULA '(A e   B))
         (CASA FORMULA '(A ou  B)))
      (APPEND (RECOLHA-VAR-QUANT (CAR   FORMULA))
         (RECOLHA-VAR-QUANT (CADDR FORMULA))))
   ((OR (CASA FORMULA '(nf A))
         (CASA FORMULA '(co A))
         (CASA FORMULA '(n   A)))
      (RECOLHA-VAR-QUANT (CADR FORMULA))))

;Partes 8 e 9:

(DEFUN RECOLHA-VAR-LIVR (FORMULA LISTA1 LISTA2)
   (SETQ LISTA1  (REMOVE-DUPLICATES (RECOLHA-VAR-QUANT FORMULA)))
   (LOOP
      ((NULL LISTA1)  LISTA2)
      (COND
         ((P-LIVRE (CAR LISTA1) FORMULA) (PUSH (POP LISTA1) LISTA2)))
      (POP LISTA1)))


(DEFUN FECHE (FORMULA SENTENCA LISTA NUMERO)
   (SETQ SENTENCA FORMULA)
   (SETQ LISTA (RECOLHA-VAR-LIVR FORMULA))
   (SETQ NUMERO 0)

   (LOOP
      ((NULL LISTA) (LIST SENTENCA NUMERO))
      (SETQ NUMERO (ADD1 NUMERO))
      (SETQ SENTENCA (INSTANCIE SENTENCA
            (POP LISTA)
            (PACK (LIST  '# NUMERO))))))

;10 e 11:
(DEFUN PROCESSE (NO)
   (SETQ SENT (GET NO 'FORMULA))
   ;;  (PRINT (LIST 'SENT-PROCESSE: SENT))
   (OR
      (I NO)
      (C NO)
      (D NO)
      (EQV  NO)
      (CO NO)
      (QU NO)
      (QE NO)
      (NFI NO)
      (NFC NO)
      (NFD NO)
      (NFEQV NO)
      (NFCO  NO)

      (NFQU    NO)
      (NFQE     NO)
      (NI     NO)
      (NC   NO)
      (ND       NO)
      (NEQV     NO)
      (NCO    NO)
      (NN1   NO)
      (NN2     NO)
      (NQ         NO)
      (N      NO)
      (EXCL  NO)
   )
)

(DEFUN RAMIFIQUE (NO LISTADADOS PONTAS)
   (SETQ PONTAS (DEPURE (FOLHAS NO)))
   ; ((NULL PONTAS) (PROGN
         ;                  (PUT NO 'FILHOS NIL)
         ;                  (PUT NO 'INCONSISTENCIA 'T)))
   (MAPCAR '(LAMBDA (ELEM) (DESENVOLVA ELEM LISTADADOS)) PONTAS)
   'T)



;12:

(DEFUN DESENVOLVA (FOLHA DLISTA DPILHA NPILHA DNASCIDOS NNASCIDOS)
   (SETQ DPILHA '(NIL))
   (SETQ NPILHA (LIST FOLHA))
   (LOOP
      ((NULL DPILHA) 'T)
      (SETQ DNASCIDOS (FLS (CAR DPILHA) DLISTA))
      (SETQ NNASCIDOS (CRIE-NOS FOLHA DNASCIDOS))
      (IF     (NULL NNASCIDOS)        (SETQ DNASCIDOS NIL ))
      (SETQ DPILHA    (APPEND DNASCIDOS (CDR DPILHA)))
      (SETQ NPILHA    (APPEND NNASCIDOS (CDR NPILHA)))
      (SETQ FOLHA (CAR NPILHA))))

;13:

(DEFUN CRIE-NOS (FOLHA LISTADADOS NOVOSNOS)
   (PUT FOLHA 'FILHOS NIL)
   ((NULL (GET FOLHA 'INCONSISTENCIA))
      (LOOP
         ((NULL LISTADADOS) (REVERSE NOVOSNOS))
         (PUSH (CRIE-NO FOLHA (POP LISTADADOS)) NOVOSNOS)))
   NIL)




;14a:

(DEFUN CRIE-NO (FOLHA DADOS FILHO)
   ((NULL DADOS) NIL)
   (SETQ NR-NO (ADD1 NR-NO))
   (SETQ FILHO (PACK (LIST 'NO NR-NO)))
   (COND ((ZEROP (CADDR DADOS))
         (PUT FILHO 'CONSTANTES (GET FOLHA 'CONSTANTES))
         (PUT FILHO 'INSTANCIAS (FIFTH DADOS))
         (PUT FILHO 'FORMULA (CADR DADOS)))
      ((MINUSP (CADDR DADOS))
         (PUT FILHO 'CONSTANTES (ADD1 (GET FOLHA 'CONSTANTES)))
         (PUT FILHO 'INSTANCIAS 0)
         (PUT FILHO 'FORMULA
            (INSTANCIE
               (CADR    DADOS)
               (FOURTH    DADOS)
               (NR-CTE (GET FILHO 'CONSTANTES)))))
      ;14B:
      ((PLUSP (CADDR DADOS))
         ( IF (> (CADDR DADOS) (GET FOLHA 'CONSTANTES))
            (PUT FILHO 'CONSTANTES (CADDR DADOS))

            (PUT FILHO 'CONSTANTES (GET FOLHA CONSTANTES)))
         (PUT FILHO 'INSTANCIAS 0)
         (PUT FILHO 'FORMULA
            (INSTANCIE
               (CADR DADOS)
               (FOURTH DADOS)
               (NR-CTE (CADDR DADOS))))))
   (PUT FILHO 'PAI FOLHA)
   (PUT FOLHA 'FILHOS (APPEND (GET FOLHA 'FILHOS) (LIST FILHO)))
   (PUT FILHO 'INCONSISTENCIA (INCONSISTENCIA FILHO))
   FILHO)

;15:

(DEFUN NR-CTE (NR)
   ((> NR NR-INICIAIS)
      (PACK (LIST '# (+ NR (- NR-INICIAIS) NR-NOVAS-INICIAIS))))
   ('T (NTH (SUB1 NR) LISTA-INICIAIS)))

;16a1:
(DEFUN TRADUZA (FORMULA PAR)
   ((AND(MEMBER 'eq FORMULA)
         (SETQ PAR (SEPARE 'eq FORMULA))
         (>=(LENGTH(CAR PAR)) 1)
         (>=(LENGTH(CADR PAR)) 1))
      (LIST (TRADUZA (UNID (CAR PAR)))
         'eq
         (TRADUZA (UNID (CADR PAR)))))
   ((AND (MEMBER 'ent FORMULA)
         (SETQ PAR (SEPARE 'ent FORMULA))
         (>= (LENGTH (CAR PAR)) 1)
         (>= (LENGTH (CADR PAR)) 1))
      (LIST (TRADUZA (UNID (CAR PAR)))
         'ent
         (TRADUZA (UNID (CADR PAR)))))
   ;Parte 16a2:
   ((AND (MEMBER 'ou FORMULA)
         (SETQ PAR (SEPARE 'ou FORMULA))
         (>= (LENGTH (CAR PAR)) 1)
         (>= (LENGTH (CADR PAR)) 1))
      (LIST (TRADUZA (UNID (CAR PAR)))
         'ou
         (TRADUZA (UNID (CADR PAR)))))
   ((AND (MEMBER 'e FORMULA)
         (SETQ PAR (SEPARE 'e FORMULA))
         (>= (LENGTH (CAR PAR)) 1)
         (>= (LENGTH (CADR PAR)) 1))
      (LIST (TRADUZA (UNID (CAR PAR)))

         'e
         (TRADUZA (UNID (CADR PAR)))))
   ;16b:
   ((AND (OR (EQ (CAR FORMULA) 'n )
            (EQ (CAR FORMULA) 'co)
            (EQ (CAR FORMULA) 'nf))
         (NOT (NULL (CDR FORMULA))))
      (LIST (CAR FORMULA)
         (TRADUZA (UNID (CDR FORMULA)))))
   ((AND (OR (EQ (CAR FORMULA) 'QS) (EQ (CAR FORMULA) 'EX))
         (ATOM (CADR FORMULA))
         (NOT (NULL (CDDR FORMULA))))
      (LIST (CAR FORMULA)
         (CADR FORMULA)
         (TRADUZA (UNID (CDDR FORMULA)))))
   ((ATOM FORMULA) FORMULA)
   (UNID FORMULA))


;Parte 17:

(DEFUN SEPARE (ATOMO FORMULA AUX1 AUX2 CONT)
   (SETQ AUX2 (CDR (MEMBER ATOMO FORMULA)))
   (SETQ CONT (- (LENGTH FORMULA) (ADD1 (LENGTH AUX2))))

   (SETQ AUX1 (FIRSTN CONT FORMULA))
   (LIST AUX1 AUX2))

;18:

(DEFUN UNID (X)
   ((ATOM X) X)
   ((= (LENGTH X) 1) (UNID (FIRST X)))
   X)


;19:

(DEFUN COMPACTE (FORMULA ABREVIATURA)
   (SETQ FORMULA (RETIRE-VACUOS FORMULA))
   (LOOP
      (SETQ ABREVIATURA (SIMPLIFIQUE FORMULA))
      ((EQUAL FORMULA ABREVIATURA) FORMULA)
      (SETQ FORMULA ABREVIATURA)))




;20a1:


(DEFUN SIMPLIFIQUE (FORMULA PADRAO PARES A B C D x sn)
   ((AND (SETQ PADRAO '(n (A e (n B))))
         (CASA FORMULA PADRAO)
         (SETQ PARES (PARELHAS PADRAO FORMULA))
         (SETQ A (CORRESPONDENTE 'A PARES))
         (SETQ B (CORRESPONDENTE 'B PARES))
         (CONGRUENTE A B))
      (LIST 'co (SIMPLIFIQUE A)))
   ((AND (SETQ PADRAO '((n A) e (co B)))
         (CASA FORMULA PADRAO)
         (SETQ PARES (PARELHAS PADRAO FORMULA))
         (SETQ A (CORRESPONDENTE 'A PARES))
         (SETQ B (CORRESPONDENTE 'B PARES))
         (CONGRUENTE A B))
      (LIST 'nf (SIMPLIFIQUE A)))
   ;Parte 20a2:
   ((AND (SETQ PADRAO '((A ent B) e (C ent D )))
         (CASA FORMULA PADRAO)
         (SETQ PARES (PARELHAS PADRAO FORMULA))
         (SETQ A (CORRESPONDENTE 'A PARES))
         (SETQ B (CORRESPONDENTE 'B PARES))
         (SETQ C (CORRESPONDENTE 'C PARES))
         (SETQ D (CORRESPONDENTE 'D PARES))

         (CONGRUENTE B C)
         (CONGRUENTE A D))
      (LIST (SIMPLIFIQUE A) 'eq (SIMPLIFIQUE B)))
   ((OR (CASA FORMULA '(nf   A))
         (CASA FORMULA '(co A))
         (CASA FORMULA '(n   A)))
      (SETQ A (SECOND FORMULA))
      (SETQ sn (FIRST FORMULA))
      (LIST sn (SIMPLIFIQUE A)))
   ;20b:
   ((OR (CASA FORMULA '(A ent B))
         (CASA FORMULA '(A e B))
         (CASA FORMULA '(A ou B))
         (CASA FORMULA '(A eq B)))
      (SETQ A (FIRST FORMULA))
      (SETQ B (THIRD FORMULA))
      (SETQ sn (SECOND FORMULA))
      (LIST (SIMPLIFIQUE A) sn (SIMPLIFIQUE B)))
   ((OR ( CASA FORMULA '(QS x A))
         ( CASA FORMULA '(EX x A)))
      (SETQ A (THIRD FORMULA))
      (SETQ x (SECOND FORMULA))
      (SETQ sn (FIRST FORMULA))
      (LIST sn x (SIMPLIFIQUE A)))

   FORMULA)

;21a:

(DEFUN RETIRE-VACUOS (FORMULA A B x  sn)
   ((OR (CASA FORMULA '(nf A))
         (CASA FORMULA '(co A))
         (CASA FORMULA '(n A)))
      (SETQ A (SECOND FORMULA))
      (SETQ sn (FIRST FORMULA))
      (LIST sn (RETIRE-VACUOS A)))
   ((OR (CASA FORMULA '(A ent B))
         (CASA FORMULA '(A e B))
         (CASA FORMULA '(A ou B))
         (CASA FORMULA '(A eq B)))
      (SETQ A (FIRST FORMULA))
      (SETQ B  (THIRD FORMULA))
      (SETQ sn (SECOND FORMULA))
      (LIST (RETIRE-VACUOS A) sn (RETIRE-VACUOS B)))
   ;21b:
   ((OR (CASA FORMULA '(QS x A))
         (CASA FORMULA '(EX x A)))
      (SETQ A (THIRD FORMULA))
      (SETQ x (SECOND FORMULA))

      (SETQ sn (FIRST FORMULA))
      (IF(P-LIVRE x A)
         (LIST sn x (RETIRE-VACUOS A))
         (RETIRE-VACUOS A )))
   FORMULA)

;22:

(DEFUN INCONSISTENCIA (NO FORMULA)
   (SETQ FORMULA (GET NO 'FORMULA))
   ((ESPECIAL FORMULA) 'T)
   (EVAL (CONS 'OR (MAPCAR
            '(LAMBDA (FORMA) (CONTRADIZ FORMULA FORMA))
            		   (ANCESTRAIS NO)))))

;23:
(DEFUN ESPECIAL (FORMULA)
   (OR (AND (CASA FORMULA '(nf(co(A e (n B)))))
         	(CONGRUENTE A B))
      	(CASA FORMULA '(nf (co (co A))))
      (CASA FORMULA '(nf (co (nf A))))))




;24:

(DEFUN CONTRADIZ (FORMA1 FORMA2)
   (OR (AND (EQ (CAR FORMA1) 'nf)
         (CONGRUENTE (CADR FORMA1) FORMA2))
      (AND (EQ (CAR FORMA2) 'nf)
         (CONGRUENTE (CADR FORMA2) FORMA1))))

;25a:
(DEFUN CONGRUENTE (FORMA1 FORMA2 A B x y sn)
   ((EQUAL FORMA1 FORMA2) 'T)
   ((OR
         (AND (CASA FORMA1 '(A eq  B))
            (CASA FORMA2 '(A eq  B)))
         (AND (CASA FORMA1 '(A ent  B))
            (CASA FORMA2 '(A ent  B)))
         (AND (CASA FORMA1 '(A e  B))
            (CASA FORMA2 '(A e  B)))
         (AND (CASA FORMA1 '(A ou  B))
            (CASA FORMA2 '(A ou  B))))
      (AND (CONGRUENTE (FIRST FORMA1)(FIRST FORMA2))
         (CONGRUENTE (THIRD FORMA1)(THIRD FORMA2))))
   ((OR (AND (CASA FORMA1 '(nf A))
            (CASA FORMA2 '(nf A)))

         (AND (CASA FORMA1 '(co A))
            (CASA FORMA2 '(co A)))
         (AND (CASA FORMA1 '(n A))
            (CASA FORMA2 '(n A))))
      (CONGRUENTE (SECOND FORMA1)(SECOND FORMA2)))
   ;25b:
   ((OR (AND (CASA FORMA1 '(QS x A))
            (CASA FORMA2 '(QS y B)))
         (AND (CASA FORMA1 '(EX x A))
            (CASA FORMA2 '(EX y B))))
      (SETQ A (THIRD FORMA1))
      (SETQ B (THIRD FORMA2))
      (SETQ x (SECOND FORMA1))
      (SETQ y (SECOND FORMA2))
      (COND ((EQ x y) (CONGRUENTE A B))
         ((AND (NOT (P-LIVRE y A))
               (SUBSTITUIVEL x y A))
            (CONGRUENTE (INSTANCIE A x y) B)))))


;26:
(DEFUN SUBSTITUIVEL (x y FORMULA A z)
   	((EQ x y) 'T)
   	((NOT (P-LIVRE x FORMULA)) 'T)

   	((OR (CASA FORMULA '(QS z A))
         		 (CASA FORMULA '(EX z A)))
      	(SETQ A (THIRD FORMULA))
      	(SETQ z (SECOND FORMULA))
      	(AND (NEQ y z) (SUBSTITUIVEL x y A)))
   	((OR (CASA FORMULA '(A ent B))
         		 (CASA FORMULA '(A e   B))
         		 (CASA FORMULA '(A ou  B))
         		 (CASA FORMULA '(A eq  B)))
      	(SUBSTITUIVEL X Y (FIRST FORMULA))
      	(SUBSTITUIVEL X Y (THIRD FORMULA)))
   	((OR (CASA FORMULA '(nf A))
         	     (CASA FORMULA '(co A))
         	     (CASA FORMULA '(n  A)))
      	(SUBSTITUIVEL X Y (SECOND FORMULA))))

;27:

(DEFUN FOLHAS (NO PILHA LISTA)
   (SETQ PILHA (LIST NO))
   (LOOP
      ((NULL PILHA) (REVERSE LISTA))
      (IF (NULL (GET (CAR PILHA) 'FILHOS))
         (PROGN (PUSH (CAR PILHA) LISTA)

            (SETQ PILHA (CDR PILHA)))
         (SETQ PILHA (APPEND (GET (CAR PILHA) 'FILHOS)
               (CDR PILHA))))))



;28:

(DEFUN DEPURE (LISTA NOVALISTA)
   (LOOP
      ((NULL LISTA) (REVERSE NOVALISTA))
      (IF (NULL (GET (CAR LISTA) 'INCONSISTENCIA))
         (PUSH (POP LISTA) NOVALISTA)
         (SETQ LISTA (CDR LISTA)))))

;29:
(DEFUN ANCESTRAIS (NO ANCESTRAL LISTA)
   ((NULL (GET NO 'PAI)) NIL)
   (SETQ ANCESTRAL (GET NO 'PAI))
   (SETQ LISTA (LIST (GET ANCESTRAL 'FORMULA)))
   (LOOP
      ((NULL (GET ANCESTRAL  'PAI)) LISTA)
      (SETQ ANCESTRAL (GET ANCESTRAL 'PAI))
      (PUSH (GET ANCESTRAL 'FORMULA) LISTA)))


;30 e 31:
(DEFUN COMBINA (FORMA PADRAO PARES)
   (SETQ PARES (PARELHAS PADRAO FORMA))
   ((AND (CASA FORMA PADRAO)
         (<= (LENGTH (SELEC 'A PARES)) 1)
         (<= (LENGTH (SELEC 'B PARES)) 1)
         (<= (LENGTH (SELEC 'C PARES)) 1)
         (<= (LENGTH (SELEC 'D PARES)) 1)) 'T))



(DEFUN CASA (FORMA PADRAO)
   ((OR (EQ PADRAO 'QS) (EQ PADRAO 'EX)) (EQ FORMA PADRAO))
   ((UPPER-CASE-P PADRAO) 'T)
   ((MEMBER PADRAO '(x y z w)) (ATOM FORMA))
   ((ATOM PADRAO) (EQ FORMA PADRAO))
   ((ATOM FORMA) NIL)
   ((/= (LENGTH FORMA) (LENGTH PADRAO)) NIL)
   ((AND (CASA (CAR FORMA) (CAR PADRAO))
         (CASA (CDR FORMA) (CDR PADRAO)))
      'T))



;32:
(DEFUN PARELHAS (PADRAO FORMA)
   ((NULL PADRAO) NIL)
   ((OR (EQ PADRAO 'QS) (EQ PADRAO 'EX)) NIL)
   ((UPPER-CASE-P PADRAO) (LIST (CONS PADRAO FORMA)))
   ((MEMBER PADRAO '(x y z w)) (LIST (CONS PADRAO FORMA)))
   ((ATOM PADRAO) NIL)
   (REMOVE-DUPLICATES
      (APPEND (PARELHAS (CAR PADRAO) (CAR FORMA))
         (PARELHAS (CDR PADRAO) (CDR FORMA))) EQUAL))




;Parte 33:

(DEFUN CORRESPONDENTE (EL PARES)
   (CDR (ASSOC EL PARES)))

;Parte 34:

(DEFUN SELEC (A LISTA SLISTA)
   (LOOP
      ((NULL LISTA) SLISTA)

      (IF (EQL ( CAAR LISTA) A)
         (PUSH (POP  LISTA) SLISTA)
         (SETQ LISTA (CDR LISTA)))))



;35:

(DEFUN SEGMENTO (EL LISTA SUBLISTA)
   ((NULL LISTA) NIL)
   (SETQ SUBLISTA (MEMBER EL LISTA 'EQUAL))
   ((NEQL SUBLISTA NIL) SUBLISTA)
   (OR (AND (LISTP (CAR LISTA)) (SEGMENTO EL (CAR LISTA)))
      (SEGMENTO EL (CDR LISTA))))

;36:

(DEFUN SUC (EL LISTA)
   ((NEQL (CDR (SEGMENTO EL LISTA)) NIL)
      (LIST (CADR (SEGMENTO EL LISTA))))
   NIL)


;37 e 38:

(DEFUN SELECAO (LISTA)
   ((NULL LISTA) NIL)
   ((INST (CAR LISTA)) (CONS (CAR LISTA) (SELECAO (CDR LISTA))))
   ( SELECAO (CDR LISTA)))

(DEFUN INST(EL)
   (AND (LISTP EL) (EQL (LENGTH EL) 5) (NUMBERP (CAR EL ))))

;39:
(DEFUN FLS (EL LISTA)
   ((NULL (SUC EL LISTA)) NIL)
   (SELECAO (CAR (SUC EL LISTA))))


;40a:
(DEFUN INSTANCIE (FORMULA x TERMO)
   ((EQ x TERMO) FORMULA)
   ((AND (OR (EQ (CAR FORMULA)  'QS)  (EQ (CAR FORMULA)  'EX))
         (ATOM (CADR FORMULA))
         (NOT (NULL (CDDR FORMULA))))
      ((EQ (CADR FORMULA) x) FORMULA)
      (LIST (CAR FORMULA)
         (CADR FORMULA)
         (INSTANCIE (THIRD FORMULA) x TERMO)))

   ((AND (OR (EQ (CAR FORMULA) 'n )
            (EQ (CAR FORMULA) 'co)
            (EQ (CAR FORMULA) 'nf))
         (NOT (NULL (CDR FORMULA))))
      (LIST (CAR FORMULA) (INSTANCIE (CADR FORMULA) x TERMO)))
   ;40b:
   ((OR(EQ(CADR FORMULA) 'ent)
         (EQ(CADR FORMULA) 'e   )
         (EQ(CADR FORMULA) 'ou )
         (EQ(CADR FORMULA) 'eq  ))
      (LIST(INSTANCIE(CAR FORMULA) x TERMO)
         (CADR FORMULA)
         (INSTANCIE(THIRD FORMULA) x TERMO)))
   ((EQUAL FORMULA x) TERMO)
   ((ATOM FORMULA) FORMULA)
   (SUBSTITUTE TERMO x FORMULA))

;41:

(DEFUN I ( NO A B F1 F2)
   ((CASA SENT '(A ent B))
      (SETQ A (FIRST SENT))
      (SETQ B (THIRD SENT))
      ;    (PRINT (LIST "APLICANDO I" SENT A B))

      (SETQ F1 (LIST 1 (LIST 'nf A) 0 0 0))
      (SETQ F2 (LIST 2 B 0 0 0))
      (RAMIFIQUE NO (LIST NIL (LIST F1 F2)))))


;42:

(DEFUN C (NO A B F1 F2)
   ((CASA SENT '(A e B))
      (SETQ A (FIRST SENT))
      (SETQ B (THIRD SENT))
      ;    (PRINT (LIST "APLICANDO C (e)" SENT  A B))
      (SETQ F1 (LIST 1 A 0 0 0))
      (SETQ F2 (LIST 2 B 0 0 0))
      (RAMIFIQUE NO (LIST NIL (LIST F1 (LIST F2))))))

;43:

(DEFUN D (NO A B F1 F2)
   ((CASA SENT '(A ou B))
      (SETQ A (FIRST SENT))
      (SETQ B (THIRD SENT))
      ;    (PRINT (LIST "APLICANDO D (ou)" SENT A B))
      (SETQ F1 (LIST 1 A 0 0 0)) ; 0 ou O?

      (SETQ F2 (LIST 2 B 0 0 0)) ; 0 ou O?
      (RAMIFIQUE NO (LIST NIL (LIST F1 F2)))))

;44:
(DEFUN EQV (NO A B F1 F2 F3 F4 )
   ((CASA SENT '(A eq B))
      (SETQ A (FIRST SENT))
      (SETQ B (THIRD SENT))
      ;   (PRINT (LIST "APLICANDO EQV (eq)" SENT  A B))
      (SETQ F1 (LIST 1 A 0 0 0))
      (SETQ F2 (LIST 2 B 0 0 0))
      (SETQ F3 (LIST 3 (LIST 'nf A) 0 0 0))
      (SETQ F4 (LIST 4 (LIST 'nf B) 0 0 0))
      (RAMIFIQUE NO (LIST NIL (LIST F1 (LIST F2) F3 (LIST F4))))))

;45:
(DEFUN CO (NO A F1 F2)
   ((CASA SENT '(co A))
      (SETQ A (SECOND SENT))
      ;   (PRINT (LIST "APLICANDO CO (co)" SENT A))
      (SETQ F1 (LIST 1 (LIST 'nf A) 0 0 0))
      (SETQ F2 (LIST 2 (LIST 'nf (LIST 'n A)) 0 0 0))
      (RAMIFIQUE NO (LIST NIL (LIST F1 F2)))))


;46 e 47:
(DEFUN QU (NO A x F1 F2)
   ((CASA SENT '(QS x A))
      (SETQ A (THIRD SENT))
      (SETQ x (SECOND SENT))
      ;  (PRINT "APLICANDO QU (QS)")
      (SETQ F1
         (LIST 1 (LIST 'QS x A) 0 0 (ADD1 (GET NO 'INSTANCIAS))))
      ( SETQ F2 (LIST 2 A (ADD1 (GET NO 'INSTANCIAS)) x 0))
      (RAMIFIQUE NO (LIST NIL (LIST F1 (LIST F2))))))

(DEFUN QE (NO A x F1 F2)
   ((CASA SENT '(EX x A))
      (SETQ A (THIRD SENT))
      (SETQ x (SECOND SENT))
      ;    (PRINT "APLICANDO QE (EX)")
      (SETQ F1 (LIST 1 A -1 x 0))
      ( RAMIFIQUE NO (LIST NIL (LIST F1)))))

;48:
(DEFUN NFI (NO A B F1 F2)
   ((CASA SENT '(nf (A ent B)))
      (SETQ A  (FIRST (SECOND SENT)))
      (SETQ B  (THIRD (SECOND SENT)))

      ;    (PRINT (LIST "APLICANDO NFI (nf (A ent B))" SENT A B))
      (SETQ F1 (LIST 1 A 0 0 0))
      (SETQ F2 (LIST 2 (LIST 'nf B) 0 0 0))
      (RAMIFIQUE NO (LIST NIL (LIST F1 (LIST F2 ))))))

;49:
(DEFUN NFC (NO A B F1 F2 )
   ( ( CASA SENT '( nf (A e B) ) )
      ( SETQ A ( FIRST ( SECOND SENT ) ) )
      ( SETQ B ( THIRD (SECOND SENT) ) )
      ;    (PRINT (LIST "APLICANDO NFC (nf (A e B))" SENT A B))
      ( SETQ F1 (LIST 1 (LIST 'nf A) 0 0 0) )
      ( SETQ F2 (LIST 2 (LIST 'nf B) 0 0 0) )
      (RAMIFIQUE NO (LIST NIL (LIST F1 F2) ) ) ) )


;50:
(DEFUN NFD (NO A B F1 F2)
   ((CASA SENT '(nf (A ou B)))
      (SETQ A (FIRST (SECOND SENT)))
      (SETQ B (THIRD (SECOND SENT)))
      ;    (PRINT (LIST "APLICANDO NFD (nf (A ou B))" SENT A B))
      (SETQ F1 (LIST 1 (LIST 'nf A) 0 0 0))
      (SETQ F2 (LIST 2 (LIST 'nf B) 0 0 0))

      (RAMIFIQUE NO (LIST NIL (LIST F1 (LIST F2))))))



;51:
(DEFUN NFEQV (NO A B F1 F2)
   ((CASA SENT '(nf (A 'eq B)))
      (SETQ A (FIRST (SECOND SENT)))
      (SETQ B (THIRD (SECOND SENT)))
      ;    (PRINT "APLICANDO NFEQV (nf (A eq B))")
      (SETQ F1 (LIST 1 A 0 0 0))
      (SETQ F2 (LIST 2 (LIST 'nf B) 0 0 0))
      (SETQ F3 (LIST 3 (LIST 'nf A) 0 0 0))
      (SETQ F4 (LIST 4 B 0 0 0))
      (RAMIFIQUE NO (LIST NIL (LIST F1 (LIST F2) F3 (LIST F4))))))

;52:
(DEFUN NFCO (NO A F1 F2)
   ((CASA SENT '(nf (co A)))
      (SETQ A (SECOND (SECOND SENT)))
      ;    (PRINT (LIST "APLICANDO NFCO (nf (co A))" SENT A))
      (SETQ F1 (LIST 1 A 0 0 0))
      (SETQ F2 (LIST 2 (LIST 'n A) 0 0 0))
      (RAMIFIQUE NO (LIST NIL (LIST F1 (LIST F2))))))


;53:
( DEFUN NFQU ( NO A x F1 )
   ( ( CASA SENT '(nf (QS x A ) ) )
      ( SETQ A ( THIRD     ( SECOND SENT ) ) )
      ( SETQ x ( SECOND ( SECOND SENT ) ) )
      ;          (PRINT "APLICANDO NFQU (nf (QS x A))")
      ( SETQ F1 ( LIST 1 ( LIST   'nf A )  -1  x  0 ) )
      ( RAMIFIQUE NO ( LIST NIL ( LIST F1 ) ) ) ) )


;54:
(DEFUN NFQE (NO A x F1)
   ((CASA SENT '(nf (EX x A)))
      (SETQ A (THIRD (SECOND SENT)))
      (SETQ x (SECOND (SECOND SENT)))
      ;    (PRINT "APLICANDO NFQE (nf (EX x A))")
      (SETQ F1 (LIST 1 (LIST 'QS x (LIST 'nf  A)) 0 0 1))
      (SETQ F2 (LIST 2 (LIST 'nf A) 1 x 0))
      (RAMIFIQUE NO (LIST NIL ( LIST F1 (LIST F2))))))

;55:
(DEFUN NI (NO A B F1 F2 F3 F4)
   ((CASA SENT '(n (A ent B)))

      (SETQ A (FIRST (SECOND SENT)))
      (SETQ B (THIRD (SECOND SENT)))
      ;    (PRINT (LIST "APLICANDO NI (n (A ent B))" SENT A B))
      (SETQ F1 (LIST 1 (LIST 'nf (LIST A 'ent B)) 0 0 0))
      (SETQ F2 (LIST 2 B 0 0 0))
      (SETQ F3 (LIST 3 (LIST 'nf (LIST 'co A)) 0 0 0))
      (SETQ F4 (LIST 4 (LIST 'n B) 0 0 0))
      (RAMIFIQUE NO (LIST NIL (LIST F1 F2 (LIST F3 F4))))))




;56:

(DEFUN NC (NO A B F1 F2 F3 F4 F5)
   	((CASA SENT '(n (A  e  B)))
      	(SETQ A (FIRST (SECOND SENT)))
      	(SETQ B (THIRD (SECOND SENT)))
      ;   (PRINT (LIST "APLICANDO NC (n (A e B))" SENT A B))
      	(SETQ F1 (LIST 1 ( LIST 'nf (LIST A 'e B)) 0 0 0))
      	(SETQ F2 (LIST 2 A 0 0 0))
      	(SETQ F3 (LIST 3 B 0 0 0))
      	(SETQ F4 (LIST 4 (LIST 'n A) 0 0 0))
      	(SETQ F5 (LIST 5 (LIST 'n B) 0 0 0))

      	(RAMIFIQUE NO (LIST NIL (LIST F1 F2 (LIST F3 (LIST F4 F5)))))))

;57:

( DEFUN ND (NO A B F1 F2 F3)
   (( CASA SENT  '(n  (A ou B)))
      ( SETQ A  (FIRST (SECOND SENT)))
      ( SETQ B  (THIRD (SECOND SENT)))
      ;    (PRINT (LIST "APLICANDO ND (n (A ou B))" SENT A B))
      ( SETQ F1  (LIST 1 (LIST  'nf  (LIST A 'ou B)) 0 0 0))
      ( SETQ F2  (LIST 2 (LIST  'nf  (LIST 'co A)) 0 0 0))
      ( SETQ F3  (LIST 3 (LIST  'nf  (LIST 'co B)) 0 0 0))
      (RAMIFIQUE NO (LIST NIL (LIST F1 F2 F3 )))))

;58:
(DEFUN NEQV (NO A B F1 F2 F3 F4 F5)
   ((CASA SENT '(n (A eq B )))
      (SETQ A (FIRST (SECOND SENT)))
      (SETQ B (THIRD (SECOND SENT)))
      ;    (PRINT "APLICANDO NEQV (n (A eq B))")
      (SETQ F1 (LIST 1 (LIST 'nf (LIST A 'eq B)) 0 0 0))
      (SETQ F2 (LIST 2 (LIST A 'eq B) 0 0 0))
      (SETQ F3 (LIST 3 (LIST 'n (LIST A 'ent B)) 0 0 0))
      (SETQ F4 (LIST 4 (LIST 'n (LIST B 'ent A)) 0 0 0))

      (RAMIFIQUE NO (LIST NIL (LIST F1 F2 (LIST F3 F4))))))

;59:
(DEFUN NCO (NO A F1)
   ((CASA SENT '(n (co A)))
      (SETQ A (SECOND (SECOND SENT)))
      ;    (PRINT (LIST "APLICANDO NCO (n (co A))" SENT A))
      (SETQ F1 (LIST 1 (LIST 'nf ( LIST 'co A)) 0 0 0))
      (RAMIFIQUE NO (LIST NIL (LIST F1)))))

;60:
(DEFUN NN1 (NO A F1 F2)
   ((CASA SENT '(n (n A)))
      (SETQ A (SECOND (SECOND SENT)))
      ; (PRINT (LIST "APLICANDO NN1 (n (n A))" SENT A NO))
      (SETQ F1(LIST 1 (LIST 'nf (LIST 'n A)) 0 0 0))
      (SETQ F2(LIST 2 (LIST 'nf (LIST 'co A)) 0 0 0))
      (RAMIFIQUE NO (LIST NIL (LIST F1 F2)))))

;61:
(DEFUN NN2(NO A F1)
   ((OR (CASA SENT '(nf (nf A)))
         (CASA SENT '(n (nf A)))
         (CASA SENT '(nf (n A))) )

      (SETQ A (SECOND (SECOND SENT)))
      ;    (PRINT (LIST "APLICANDO NN2 (n* (n* A))" SENT  A ))
      (SETQ F1 (LIST 1 A 0 0 0))
      (RAMIFIQUE NO (LIST NIL (LIST F1)))))


;62:
(DEFUN NQ (NO A x sn F1 F2 F3)
   ((OR (CASA SENT '(n (QS x A)))
         (CASA SENT '(n (EX x A))))
      (SETQ A (THIRD (SECOND SENT)))
      (SETQ x (SECOND (SECOND SENT)))
      ;    (PRINT "APLICANDO NQ (n (QS/EX A))")
      (SETQ sn (FIRST (SECOND SENT)))
      (SETQ F1 (LIST 1 (LIST 'nf (LIST sn x A)) 0 0 0))
      (SETQ F2 (LIST 2 (LIST sn x  A) 0 0 0))
      (SETQ F3 (LIST 3 (LIST 'nf (LIST 'co A)) -1 x 0))
      (RAMIFIQUE NO (LIST NIL (LIST F1 F2 (LIST F3))))))

;63:
(DEFUN N (NO A F1 F2)
   ((CASA SENT '(n A))
      ;    (PRINT (LIST "APLICANDO N (n A)" SENT))
      (SETQ A (CADR SENT))

      (SETQ F1 (LIST 1(LIST 'nf A) 0 0 0))
      (SETQ F2 (LIST 2 A 0 0 0))
      (RAMIFIQUE NO (LIST NIL (LIST F1 F2))))
)

;64:
(DEFUN EXCL (NO)
   ((AND (NULL (GET NO 'FILHOS))
         (NULL (GET NO 'INCONSISTENCIA)))
      'satisfativel)
   'T)

(RDS)

