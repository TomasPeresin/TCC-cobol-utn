      ******************************************************************
      * Author: Peresin Tomas Ignacio
      * Date: 2022
      * Purpose: Trabajo Comprension de Contenidos
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCC-Peresin.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT VENTA ASSIGN TO "VENTA.DAT"
           ORGANIZATION IS INDEXED
           RECORD KEY IS ID-PRODUCTO
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS ESTADO.

           SELECT PROVEEDOR ASSIGN TO "PROVEEDOR.DAT"
           ORGANIZATION IS SEQUENTIAL
           FILE STATUS IS ESTADO.

           SELECT RUBRO ASSIGN TO "RUBRO.DAT"
           ORGANIZATION IS SEQUENTIAL
           FILE STATUS IS ESTADO.
       DATA DIVISION.
       FILE SECTION.
           FD VENTA.
           01 REG-VENTA.
               03 ID-PRODUCTO PIC 999.
               03 NOMBRE PIC X(15).
               03 FECHA-VENTA.
                   10 ANIO PIC 9(4).
                   10 MES PIC  9(2).
                   10 DIA PIC  9(2).
               03 TOTAL-VENDIDO PIC 999.
               03 ID-PROVEEDOR-V PIC 999.
               03 ID-RUBRO-V PIC 999.

           FD PROVEEDOR.
           01 REG-PROVEEDOR.
               03 ID-PROVEEDOR PIC 999.
               03 NOMBRE PIC A(15).
               03 APELLIDO PIC A(15).
               03 EMAIL PIC X(20).
               03 NRO-CELULAR PIC X9(13).

           FD RUBRO.
           01 REG-RUBRO.
               03 ID-RUBRO PIC 999.
               03 DESCRIPCION PIC X(30).

       WORKING-STORAGE SECTION.
           77 OP-MENU PIC 9 VALUE 9.
           77 BANDERA PIC 9 VALUE 9.
           77 ESTADO PIC 99.
           77 OP-USUARIO PIC 9 VALUE 9.

           01 REG-VENTA-VC.
               03 ID-PRODUCTO-VC PIC 999.
               03 NOMBRE-VC PIC X(15).
               03 FECHA-VENTA-VC.
                   10 ANIO-VC PIC 9(4).
                   10 MES-VC PIC  9(2).
                   10 DIA-VC PIC  9(2).
               03 TOTAL-VENDIDO-VC PIC 999.
               03 ID-PROVEEDOR-VC PIC 999.
               03 ID-RUBRO-VC PIC 999.

           01 REG-PROVEEDOR-PC.
               03 ID-PROVEEDOR-PC PIC 999.
               03 NOMBRE-PC PIC A(15).
               03 APELLIDO-PC PIC A(15).
               03 EMAIL-PC PIC X(20).
               03 NRO-CELULAR-PC PIC X9(13).

           01 REG-RUBRO-RC.
               03 ID-RUBRO-RC PIC 999.
               03 DESCRIPCION-RC PIC X(30).

           01 CAMPOS-FECHA-ACTUAL.
               05 FECHA-ACTUAL.
                   10 ANIO-ACTUAL  PIC 9(4).
                   10 MES-ACTUAL   PIC 9(2).
                   10 DIA-ACTUAL   PIC 9(2).
       PROCEDURE DIVISION.
       INICIO.
           PERFORM PRUEBA-ARCHIVOS.
           PERFORM MENU UNTIL OP-MENU = 0.
           STOP RUN.

       MENU.
           DISPLAY "BIENVENIDO A LA VERDULERIA.".
           DISPLAY "ELIJA UNA OPCION: ".
           DISPLAY "1-CARGA PROVEEDOR.".
           DISPLAY "2-CARGA RUBRO.".
           DISPLAY "3-CARGA VENTA.".
           DISPLAY "4-LISTAR.".
           DISPLAY "0-SALIR".
           ACCEPT OP-MENU.
                IF OP-MENU=1 THEN PERFORM CARGA-P UNTIL OP-USUARIO = 2
           ELSE IF OP-MENU=2 THEN PERFORM CARGA-R UNTIL OP-USUARIO = 2
           ELSE IF OP-MENU=3 THEN PERFORM CARGA-V UNTIL OP-USUARIO = 2
           ELSE IF OP-MENU=4 THEN PERFORM LISTAR.
           COMPUTE OP-USUARIO = 9.

       CARGA-P.
           COMPUTE BANDERA = 9.
           OPEN INPUT PROVEEDOR.
           DISPLAY "VA A CARGAR UN PROVEEDOR".
           DISPLAY "POR FAVOR INGRESE SU ID:"
           ACCEPT ID-PROVEEDOR-VC.
           PERFORM CONSULTA-PROVEEDOR
               UNTIL (BANDERA = 0 OR BANDERA = 1).
           CLOSE PROVEEDOR.
           IF BANDERA = 1 THEN
               DISPLAY "ID DE PROVEEDOR EN USO."
               DISPLAY "INGRESE OTRO ID."
               ELSE OPEN EXTEND PROVEEDOR
                   PERFORM CARGA-PROVEEDOR
                   CLOSE PROVEEDOR.
           DISPLAY "¿DESEA INGRESAR OTRO PROVEEDOR? 1-SI/2-NO"
           ACCEPT OP-USUARIO.

       CONSULTA-PROVEEDOR.
           READ PROVEEDOR INTO REG-PROVEEDOR-PC AT END
               COMPUTE BANDERA = 0.
           IF ID-PROVEEDOR-VC = ID-PROVEEDOR-PC THEN
               COMPUTE BANDERA = 1.
      *    VERIFICA SI YA EXISTE LA ID.

       CARGA-PROVEEDOR.
           DISPLAY "INGRESE NOMBRE:" ACCEPT NOMBRE-PC.
           DISPLAY "INGRESE APELLIDO: " ACCEPT APELLIDO-PC.
           DISPLAY "INGRESE EMAIL: " ACCEPT EMAIL-PC.
           DISPLAY "INGRESE CELULAR: " ACCEPT NRO-CELULAR-PC.
           MOVE ID-PROVEEDOR-VC TO ID-PROVEEDOR-PC.
           WRITE REG-PROVEEDOR FROM REG-PROVEEDOR-PC.

       CARGA-V.
           COMPUTE BANDERA = 9.
           PERFORM VERIFICAR-PROVEEDOR.
           IF BANDERA = 0 THEN DISPLAY
           "NO SE PUEDE INGRESAR EL PRODUCTO YA QUE EL PROVEEDOR "
           "NO ESTÁ REGISTRADO"
           ELSE
               PERFORM VERIFICAR-RUBRO
               IF BANDERA = 0 THEN DISPLAY
               "NO SE PUEDE INGRESAR EL PRODUCTO YA QUE EL RUBRO "
               "NO ESTÁ REGISTRADO"
               ELSE
                    OPEN EXTEND VENTA
                    PERFORM CARGA-VENTA
                    CLOSE VENTA.
           DISPLAY "INGRESAR OTRA VENTA? 1-SI/2-NO".
           ACCEPT OP-USUARIO.

       VERIFICAR-PROVEEDOR.
           OPEN INPUT PROVEEDOR.
           DISPLAY "INGRESARA UNA NUEVA VENTA".
           DISPLAY "INGRESE LA ID DEL PROVEEDOR:".
           ACCEPT ID-PROVEEDOR-VC.
           PERFORM CONSULTA-PROVEEDOR
           UNTIL (BANDERA = 1 OR BANDERA = 0).
           CLOSE PROVEEDOR.

       VERIFICAR-RUBRO.
           OPEN INPUT RUBRO.
           DISPLAY "INGRESE LA ID DEL RUBRO:".
           ACCEPT ID-RUBRO-VC.
           COMPUTE BANDERA = 9.
           PERFORM CONSULTA-RUBRO
           UNTIL (BANDERA = 1 OR BANDERA = 0).
           CLOSE RUBRO.

       CARGA-VENTA.
           DISPLAY "INGRESE ID DEL PRODUCTO: " ACCEPT ID-PRODUCTO-VC.
           DISPLAY "INGRESE NOMBRE DEL PRODUCTO: " ACCEPT NOMBRE-VC.
           MOVE FUNCTION CURRENT-DATE TO CAMPOS-FECHA-ACTUAL.
           COMPUTE BANDERA = 9.
           PERFORM CARGA-FECHA UNTIL BANDERA = 1.
           DISPLAY "INGRESE TOTAL VENDIDO: " ACCEPT TOTAL-VENDIDO-VC.
           WRITE REG-VENTA FROM REG-VENTA-VC.

       CARGA-FECHA.
           DISPLAY "INGRESE DIA DE VENTA".
           ACCEPT DIA-VC.
           DISPLAY "INGRESE MES DE VENTA".
           ACCEPT MES-VC.
           DISPLAY "INGRESE AÑO".
           ACCEPT ANIO-VC.
           IF (ANIO-VC = ANIO-ACTUAL AND MES-VC = MES-ACTUAL AND
               DIA-VC <= DIA-ACTUAL) THEN COMPUTE BANDERA = 1
           ELSE IF (ANIO-VC = ANIO-ACTUAL AND MES-VC < MES-ACTUAL)
               THEN COMPUTE BANDERA = 1
           ELSE IF (ANIO-VC < ANIO-ACTUAL) COMPUTE BANDERA = 1
           ELSE
              DISPLAY "INGRESE UNA FECHA ANTERIOR O IGUAL A LA ACTUAL.".

       CARGA-R.
           OPEN INPUT RUBRO.
           DISPLAY "VA A CARGAR UN RUBRO".
           DISPLAY "POR FAVOR INGRESE LA ID:"
           ACCEPT ID-RUBRO-VC.
           COMPUTE BANDERA = 9.
           PERFORM CONSULTA-RUBRO
               UNTIL BANDERA = 0 OR BANDERA = 1.
           CLOSE RUBRO
           IF BANDERA = 1 THEN
               DISPLAY "ID DE RUBRO EN USO."
               DISPLAY "INGRESE OTRO ID."
               ELSE OPEN EXTEND RUBRO
                   PERFORM CARGA-RUBRO
                   CLOSE RUBRO.
           DISPLAY "¿DESEA INGRESAR OTRO RUBRO? 1-SI/ 2-NO"
           ACCEPT OP-USUARIO.

       CONSULTA-RUBRO.
           READ RUBRO INTO REG-RUBRO-RC AT END
               COMPUTE BANDERA = 0.
           IF ID-RUBRO-VC = ID-RUBRO-RC THEN
               COMPUTE BANDERA = 1.
      *    VERIFICA SI YA EXISTE LA ID.

       CARGA-RUBRO.
           DISPLAY "INGRESE DESCRIPCION:" ACCEPT DESCRIPCION-RC.
           MOVE ID-RUBRO-VC TO ID-RUBRO-RC.
           WRITE REG-RUBRO FROM REG-RUBRO-RC.

       LISTAR.
           COMPUTE OP-USUARIO = 1.
           OPEN INPUT PROVEEDOR
           DISPLAY "PROVEEDORES:".
           PERFORM LISTAR-PROVEEDOR UNTIL OP-USUARIO = 2.
           CLOSE PROVEEDOR.
           COMPUTE OP-USUARIO = 1.
           OPEN INPUT RUBRO.
           DISPLAY "RUBROS:".
           PERFORM LISTAR-RUBRO UNTIL OP-USUARIO = 2.
           CLOSE RUBRO.
           COMPUTE OP-USUARIO = 1.
           OPEN INPUT VENTA.
           DISPLAY "VENTAS:".
           PERFORM LISTAR-VENTA UNTIL OP-USUARIO = 2.
           CLOSE VENTA.

       LISTAR-PROVEEDOR.
           READ PROVEEDOR INTO REG-PROVEEDOR-PC
           AT END COMPUTE OP-USUARIO = 2.
           IF OP-USUARIO = 2 THEN DISPLAY " "
               ELSE
                   DISPLAY REG-PROVEEDOR-PC
                   DISPLAY "--------------------------------".

       LISTAR-RUBRO.
           READ RUBRO INTO REG-RUBRO-RC AT END COMPUTE OP-USUARIO = 2.
           IF OP-USUARIO = 2 THEN DISPLAY " "
               ELSE
                   DISPLAY REG-RUBRO-RC
                   DISPLAY "--------------------------------".

       LISTAR-VENTA.
           READ VENTA INTO REG-VENTA-VC AT END COMPUTE OP-USUARIO = 2.
            IF OP-USUARIO = 2 THEN DISPLAY " "
               ELSE
                   DISPLAY REG-VENTA-VC
                   DISPLAY "--------------------------------".

       PRUEBA-ARCHIVOS.
           OPEN INPUT PROVEEDOR.
           IF ESTADO = 35 THEN PERFORM CARGA-ARCHIVO-PROVEEDOR.
           OPEN INPUT RUBRO.
           IF ESTADO = 35 THEN PERFORM CARGA-ARCHIVO-RUBRO.
           OPEN INPUT VENTA.
           IF ESTADO = 35 THEN PERFORM CARGA-ARCHIVO-VENTA.
           CLOSE PROVEEDOR.
           CLOSE RUBRO.
           CLOSE VENTA.

       CARGA-ARCHIVO-PROVEEDOR.
           CLOSE PROVEEDOR.
           OPEN OUTPUT PROVEEDOR.

       CARGA-ARCHIVO-RUBRO.
           CLOSE RUBRO.
           OPEN OUTPUT RUBRO.

       CARGA-ARCHIVO-VENTA.
           CLOSE VENTA.
           OPEN OUTPUT VENTA.

       END PROGRAM TCC-Peresin.
