PROCEDURE rutina
    PARAMETERS vv,lnn


    IF lnn = 4 then

        nvocod = vv+"05"
        nv95 = vv+"99"

        SELECT COUNT(*) FROM puc1;
            WHERE LEFT(puc1.codcontabl,6) = nvocod ;
            INTO ARRAY CCTTD

        DO WHILE CCTTD > 0 AND nvocod <= nv95

            SELECT COUNT(*) FROM puc1 ;
                WHERE LEFT(puc1.codcontabl,6) = nvocod ;
                INTO ARRAY CCTTD

            IF CCTTD > 0
                nvocod = STR((VAL(nvocod) + 5),6)
            ENDIF

        ENDDO

        IF nvocod <= nv95

            INSERT INTO puc1.DBF (codcontabl, tiposaldo ) VALUES (nvocod, qqq.naturaleza)

        ELSE
            MESSAGEBOX("No se puede Crear más Subcuentas",16,"¡No se puede!")
        ENDIF
    ENDIF

    IF lnn = 6  AND variables.qdigit = 6 then

        nvocod = vv
        O = "o"
        SELECT COUNT(*)FROM puc1;
            WHERE LEFT(puc1.codcontabl,lnn) = vv;
            INTO ARRAY CUANTOS

        IF CUANTOS = 0

            SELECT LEFT(ALLTRIM(puc.denomina),30);
                FROM puc INTO ARRAY ttt;
                WHERE  LEFT (puc.codigo,lnn) = LEFT(vv,lnn)

            INSERT INTO puc1.DBF (codcontabl, nombrecta, original, tiposaldo );
                VALUES (nvocod,ttt,O , qqq.naturaleza )

        ELSE
            MESSAGEBOX("Esta Cuenta ya Existe",16,"¡No se puede!")
        ENDIF

    ELSE
        IF lnn = 6 AND variables.qdigit = 8 then

            nvocod = vv + "01"
            nv95 = vv + "99"

            SELECT COUNT(*) FROM puc1 ;
                WHERE LEFT(puc1.codcontabl,8) = nvocod ;
                INTO ARRAY CCTTD

            DO WHILE CCTTD > 0 AND nvocod <= nv95

                SELECT COUNT(*) FROM puc1 ;
                    WHERE LEFT(puc1.codcontabl,8) = nvocod ;
                    INTO ARRAY CCTTD

                IF CCTTD > 0
                    nvocod = STR((VAL(nvocod)+1),8)
                ENDIF

            ENDDO

            IF nvocod <= nv95

                INSERT INTO puc1.DBF (codcontabl, tiposaldo) VALUES (nvocod, qqq.naturaleza)

            ELSE
                MESSAGEBOX("No se puede Crear más Subcuentas",16,"¡No se puede!")
            ENDIF
        ENDIF
    ENDIF
    RETURN
