SELECT insaplica.codtipo, insaplica.codinsum, ; 
    SUM(auxapli.cantusada) AS cantus, ; 
    SUM((auxapli.cantusada * (compins.costunit/compins.equivale)) ; 
    +(auxapli.cantusada*compins.sobrecosto)) AS vtot, " " ; 
    FROM  sacfadb!insaplica inner JOIN sacfadb!auxapli ; 
    inner JOIN sacfadb!compins ; 
    ON  auxapli.codcomp = compins.codcomp ; 
    ON  insaplica.codinsap = auxapli.codinsap ; 
    WHERE insaplica.numcontrol = 23 ; 
    GROUP BY auxapli.codtipo, auxapli.codinsum ; 
    UNION ; 
SELECT insaplica.codtipo, insaplica.codinsum, ; 
    " ", " ", COUNT(insaplica.codinsum)AS cuant ; 
    FROM sacfadb!insaplica ; 
    WHERE insaplica.numcontrol = 23 ; 
    GROUP BY insaplica.codtipo, insaplica.codinsum ; 
    INTO TABLE tem_cos_insum
