      subroutine broiler

C     ~ ~ ~ PURPOSE ~ ~ ~
C     this subroutine computes the lake hydrologic pesticide balance.
C     ~ ~ ~ INCOMING VARIABLES ~ ~ ~
C     variable          |definition
C     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
C     nChicks           | number of chicks, less than 1/2 of life
C     nBroilers         | number of birds, older than 1/2 of life
C     wtChicks          | weight of chicks
C     wtBroilers        | weight of mature birds
C     ME_young          | Energy of diet, kcal / kg
C     ME_mature         | Energy of diet, kcal / kg
C     CPy               | Crude protein for growing, %
C     CPo               | Crude protein for finishing, %
C     nP_young          | non-phytate P in diet for growing, %
C     nP_mature         | non-phytate P in diet for finishing, %
C     P_young           | added Phytate in diet for growing, U/kg diet
C     P_mature          | added Phytate in diet for finishing, U/kg diet
C     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

C     ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
C     variable          | definition
C     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
C     nChicks           | number of chicks, less than 1/2 of life
C     nBroilers         | number of birds, older than 1/2 of life
C     wtChicks          | weight of chicks
C     wtBroilers        | weight of mature birds
C     intake            | feed intake, total kg
C     Nexc              | Total N excretion, kg
C     Pexc              | Total P excretion, kg
C     Norg_frac         | organic N fraction of excreted 
C     Ninorg_frac       | inorganic N fraction of excreted
C     Porg_frac         | organic P fraction of excreted 
C     Pinorg_frac       | inorganic P fraction of excreted
C     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

C     ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
C     name        |units         |definition
C     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
C     
C     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

C     ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
