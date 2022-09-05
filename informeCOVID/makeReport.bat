@echo off
::::::::::::::::::::::: CONFIGURACION DE RUTAS ::::::::::::::::::::::::::::::::::
:: Configurar Ruta hasta Rscript.exe (i386 - x86 - 32 bits).
set RUTA_RSCRIPT="C:/Program Files/R/R-4.1.2/bin/i386/Rscript.exe"
:: Configurar Ruta hasta carpeta principal que contiene este mismo archivo.
set RUTA_PROGRAMA="\\woody\asan\Servicios\EnfermeriaMedPreventiva\APLICACIONES\AUTOMATIZACION\informeCOVID"
set RUTA_MDB="\\woody\asan\Servicios\EnfermeriaMedPreventiva\APLICACIONES\COMPARTIDO\IRAS\IRAS.mdb"
set RUTA_INFORMES="\\woody\asan\Servicios\EnfermeriaMedPreventiva\INFORMES"

::::::::::::::::::::::: NO MODIFICAR NADA A PARTIR DE AQUI ::::::::::::::::::::::

set RUTA_START="makeReport.R"
echo Generando Informe COVID-19 con fecha %date%
:: BORRAR UNIDAD INFORMES Y CREAR NUEVAMENTE
net use k: /delete /y
net use k: %RUTA_INFORMES% /persistent:no
:: BORRAR UNIDAD PROGRAMA Y CREAR NUEVAMENTE
net use j: /delete /y
net use j: %RUTA_PROGRAMA% /persistent:no
:: SET RUTA AL PROGRAMA
J:
:: EJECUTAR PROGRAMA
%RUTA_RSCRIPT% %RUTA_START%