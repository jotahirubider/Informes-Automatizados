# INFORME AISLAMIENTOS A VALIDAR A LAS 12:15
$Sta = New-ScheduledTaskAction -Execute "\\woody\asan\Servicios\EnfermeriaMedPreventiva\APLICACIONES\AUTOMATIZACION\informeAislamientos\makeReport.bat"
$Stt = New-ScheduledTaskTrigger -DaysOfWeek @('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday') -At 12:15 -Weekly
Register-ScheduledTask InformeAislamientosValidar -Action $Sta -Trigger $Stt

# INFORME AISLAMIENTOS VALIDADO A LAS 14:00
$Stt = New-ScheduledTaskTrigger -DaysOfWeek @('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday') -At 14:10 -Weekly
Register-ScheduledTask InformeAislamientosValidado -Action $Sta -Trigger $Stt

# INFORME COVID-19 A LAS 13:30
$Sta = New-ScheduledTaskAction -Execute "\\woody\asan\Servicios\EnfermeriaMedPreventiva\APLICACIONES\AUTOMATIZACION\informeCOVID\makeReport.bat"
$Stt = New-ScheduledTaskTrigger -DaysOfWeek @('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday') -At 13:30 -Weekly
Register-ScheduledTask InformeCOVID -Action $Sta -Trigger $Stt

# INFORME FIRMA A LAS 12:40
$Sta = New-ScheduledTaskAction -Execute "\\woody\asan\Servicios\EnfermeriaMedPreventiva\APLICACIONES\AUTOMATIZACION\informeFIRMA\makeReport.bat"
$Stt = New-ScheduledTaskTrigger -DaysOfWeek @('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday') -At 13:10 -Weekly
Register-ScheduledTask InformeFIRMA -Action $Sta -Trigger $Stt

# Unregister-ScheduledTask -TaskName InformeAislamientosValidar
# Unregister-ScheduledTask -TaskName InformeAislamientosValidado
# Unregister-ScheduledTask -TaskName InformeCOVID
# Unregister-ScheduledTask -TaskName InformeFIRMA
# Get-ScheduledTask