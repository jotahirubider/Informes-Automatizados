Write-Host Configurando alerta recordatorio de informe...
$Sta = New-ScheduledTaskAction -Execute "\\woody\asan\Servicios\EnfermeriaMedPreventiva\APLICACIONES\AUTOMATIZACION\scheduledTasks\alertaInforme.bat"
$Stt = New-ScheduledTaskTrigger -DaysOfWeek @('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday') -At 12:20 -Weekly
Register-ScheduledTask AlertaInforme -Action $Sta -Trigger $Stt