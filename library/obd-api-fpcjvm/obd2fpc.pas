{ Imports for Java packages/classes: com. }
unit obd2fpc;
{$mode delphi}

interface

type
  CGPOCPSpacesOffCommand = class;
  Arr1CGPOCPSpacesOffCommand = array of CGPOCPSpacesOffCommand;
  Arr2CGPOCPSpacesOffCommand = array of Arr1CGPOCPSpacesOffCommand;
  Arr3CGPOCPSpacesOffCommand = array of Arr2CGPOCPSpacesOffCommand;

  CGPOENoDataException = class;
  Arr1CGPOENoDataException = array of CGPOENoDataException;
  Arr2CGPOENoDataException = array of Arr1CGPOENoDataException;
  Arr3CGPOENoDataException = array of Arr2CGPOENoDataException;

  CGPOCFFuelTrimCommand = class;
  Arr1CGPOCFFuelTrimCommand = array of CGPOCFFuelTrimCommand;
  Arr2CGPOCFFuelTrimCommand = array of Arr1CGPOCFFuelTrimCommand;
  Arr3CGPOCFFuelTrimCommand = array of Arr2CGPOCFFuelTrimCommand;

  CGPOCCEquivalentRatioCommand = class;
  Arr1CGPOCCEquivalentRatioCommand = array of CGPOCCEquivalentRatioCommand;
  Arr2CGPOCCEquivalentRatioCommand = array of Arr1CGPOCCEquivalentRatioCommand;
  Arr3CGPOCCEquivalentRatioCommand = array of Arr2CGPOCCEquivalentRatioCommand;

  CGPOEUnknownErrorException = class;
  Arr1CGPOEUnknownErrorException = array of CGPOEUnknownErrorException;
  Arr2CGPOEUnknownErrorException = array of Arr1CGPOEUnknownErrorException;
  Arr3CGPOEUnknownErrorException = array of Arr2CGPOEUnknownErrorException;

  CGPOCPAvailablePidsCommand_41_60 = class;
  Arr1CGPOCPAvailablePidsCommand_41_60 = array of CGPOCPAvailablePidsCommand_41_60;
  Arr2CGPOCPAvailablePidsCommand_41_60 = array of Arr1CGPOCPAvailablePidsCommand_41_60;
  Arr3CGPOCPAvailablePidsCommand_41_60 = array of Arr2CGPOCPAvailablePidsCommand_41_60;

  CGPOCObdCommand = class;
  Arr1CGPOCObdCommand = array of CGPOCObdCommand;
  Arr2CGPOCObdCommand = array of Arr1CGPOCObdCommand;
  Arr3CGPOCObdCommand = array of Arr2CGPOCObdCommand;

  CGPOCFAirFuelRatioCommand = class;
  Arr1CGPOCFAirFuelRatioCommand = array of CGPOCFAirFuelRatioCommand;
  Arr2CGPOCFAirFuelRatioCommand = array of Arr1CGPOCFAirFuelRatioCommand;
  Arr3CGPOCFAirFuelRatioCommand = array of Arr2CGPOCFAirFuelRatioCommand;

  CGPOEAvailableCommandNames = class;
  Arr1CGPOEAvailableCommandNames = array of CGPOEAvailableCommandNames;
  Arr2CGPOEAvailableCommandNames = array of Arr1CGPOEAvailableCommandNames;
  Arr3CGPOEAvailableCommandNames = array of Arr2CGPOEAvailableCommandNames;

  CGPOCPPressureCommand = class;
  Arr1CGPOCPPressureCommand = array of CGPOCPPressureCommand;
  Arr2CGPOCPPressureCommand = array of Arr1CGPOCPPressureCommand;
  Arr3CGPOCPPressureCommand = array of Arr2CGPOCPPressureCommand;

  CGPOCPResetTroubleCodesCommand = class;
  Arr1CGPOCPResetTroubleCodesCommand = array of CGPOCPResetTroubleCodesCommand;
  Arr2CGPOCPResetTroubleCodesCommand = array of Arr1CGPOCPResetTroubleCodesCommand;
  Arr3CGPOCPResetTroubleCodesCommand = array of Arr2CGPOCPResetTroubleCodesCommand;

  CGPOCCTimingAdvanceCommand = class;
  Arr1CGPOCCTimingAdvanceCommand = array of CGPOCCTimingAdvanceCommand;
  Arr2CGPOCCTimingAdvanceCommand = array of Arr1CGPOCCTimingAdvanceCommand;
  Arr3CGPOCCTimingAdvanceCommand = array of Arr2CGPOCCTimingAdvanceCommand;

  CGPOEObdProtocols = class;
  Arr1CGPOEObdProtocols = array of CGPOEObdProtocols;
  Arr2CGPOEObdProtocols = array of Arr1CGPOEObdProtocols;
  Arr3CGPOEObdProtocols = array of Arr2CGPOEObdProtocols;

  CGPOCPDescribeProtocolNumberCommand = class;
  Arr1CGPOCPDescribeProtocolNumberCommand = array of CGPOCPDescribeProtocolNumberCommand;
  Arr2CGPOCPDescribeProtocolNumberCommand = array of Arr1CGPOCPDescribeProtocolNumberCommand;
  Arr3CGPOCPDescribeProtocolNumberCommand = array of Arr2CGPOCPDescribeProtocolNumberCommand;

  CGPOCPAdaptiveTimingCommand = class;
  Arr1CGPOCPAdaptiveTimingCommand = array of CGPOCPAdaptiveTimingCommand;
  Arr2CGPOCPAdaptiveTimingCommand = array of Arr1CGPOCPAdaptiveTimingCommand;
  Arr3CGPOCPAdaptiveTimingCommand = array of Arr2CGPOCPAdaptiveTimingCommand;

  CGPOCCVinCommand = class;
  Arr1CGPOCCVinCommand = array of CGPOCCVinCommand;
  Arr2CGPOCCVinCommand = array of Arr1CGPOCCVinCommand;
  Arr3CGPOCCVinCommand = array of Arr2CGPOCCVinCommand;

  CGPOEMisunderstoodCommandException = class;
  Arr1CGPOEMisunderstoodCommandException = array of CGPOEMisunderstoodCommandException;
  Arr2CGPOEMisunderstoodCommandException = array of Arr1CGPOEMisunderstoodCommandException;
  Arr3CGPOEMisunderstoodCommandException = array of Arr2CGPOEMisunderstoodCommandException;

  CGPOCPEchoOffCommand = class;
  Arr1CGPOCPEchoOffCommand = array of CGPOCPEchoOffCommand;
  Arr2CGPOCPEchoOffCommand = array of Arr1CGPOCPEchoOffCommand;
  Arr3CGPOCPEchoOffCommand = array of Arr2CGPOCPEchoOffCommand;

  CGPOEUnsupportedCommandException = class;
  Arr1CGPOEUnsupportedCommandException = array of CGPOEUnsupportedCommandException;
  Arr2CGPOEUnsupportedCommandException = array of Arr1CGPOEUnsupportedCommandException;
  Arr3CGPOEUnsupportedCommandException = array of Arr2CGPOEUnsupportedCommandException;

  CGPOCPHeadersOffCommand = class;
  Arr1CGPOCPHeadersOffCommand = array of CGPOCPHeadersOffCommand;
  Arr2CGPOCPHeadersOffCommand = array of Arr1CGPOCPHeadersOffCommand;
  Arr3CGPOCPHeadersOffCommand = array of Arr2CGPOCPHeadersOffCommand;

  CGPOCELoadCommand = class;
  Arr1CGPOCELoadCommand = array of CGPOCELoadCommand;
  Arr2CGPOCELoadCommand = array of Arr1CGPOCELoadCommand;
  Arr3CGPOCELoadCommand = array of Arr2CGPOCELoadCommand;

  CGPOCCDtcNumberCommand = class;
  Arr1CGPOCCDtcNumberCommand = array of CGPOCCDtcNumberCommand;
  Arr2CGPOCCDtcNumberCommand = array of Arr1CGPOCCDtcNumberCommand;
  Arr3CGPOCCDtcNumberCommand = array of Arr2CGPOCCDtcNumberCommand;

  CGPOCPLineFeedOffCommand = class;
  Arr1CGPOCPLineFeedOffCommand = array of CGPOCPLineFeedOffCommand;
  Arr2CGPOCPLineFeedOffCommand = array of Arr1CGPOCPLineFeedOffCommand;
  Arr3CGPOCPLineFeedOffCommand = array of Arr2CGPOCPLineFeedOffCommand;

  CGPOCCPendingTroubleCodesCommand = class;
  Arr1CGPOCCPendingTroubleCodesCommand = array of CGPOCCPendingTroubleCodesCommand;
  Arr2CGPOCCPendingTroubleCodesCommand = array of Arr1CGPOCCPendingTroubleCodesCommand;
  Arr3CGPOCCPendingTroubleCodesCommand = array of Arr2CGPOCCPendingTroubleCodesCommand;

  CGPOCPObdProtocolCommand = class;
  Arr1CGPOCPObdProtocolCommand = array of CGPOCPObdProtocolCommand;
  Arr2CGPOCPObdProtocolCommand = array of Arr1CGPOCPObdProtocolCommand;
  Arr3CGPOCPObdProtocolCommand = array of Arr2CGPOCPObdProtocolCommand;

  CGPOCPBarometricPressureCommand = class;
  Arr1CGPOCPBarometricPressureCommand = array of CGPOCPBarometricPressureCommand;
  Arr2CGPOCPBarometricPressureCommand = array of Arr1CGPOCPBarometricPressureCommand;
  Arr3CGPOCPBarometricPressureCommand = array of Arr2CGPOCPBarometricPressureCommand;

  CGPOCPCloseCommand = class;
  Arr1CGPOCPCloseCommand = array of CGPOCPCloseCommand;
  Arr2CGPOCPCloseCommand = array of Arr1CGPOCPCloseCommand;
  Arr3CGPOCPCloseCommand = array of Arr2CGPOCPCloseCommand;

  CGPOCObdMultiCommand = class;
  Arr1CGPOCObdMultiCommand = array of CGPOCObdMultiCommand;
  Arr2CGPOCObdMultiCommand = array of Arr1CGPOCObdMultiCommand;
  Arr3CGPOCObdMultiCommand = array of Arr2CGPOCObdMultiCommand;

  CGPOCEMassAirFlowCommand = class;
  Arr1CGPOCEMassAirFlowCommand = array of CGPOCEMassAirFlowCommand;
  Arr2CGPOCEMassAirFlowCommand = array of Arr1CGPOCEMassAirFlowCommand;
  Arr3CGPOCEMassAirFlowCommand = array of Arr2CGPOCEMassAirFlowCommand;

  CGPOCPersistentCommand = class;
  Arr1CGPOCPersistentCommand = array of CGPOCPersistentCommand;
  Arr2CGPOCPersistentCommand = array of Arr1CGPOCPersistentCommand;
  Arr3CGPOCPersistentCommand = array of Arr2CGPOCPersistentCommand;

  CGPOCPAvailablePidsCommand = class;
  Arr1CGPOCPAvailablePidsCommand = array of CGPOCPAvailablePidsCommand;
  Arr2CGPOCPAvailablePidsCommand = array of Arr1CGPOCPAvailablePidsCommand;
  Arr3CGPOCPAvailablePidsCommand = array of Arr2CGPOCPAvailablePidsCommand;

  CGPOCFWidebandAirFuelRatioCommand = class;
  Arr1CGPOCFWidebandAirFuelRatioCommand = array of CGPOCFWidebandAirFuelRatioCommand;
  Arr2CGPOCFWidebandAirFuelRatioCommand = array of Arr1CGPOCFWidebandAirFuelRatioCommand;
  Arr3CGPOCFWidebandAirFuelRatioCommand = array of Arr2CGPOCFWidebandAirFuelRatioCommand;

  CGPOEFuelType = class;
  Arr1CGPOEFuelType = array of CGPOEFuelType;
  Arr2CGPOEFuelType = array of Arr1CGPOEFuelType;
  Arr3CGPOEFuelType = array of Arr2CGPOEFuelType;

  CGPOCFFuelLevelCommand = class;
  Arr1CGPOCFFuelLevelCommand = array of CGPOCFFuelLevelCommand;
  Arr2CGPOCFFuelLevelCommand = array of Arr1CGPOCFFuelLevelCommand;
  Arr3CGPOCFFuelLevelCommand = array of Arr2CGPOCFFuelLevelCommand;

  CGPOCFConsumptionRateCommand = class;
  Arr1CGPOCFConsumptionRateCommand = array of CGPOCFConsumptionRateCommand;
  Arr2CGPOCFConsumptionRateCommand = array of Arr1CGPOCFConsumptionRateCommand;
  Arr3CGPOCFConsumptionRateCommand = array of Arr2CGPOCFConsumptionRateCommand;

  CGPOCPDescribeProtocolCommand = class;
  Arr1CGPOCPDescribeProtocolCommand = array of CGPOCPDescribeProtocolCommand;
  Arr2CGPOCPDescribeProtocolCommand = array of Arr1CGPOCPDescribeProtocolCommand;
  Arr3CGPOCPDescribeProtocolCommand = array of Arr2CGPOCPDescribeProtocolCommand;

  CGPOCERPMCommand = class;
  Arr1CGPOCERPMCommand = array of CGPOCERPMCommand;
  Arr2CGPOCERPMCommand = array of Arr1CGPOCERPMCommand;
  Arr3CGPOCERPMCommand = array of Arr2CGPOCERPMCommand;

  CGPOCPSelectProtocolCommand = class;
  Arr1CGPOCPSelectProtocolCommand = array of CGPOCPSelectProtocolCommand;
  Arr2CGPOCPSelectProtocolCommand = array of Arr1CGPOCPSelectProtocolCommand;
  Arr3CGPOCPSelectProtocolCommand = array of Arr2CGPOCPSelectProtocolCommand;

  CGPOCCModuleVoltageCommand = class;
  Arr1CGPOCCModuleVoltageCommand = array of CGPOCCModuleVoltageCommand;
  Arr2CGPOCCModuleVoltageCommand = array of Arr1CGPOCCModuleVoltageCommand;
  Arr3CGPOCCModuleVoltageCommand = array of Arr2CGPOCCModuleVoltageCommand;

  CGPOCCDistanceSinceCCCommand = class;
  Arr1CGPOCCDistanceSinceCCCommand = array of CGPOCCDistanceSinceCCCommand;
  Arr2CGPOCCDistanceSinceCCCommand = array of Arr1CGPOCCDistanceSinceCCCommand;
  Arr3CGPOCCDistanceSinceCCCommand = array of Arr2CGPOCCDistanceSinceCCCommand;

  CGPOCFFindFuelTypeCommand = class;
  Arr1CGPOCFFindFuelTypeCommand = array of CGPOCFFindFuelTypeCommand;
  Arr2CGPOCFFindFuelTypeCommand = array of Arr1CGPOCFFindFuelTypeCommand;
  Arr3CGPOCFFindFuelTypeCommand = array of Arr2CGPOCFFindFuelTypeCommand;

  CGPOCPFuelPressureCommand = class;
  Arr1CGPOCPFuelPressureCommand = array of CGPOCPFuelPressureCommand;
  Arr2CGPOCPFuelPressureCommand = array of Arr1CGPOCPFuelPressureCommand;
  Arr3CGPOCPFuelPressureCommand = array of Arr2CGPOCPFuelPressureCommand;

  CGPOCPTimeoutCommand = class;
  Arr1CGPOCPTimeoutCommand = array of CGPOCPTimeoutCommand;
  Arr2CGPOCPTimeoutCommand = array of Arr1CGPOCPTimeoutCommand;
  Arr3CGPOCPTimeoutCommand = array of Arr2CGPOCPTimeoutCommand;

  CGPOCPObdRawCommand = class;
  Arr1CGPOCPObdRawCommand = array of CGPOCPObdRawCommand;
  Arr2CGPOCPObdRawCommand = array of Arr1CGPOCPObdRawCommand;
  Arr3CGPOCPObdRawCommand = array of Arr2CGPOCPObdRawCommand;

  CGPOEFuelTrim = class;
  Arr1CGPOEFuelTrim = array of CGPOEFuelTrim;
  Arr2CGPOEFuelTrim = array of Arr1CGPOEFuelTrim;
  Arr3CGPOEFuelTrim = array of Arr2CGPOEFuelTrim;

  CGPOCCPermanentTroubleCodesCommand = class;
  Arr1CGPOCCPermanentTroubleCodesCommand = array of CGPOCCPermanentTroubleCodesCommand;
  Arr2CGPOCCPermanentTroubleCodesCommand = array of Arr1CGPOCCPermanentTroubleCodesCommand;
  Arr3CGPOCCPermanentTroubleCodesCommand = array of Arr2CGPOCCPermanentTroubleCodesCommand;

  CGPOCTAirIntakeTemperatureCommand = class;
  Arr1CGPOCTAirIntakeTemperatureCommand = array of CGPOCTAirIntakeTemperatureCommand;
  Arr2CGPOCTAirIntakeTemperatureCommand = array of Arr1CGPOCTAirIntakeTemperatureCommand;
  Arr3CGPOCTAirIntakeTemperatureCommand = array of Arr2CGPOCTAirIntakeTemperatureCommand;

  CGPOEBusInitException = class;
  Arr1CGPOEBusInitException = array of CGPOEBusInitException;
  Arr2CGPOEBusInitException = array of Arr1CGPOEBusInitException;
  Arr3CGPOEBusInitException = array of Arr2CGPOEBusInitException;

  CGPOENonNumericResponseException = class;
  Arr1CGPOENonNumericResponseException = array of CGPOENonNumericResponseException;
  Arr2CGPOENonNumericResponseException = array of Arr1CGPOENonNumericResponseException;
  Arr3CGPOENonNumericResponseException = array of Arr2CGPOENonNumericResponseException;

  CGPOCPAvailablePidsCommand_01_20 = class;
  Arr1CGPOCPAvailablePidsCommand_01_20 = array of CGPOCPAvailablePidsCommand_01_20;
  Arr2CGPOCPAvailablePidsCommand_01_20 = array of Arr1CGPOCPAvailablePidsCommand_01_20;
  Arr3CGPOCPAvailablePidsCommand_01_20 = array of Arr2CGPOCPAvailablePidsCommand_01_20;

  CGPOCTAmbientAirTemperatureCommand = class;
  Arr1CGPOCTAmbientAirTemperatureCommand = array of CGPOCTAmbientAirTemperatureCommand;
  Arr2CGPOCTAmbientAirTemperatureCommand = array of Arr1CGPOCTAmbientAirTemperatureCommand;
  Arr3CGPOCTAmbientAirTemperatureCommand = array of Arr2CGPOCTAmbientAirTemperatureCommand;

  CGPOCPIntakeManifoldPressureCommand = class;
  Arr1CGPOCPIntakeManifoldPressureCommand = array of CGPOCPIntakeManifoldPressureCommand;
  Arr2CGPOCPIntakeManifoldPressureCommand = array of Arr1CGPOCPIntakeManifoldPressureCommand;
  Arr3CGPOCPIntakeManifoldPressureCommand = array of Arr2CGPOCPIntakeManifoldPressureCommand;

  CGPOUCommandAvailabilityHelper = class;
  Arr1CGPOUCommandAvailabilityHelper = array of CGPOUCommandAvailabilityHelper;
  Arr2CGPOUCommandAvailabilityHelper = array of Arr1CGPOUCommandAvailabilityHelper;
  Arr3CGPOUCommandAvailabilityHelper = array of Arr2CGPOUCommandAvailabilityHelper;

  CGPOEUnableToConnectException = class;
  Arr1CGPOEUnableToConnectException = array of CGPOEUnableToConnectException;
  Arr2CGPOEUnableToConnectException = array of Arr1CGPOEUnableToConnectException;
  Arr3CGPOEUnableToConnectException = array of Arr2CGPOEUnableToConnectException;

  CGPOCPObdWarmstartCommand = class;
  Arr1CGPOCPObdWarmstartCommand = array of CGPOCPObdWarmstartCommand;
  Arr2CGPOCPObdWarmstartCommand = array of Arr1CGPOCPObdWarmstartCommand;
  Arr3CGPOCPObdWarmstartCommand = array of Arr2CGPOCPObdWarmstartCommand;

  CGPOCCIgnitionMonitorCommand = class;
  Arr1CGPOCCIgnitionMonitorCommand = array of CGPOCCIgnitionMonitorCommand;
  Arr2CGPOCCIgnitionMonitorCommand = array of Arr1CGPOCCIgnitionMonitorCommand;
  Arr3CGPOCCIgnitionMonitorCommand = array of Arr2CGPOCCIgnitionMonitorCommand;

  CGPOCPercentageObdCommand = class;
  Arr1CGPOCPercentageObdCommand = array of CGPOCPercentageObdCommand;
  Arr2CGPOCPercentageObdCommand = array of Arr1CGPOCPercentageObdCommand;
  Arr3CGPOCPercentageObdCommand = array of Arr2CGPOCPercentageObdCommand;

  CGPOCERuntimeCommand = class;
  Arr1CGPOCERuntimeCommand = array of CGPOCERuntimeCommand;
  Arr2CGPOCERuntimeCommand = array of Arr1CGPOCERuntimeCommand;
  Arr3CGPOCERuntimeCommand = array of Arr2CGPOCERuntimeCommand;

  CGPOCEAbsoluteLoadCommand = class;
  Arr1CGPOCEAbsoluteLoadCommand = array of CGPOCEAbsoluteLoadCommand;
  Arr2CGPOCEAbsoluteLoadCommand = array of Arr1CGPOCEAbsoluteLoadCommand;
  Arr3CGPOCEAbsoluteLoadCommand = array of Arr2CGPOCEAbsoluteLoadCommand;

  CGPOCEOilTempCommand = class;
  Arr1CGPOCEOilTempCommand = array of CGPOCEOilTempCommand;
  Arr2CGPOCEOilTempCommand = array of Arr1CGPOCEOilTempCommand;
  Arr3CGPOCEOilTempCommand = array of Arr2CGPOCEOilTempCommand;

  CGPOCEThrottlePositionCommand = class;
  Arr1CGPOCEThrottlePositionCommand = array of CGPOCEThrottlePositionCommand;
  Arr2CGPOCEThrottlePositionCommand = array of Arr1CGPOCEThrottlePositionCommand;
  Arr3CGPOCEThrottlePositionCommand = array of Arr2CGPOCEThrottlePositionCommand;

  CGPOEStoppedException = class;
  Arr1CGPOEStoppedException = array of CGPOEStoppedException;
  Arr2CGPOEStoppedException = array of Arr1CGPOEStoppedException;
  Arr3CGPOEStoppedException = array of Arr2CGPOEStoppedException;

  CGPOCTTemperatureCommand = class;
  Arr1CGPOCTTemperatureCommand = array of CGPOCTTemperatureCommand;
  Arr2CGPOCTTemperatureCommand = array of Arr1CGPOCTTemperatureCommand;
  Arr3CGPOCTTemperatureCommand = array of Arr2CGPOCTTemperatureCommand;

  CGPOCTEngineCoolantTemperatureCommand = class;
  Arr1CGPOCTEngineCoolantTemperatureCommand = array of CGPOCTEngineCoolantTemperatureCommand;
  Arr2CGPOCTEngineCoolantTemperatureCommand = array of Arr1CGPOCTEngineCoolantTemperatureCommand;
  Arr3CGPOCTEngineCoolantTemperatureCommand = array of Arr2CGPOCTEngineCoolantTemperatureCommand;

  CGPOCCDistanceMILOnCommand = class;
  Arr1CGPOCCDistanceMILOnCommand = array of CGPOCCDistanceMILOnCommand;
  Arr2CGPOCCDistanceMILOnCommand = array of Arr1CGPOCCDistanceMILOnCommand;
  Arr3CGPOCCDistanceMILOnCommand = array of Arr2CGPOCCDistanceMILOnCommand;

  CGPOCPObdResetCommand = class;
  Arr1CGPOCPObdResetCommand = array of CGPOCPObdResetCommand;
  Arr2CGPOCPObdResetCommand = array of Arr1CGPOCPObdResetCommand;
  Arr3CGPOCPObdResetCommand = array of Arr2CGPOCPObdResetCommand;

  CGPOEResponseException = class;
  Arr1CGPOEResponseException = array of CGPOEResponseException;
  Arr2CGPOEResponseException = array of Arr1CGPOEResponseException;
  Arr3CGPOEResponseException = array of Arr2CGPOEResponseException;

  CGPOCSpeedCommand = class;
  Arr1CGPOCSpeedCommand = array of CGPOCSpeedCommand;
  Arr2CGPOCSpeedCommand = array of Arr1CGPOCSpeedCommand;
  Arr3CGPOCSpeedCommand = array of Arr2CGPOCSpeedCommand;

  CGPOCPAvailablePidsCommand_21_40 = class;
  Arr1CGPOCPAvailablePidsCommand_21_40 = array of CGPOCPAvailablePidsCommand_21_40;
  Arr2CGPOCPAvailablePidsCommand_21_40 = array of Arr1CGPOCPAvailablePidsCommand_21_40;
  Arr3CGPOCPAvailablePidsCommand_21_40 = array of Arr2CGPOCPAvailablePidsCommand_21_40;

  CGPOCPFuelRailPressureCommand = class;
  Arr1CGPOCPFuelRailPressureCommand = array of CGPOCPFuelRailPressureCommand;
  Arr2CGPOCPFuelRailPressureCommand = array of Arr1CGPOCPFuelRailPressureCommand;
  Arr3CGPOCPFuelRailPressureCommand = array of Arr2CGPOCPFuelRailPressureCommand;

  CGPOCCTroubleCodesCommand = class;
  Arr1CGPOCCTroubleCodesCommand = array of CGPOCCTroubleCodesCommand;
  Arr2CGPOCCTroubleCodesCommand = array of Arr1CGPOCCTroubleCodesCommand;
  Arr3CGPOCCTroubleCodesCommand = array of Arr2CGPOCCTroubleCodesCommand;

  CGPOCSystemOfUnits = interface;
  Arr1CGPOCSystemOfUnits = array of CGPOCSystemOfUnits;
  Arr2CGPOCSystemOfUnits = array of Arr1CGPOCSystemOfUnits;
  Arr3CGPOCSystemOfUnits = array of Arr2CGPOCSystemOfUnits;

  CAIUPredicate = interface;
  Arr1CAIUPredicate = array of CAIUPredicate;
  Arr2CAIUPredicate = array of Arr1CAIUPredicate;
  Arr3CAIUPredicate = array of Arr2CAIUPredicate;

  JURPattern = class external 'java.util.regex' name 'Pattern';
  Arr1JURPattern = array of JURPattern;
  Arr2JURPattern = array of Arr1JURPattern;
  Arr3JURPattern = array of Arr2JURPattern;

  JLObject = class external 'java.lang' name 'Object';
  Arr1JLObject = array of JLObject;
  Arr2JLObject = array of Arr1JLObject;
  Arr3JLObject = array of Arr2JLObject;

  JLStringBuilder = class external 'java.lang' name 'StringBuilder';
  Arr1JLStringBuilder = array of JLStringBuilder;
  Arr2JLStringBuilder = array of Arr1JLStringBuilder;
  Arr3JLStringBuilder = array of Arr2JLStringBuilder;

  JIInputStream = class external 'java.io' name 'InputStream';
  Arr1JIInputStream = array of JIInputStream;
  Arr2JIInputStream = array of Arr1JIInputStream;
  Arr3JIInputStream = array of Arr2JIInputStream;

  JUArrayList = class external 'java.util' name 'ArrayList';
  Arr1JUArrayList = array of JUArrayList;
  Arr2JUArrayList = array of Arr1JUArrayList;
  Arr3JUArrayList = array of Arr2JUArrayList;

  JLClass = class external 'java.lang' name 'Class';
  Arr1JLClass = array of JLClass;
  Arr2JLClass = array of Arr1JLClass;
  Arr3JLClass = array of Arr2JLClass;

  JLString = class external 'java.lang' name 'String';
  Arr1JLString = array of JLString;
  Arr2JLString = array of Arr1JLString;
  Arr3JLString = array of Arr2JLString;

  JLLong = class external 'java.lang' name 'Long';
  Arr1JLLong = array of JLLong;
  Arr2JLLong = array of Arr1JLLong;
  Arr3JLLong = array of Arr2JLLong;

  JLEnum = class external 'java.lang' name 'Enum';
  Arr1JLEnum = array of JLEnum;
  Arr2JLEnum = array of Arr1JLEnum;
  Arr3JLEnum = array of Arr2JLEnum;

  JLRuntimeException = class external 'java.lang' name 'RuntimeException';
  Arr1JLRuntimeException = array of JLRuntimeException;
  Arr2JLRuntimeException = array of Arr1JLRuntimeException;
  Arr3JLRuntimeException = array of Arr2JLRuntimeException;

  JIOutputStream = class external 'java.io' name 'OutputStream';
  Arr1JIOutputStream = array of JIOutputStream;
  Arr2JIOutputStream = array of Arr1JIOutputStream;
  Arr3JIOutputStream = array of Arr2JIOutputStream;


{$include obd2fpc.inc}

implementation

end.
