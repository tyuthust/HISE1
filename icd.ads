with Network;
with Heart; use Heart;
with Measures; use Measures;

-- This package provides some basic functinality to provide 
-- the necessary calculations of the impulse to be delivered based on the measured heart rate
package ICD is 

-- Various patient heart's health type
type PatientHeartHealthType is (Healthy, Tachycardia,Ventricle_fibrillation);

HISTORY_LENGTH: constant Integer := 6;
type HeartRateHistory is array (Integer range 0..HISTORY_LENGTH) of Network.RateRecord;


-- Determine whether the current heart rate belongs to Tachycardia
function isTachycardia(HeartRate: in BPM) 
    return boolean;

-- Determine whether the past period of heart rate history belongs to Ventricle fibrillation
function isVentricleFibrillation(Hrh: in HeartRateHistory)
    return boolean;

-- Update the heart rate history
procedure updateHeartRateHistory(Hrh: in out HeartRateHistory;HeartRate: in BPM;CurrentTime:in TickCount);

-- Init the Heart Rate History Array
procedure initHRH(Hrh: in out HeartRateHistory); 
end ICD;
