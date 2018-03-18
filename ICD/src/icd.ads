with Network;
with Heart; use Heart;
with Measures; use Measures;

-- This package provides some basic functinality to provide 
-- the necessary calculations of the impulse to be delivered based on the measured heart rate
package ICD is 
 -- The record type for a heart rate monitor

   
-- Various patient heart's health type
type PatientHeartHealthType is (Healthy, Tachycardia,Ventricle_fibrillation);

HISTORY_LENGTH: constant Integer := 6;
type HeartRateHistory is array (Integer range 0..HISTORY_LENGTH) of Network.RateRecord;

type ICDType is
    record
-- The measure heart rate for the heart
       HealthType : PatientHeartHealthType;
       IsOn : Boolean;  -- Indicates whether the ICD is on.
    end record;   

-- Determine whether the current heart rate belongs to Tachycardia
   function isTachycardia(HeartRate: in BPM)
                          return boolean;

-- Determine whether the past period of heart rate history belongs to Ventricle fibrillation
   function isVentricleFibrillation(Hrh: in HeartRateHistory)
                                    return boolean;
   
   -- Set the TachyBound field
   function changeTachycardiaUpperBoundSet(TachyB: in Integer;Icd: out ICDType) return Boolean;
   
   -- Set the number of joules to deliver filed
   function changeJoulesDeliverNumForVentricle_fibrillation(JoulesToD: in Measures.Joules;Icd: out ICDType) return Boolean;
   
   -- Read the TachyBound field and joules to deliver filed
   function readSet(TachyB: out Integer;JoulesToD: out Measures.Joules;Icd: out ICDType) return Boolean;
   
   -- Update the heart rate history
   procedure updateHeartRateHistory(Hrh: in out HeartRateHistory;HeartRate: in BPM;CurrentTime:in TickCount);
   
   -- Init the Heart Rate History Array
   procedure Init(Hrh: in out HeartRateHistory;Icd: out ICDType);
   
   -- Turn on the ICD
   procedure On(Icd: out ICDType);
      
   -- Turn off the ICD.
   procedure Off(Icd : out ICDType);
   
   -- Get the status of the HRM (on/off)
   function IsOn(Icd : in ICDType) return Boolean;
   --# return B.IsOn;
   
   -- Tick the clock and Check the health type of the patient, call by closedLoop
   procedure Tick(Icd: in out ICDType;Hrh: in out HeartRateHistory;HeartRate: in BPM;CurrentTime:in TickCount);
end ICD;
