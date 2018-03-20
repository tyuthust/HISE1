with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Measures;
with Heart; use Heart;
with Network;
with ImpulseGenerator;

package body ICD is
   -- The initial upper bound for tachycardia when the system starts
   TachyBound: Measures.BPM:=100;

   -- The initial number of joules to deliver in the case of a ventricle fibrillation
   JoulesToDeliver: Measures.Joules:=30;

   -- The constant of how much the given impulse rate should be greater than the current heart rate
   AboveHeartRate: Constant Measures.BPM:=15;

   -- The init counter records how many ticks after the icd has been init
   InitCounter: Integer:=-1;

   -- The variable records the current heart rate when tachycardia is detected
   CurrentHeartRate:Measures.BPM:=0;

   function isTachycardia(HeartRate: in BPM) return Boolean is
      Output: Boolean:=False;
   begin
      if(HeartRate>TachyBound) then
         Output:= True;
      else
         Output:= False;
      end if;
      return Output;
   end isTachycardia;

   function isVentricleFibrillation(Hrh: in HeartRateHistory) return Boolean is
      Sum: Integer := 0;
      Temp_Minus: Integer;
      Output: Boolean:=False;
   begin
      for I in Integer range 1..Hrh'Last loop
         Temp_Minus:= Hrh(I).Rate-Hrh(I-1).Rate;
         if Temp_Minus>=0 then
            Sum:=Sum+Temp_Minus;
         else
            Sum:=Sum-Temp_Minus;
         end if;
      end loop;
      -- When the tick is less than 6 after then icd is inited, the data is too little to consider whether there is ventricle fibrillation
      if Sum/6>=10 and InitCounter>6 then
--        if Sum/6>=10 then
         Output:=True;
      end if;
      return Output;
   end isVentricleFibrillation;


   function changeTachycardiaUpperBoundSet(TachyB: in Measures.BPM; Icd: in ICDType) return Boolean is
   begin
      if Icd.IsOn then
         return False;
      else
         TachyBound:=TachyB;
         return True;
      end if;
   end changeTachycardiaUpperBoundSet;

   function changeJoulesDeliverNumForVentricle_fibrillation(JoulesToD: in Measures.Joules;Icd: in ICDType) return Boolean is
   begin
      if Icd.IsOn then
         return False;
      else
         JoulesToDeliver:=JoulesToD;
         return True;
      end if;
   end changeJoulesDeliverNumForVentricle_fibrillation;

   function readSet(TachyB: out Measures.BPM;JoulesToD: out Measures.Joules;Icd: in ICDType) return Boolean is
   begin
      if Icd.IsOn then
         return False;
      else
         TachyB:=TachyBound;
         JoulesToD:=JoulesToDeliver;
         return True;
      end if;
   end readSet;


   procedure updateHeartRateHistory(Hrh: in out HeartRateHistory; HeartRate: in BPM; CurrentTime: in TickCount) is
      HistoryPro: Integer:= Hrh'Last;
   begin
      while HistoryPro>Hrh'First loop
         Hrh(HistoryPro):=(Rate => Hrh(HistoryPro-1).Rate,
                           Time => Hrh(HistoryPro-1).Time);
         HistoryPro:=HistoryPro-1;
      end loop;
      Hrh(Hrh'First):=(Rate => HeartRate, Time => CurrentTime);
   end updateHeartRateHistory;

   procedure Init(Hrh: in out HeartRateHistory;Icd: out ICDType) is
      DefaultHR:BPM:=-1;
   begin
      for I in Integer range 0..Hrh'Last loop
         Hrh(I).Time:=TickCount(TickCount'First);
         Hrh(I).Rate:=DefaultHR;
      end loop;
      Icd.HealthType:=Healthy;
      Icd.IsOn:=False;
      InitCounter:=0;
   end Init;

   procedure On(Icd: out ICDType) is
   begin
      Icd.IsOn:=True;
   end On;

   procedure Off(Icd: out ICDType) is
   begin
      Icd.IsOn:=False;
   end Off;

   function IsOn(Icd : in ICDType) return Boolean is
   begin
      return Icd.IsOn;
   end IsOn;




   procedure activeWhenVentricle_fibrillation(Generator:in out ImpulseGenerator.GeneratorType;
                                              Ventricle_fibrillationActiveFlag: in out Boolean) is
   begin
      ImpulseGenerator.SetImpulse(Generator,JoulesToDeliver);
      PUT("Signals Joules: ");
      PUT(JoulesToDeliver);
      New_Line;
      Ventricle_fibrillationActiveFlag:=True;
   end activeWhenVentricle_fibrillation;

   procedure activeWhenTachycardia(ImpulseGeneratorcounter: in out Integer;
                                   Generator:in out ImpulseGenerator.GeneratorType;
                                   HeartRate : in Measures.BPM;
                                   ActiveFlag: in out Boolean;
                                   TickTimesInOneMinute: in Integer;
                                   ImpulseTickFloat:out Float;
                                   ImpulseTickFlag: in out Boolean) is

      TempHeartRate: Measures.BPM;
      -- This number is the reciprocal value of the impulse interval but more precise
      ReciprocalTickFloat: Float range 0.0..1.0 :=0.0;
   begin

      TempHeartRate:=HeartRate+AboveHeartRate;
      ReciprocalTickFloat:=Float(TempHeartRate)/Float(TickTimesInOneMinute);
      -- Add the temp reciprocalTick in the Impulse tick progress
      ImpulseTickFloat:=ReciprocalTickFloat;

      PUT("Counter");
      PUT(ImpulseGeneratorcounter);
      New_Line;


      if ImpulseGeneratorcounter>0  then
         if ImpulseTickFlag then
            ImpulseGeneratorcounter:=ImpulseGeneratorcounter-1;
            ImpulseTickFlag:=False;
            ActiveFlag:=True;
         end if;
      else
         -- The treatment stops
         ActiveFlag:=False;
         -- Set the Joules of the impulse deliver to haert as 0 when Tachycardia treatment is done
--           ImpulseGenerator.SetImpulse(Generator,J => 0);
--           PUT("Reset the impulse: ");
--           PUT(Generator.Impulse);
--           New_Line;
      end if;
   end activeWhenTachycardia;

   procedure Tick(Icd: in out ICDType;
                  Hrh: in out HeartRateHistory;
                  HeartRate: in BPM;
                  CurrentTime:in TickCount;
                  Generator:in out ImpulseGenerator.GeneratorType;
                  TickTimesInOneMinute: in Integer;
                  ImpulseTickFloat: in out Float;
                  ImpulseGeneratorcounter: in out Integer;
                  ActiveFlag: in out Boolean;
                  ImpulseTickFlag: in out Boolean;
                  Ventricle_fibrillationActiveFlag: in out Boolean) is
   Temp_ImpulseTickFloat:Float:=0.0;
   begin
      if Icd.IsOn then
         if InitCounter>=0 then
            InitCounter:=InitCounter+1;
         end if;

         -- update the current time and heart rate in the heart rate history array(hrh).
         updateHeartRateHistory(Hrh         => Hrh,
                             HeartRate   => HeartRate,
                             CurrentTime => CurrentTime);
         if isTachycardia(HeartRate => HeartRate) or ImpulseGeneratorcounter>0 then
            -- When detected the tachycardia and it is not under a treatment, set the 10 signals and start the treatment
            if ActiveFlag=False then
               ActiveFlag:=True;
               CurrentHeartRate:=HeartRate;
               ImpulseGeneratorcounter:=10;
            end if;
            -- Set the health type as tachycardia
            Icd.HealthType:=Tachycardia;

            activeWhenTachycardia(ImpulseGeneratorcounter => ImpulseGeneratorcounter,
                                  Generator               => Generator ,
                                  TickTimesInOneMinute =>TickTimesInOneMinute,
                                  ImpulseTickFloat =>Temp_ImpulseTickFloat,
                                  ActiveFlag              => ActiveFlag,
                                  ImpulseTickFlag =>ImpulseTickFlag,
                                  HeartRate   => CurrentHeartRate);
            PUT("INIT: ");
            PUT(ImpulseTickFloat'Image);
            New_Line;
            ImpulseTickFloat:=ImpulseTickFloat+Temp_ImpulseTickFloat;
            PUT("TEMP: ");
            PUT(Temp_ImpulseTickFloat'Image);
            New_Line;
            PUT("Impulse: ");
            PUT(ImpulseTickFloat'Image);
            New_Line;
         elsif isVentricleFibrillation(Hrh => Hrh) then
            -- Set the health type as ventricle fibrillation
            Icd.HealthType:=Ventricle_fibrillation;
            -- When detected the tachycardia and it is not under a treatment, start the treatment
            activeWhenVentricle_fibrillation(Generator               => Generator,
                                             Ventricle_fibrillationActiveFlag => Ventricle_fibrillationActiveFlag);
         else
            -- Set the health type as healthy
            Icd.HealthType:=Healthy;
         end if;
      end if;
   end Tick;
end ICD;
