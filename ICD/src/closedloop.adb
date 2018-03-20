with Measures; use Measures;
with Heart;
with HRM;
with ImpulseGenerator;
with Network;
with Principal;
with ICD;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
package body ClosedLoop is
   
   Hrt : Heart.HeartType;                -- The simulated heart
   Monitor : HRM.HRMType;                -- The simulated heart rate monitor
   Generator : ImpulseGenerator.GeneratorType; -- The simulated generator
   HeartRate : BPM;
   IcdUnit : ICD.ICDType;
   Net : Network.Network;                -- The simulated network
   Card : Principal.PrincipalPtr := new Principal.Principal;  -- A cardiologist
   Clin : Principal.PrincipalPtr := new Principal.Principal;  -- A clinical assistant
   Patient : Principal.PrincipalPtr := new Principal.Principal; -- A patient
   
   -- an array of known principals to use to initialise the network
   -- but note that the network can generate messages from other, unknown,
   -- principals too
   KnownPrincipals : access Network.PrincipalArray := new Network.PrincipalArray(0..2); 
   
   -- stores whether there was a message available on the network
   MsgAvailable : Boolean := False;
   
   -- stores the current message read from the network (if one was available)
   Msg : Network.NetworkMessage;
   
   -- stores some history information on measured heart rate
   History : Network.RateHistory;
   HistoryPos : Integer := History'First;
   CurrentTime : TickCount := 0;  -- current time as measured in ticks
   
   --Para using by ICD
   ICDHistory : ICD.HeartRateHistory;
   Impulsecounter: Integer range 0..10:=0;
   -- Treatment active flag
   TreatmentActiveFlag: Boolean:=False;
   
   -- check whether the candidate Principal is a known one
   function CheckIsKnownPrincipal(
                                  CandidatePrincipalPtr : in Principal.PrincipalPtr;
                                  KnownPrincipalArray : in Network.PrincipalArray) 
                                  return Boolean is
      IsKnown : Boolean := False;
   begin
      for i in Integer range KnownPrincipalArray'Range loop
         if (Principal.PrincipalPtrToString(CandidatePrincipalPtr) = Principal.PrincipalPtrToString(KnownPrincipalArray(i))) then
            IsKnown := True;
         end if;
      end loop;
      if (IsKnown) then
         Put_Line("This is Known Pricipal");
      else
         Put_Line("This is NOT Known Pricipal");
         end if;
         return IsKnown;
   end CheckIsKnownPrincipal;
  
  -- This procedure should create three Principals: one Patient, one 
  -- Cardiologist and one Clinical Assistant. These are the authorised 
  -- principals for the device, i.e. the Patient is one who has the
  -- device implanted in them, the Cardiologist is the patient's assigned
  -- cardiologist and the Clinical Assistant is their assigned
  -- clinical assistant.
  -- It should then create and initialise each of the components of the
  -- closed loop system, with the Network initialised so that the three
  -- principals mentioned above are the "known" principals (see network.ads)
   procedure Init is
      
   begin
      -- set up the principals with the correct roles
      Principal.InitPrincipalForRole(Card.all,Principal.Cardiologist);
      Principal.InitPrincipalForRole(Clin.all,Principal.ClinicalAssistant);
      Principal.InitPrincipalForRole(Patient.all,Principal.Patient);
      KnownPrincipals(0) := Card;
      KnownPrincipals(1) := Clin;
      KnownPrincipals(2) := Patient;
      
      Put("Known Principals: "); New_Line;
      Principal.DebugPrintPrincipalPtr(Card); New_Line;
      Principal.DebugPrintPrincipalPtr(Clin); New_Line;
      Principal.DebugPrintPrincipalPtr(Patient); New_Line;
      
      -- Initialise the components and turn the machines on
      Heart.Init(Hrt);
      HRM.Init(Monitor);
      ImpulseGenerator.Init(Generator);
      Network.Init(Net,KnownPrincipals);

      ICD.Init(Hrh => ICDHistory,Icd => IcdUnit);
      ICD.On(Icd => IcdUnit);
      
      
      HRM.On(Monitor, Hrt);
      ImpulseGenerator.On(Generator);
      
      -- Set the new impulse to 0
      ImpulseGenerator.SetImpulse(Generator, 0); 
   end Init;
  
  -- This procesure simulates one clock tick (decisecond)
  -- Besides calling the Tick procedures of each of the closed-loop
  -- components, it also needs to do things like handling network messages
  -- from authorised principals (e.g. by calling procedures of the ICD 
  -- package that you will write)
   procedure Tick is
      -- Ready for response towards the request if applicable
      MsgJoules : Measures.Joules;
      MsgBPM : Measures.BPM;
      sendMSg : Network.NetworkMessage;
   begin
      -- read messages from the network but don't act on them here,
      -- just print them out
      Network.GetNewMessage(Net,MsgAvailable,Msg);
      if (MsgAvailable) then

         
         
         -- feature based on the MSG
         case Msg.MessageType is
            when Network.ModeOn =>
               -- check if principal is known
               if(
                  CheckIsKnownPrincipal(
                                        CandidatePrincipalPtr=>Msg.MOnSource,
                                        KnownPrincipalArray => KnownPrincipals.all)
                 ) then
              
                  Put("ModeOn (MOnSource: ");
                  Principal.DebugPrintPrincipalPtr(Msg.MOnSource);
                  Put(")"); New_Line;
                  ICD.On(Icd => IcdUnit);
               end if;
               
            when Network.ModeOff =>
               if(
                  CheckIsKnownPrincipal(
                                        CandidatePrincipalPtr=>Msg.MOffSource,
                                        KnownPrincipalArray => KnownPrincipals.all)
                 ) then
                  Put("ModeOff (MOffSource: ");
                  Principal.DebugPrintPrincipalPtr(Msg.MOffSource);
                  Put(")"); New_Line;
                  ICD.Off(Icd => IcdUnit);
                  
               end if;

            when Network.ReadRateHistoryRequest =>
               if(
                  CheckIsKnownPrincipal(
                                        CandidatePrincipalPtr=>Msg.HSource,
                                        KnownPrincipalArray => KnownPrincipals.all)
                 ) then
                  Put("ReadRateHistoryRequest (HSource: ");
                  Principal.DebugPrintPrincipalPtr(Msg.HSource);
                  Put(")"); New_Line;
                  
               end if;
            when Network.ReadSettingsRequest =>
               if(
                  CheckIsKnownPrincipal(
                                        CandidatePrincipalPtr=>Msg.RSource,
                                        KnownPrincipalArray => KnownPrincipals.all)
                 ) then
                  Put("ReadSettingsRequest (RSource: ");
                  Principal.DebugPrintPrincipalPtr(Msg.RSource);
                  Put(")"); New_Line;
                  if(ICD.readSet(TachyB => MsgBPM,
                              JoulesToD => MsgJoules,
                                 Icd => IcdUnit)) then
                     sendMSg  :=(MessageType=>Network.ReadSettingsResponse,
                                 RDestination=>Msg.RSource,
                                 RTachyBound=>MsgBPM,
                                 RJoulesToDeliver=>MsgJoules);
                     Network.SendMessage(Net => Net,Message => sendMSg);
                  else
                     sendMSg :=(MessageType=>Network.ReadSettingsResponse,
                                RDestination=>Msg.RSource,
                                RTachyBound=>Measures.MIN_BPM,
                                RJoulesToDeliver=>Measures.MIN_JOULES);
                      Network.SendMessage(Net => Net,Message => sendMSg);
                  end if;
               end if;
            -- will not enter here   
            when Network.ReadRateHistoryResponse =>
               if(
                  CheckIsKnownPrincipal(
                                        CandidatePrincipalPtr=>Msg.HDestination,
                                        KnownPrincipalArray => KnownPrincipals.all)
                 ) then
                  Put("ReadRateHistoryRequest (HDestination: ");
                  Principal.DebugPrintPrincipalPtr(Msg.HDestination);
                  Put("; History: "); 
                  for Index in Msg.History'Range loop
                     Ada.Integer_Text_IO.Put(Integer(Msg.History(Index).Rate));
                     Put(" @ "); Ada.Integer_Text_IO.Put(Integer(Msg.History(Index).Time));
                     Put(", ");
                  end loop;
                  Put(")"); New_Line;
               end if;
               
            when Network.ChangeSettingsRequest =>
               if(
                  CheckIsKnownPrincipal(
                                        CandidatePrincipalPtr=>Msg.CSource,
                                        KnownPrincipalArray => KnownPrincipals.all)
                 ) then
                  Put("ChangeSettingsRequest (CSource: ");
                  Principal.DebugPrintPrincipalPtr(Msg.CSource);
                  Put("| New TachyBound = ");
                  Put(Msg.CTachyBound);
                  Put("| New Joules = ");
                  Put(Msg.CJoulesToDeliver);
                  Put(") ");New_Line;
                  
               end if;
               
               when others =>
                  -- you should implement these for your own debugging if you wish
               null;
         end case;
      end if;
      
      -- Read and print the current measured heart rate
      HRM.GetRate(Monitor, HeartRate);
      --Put("Measured heart rate  = ");
      --Put(Item => HeartRate);
      --New_Line;
      
      -- record the initial history only
      if HistoryPos <= History'Last then
         History(HistoryPos) := (Rate => HeartRate, Time => CurrentTime);
         HistoryPos := HistoryPos + 1;
      end if;
      
      -- Tick all components to simulate the passage of one decisecond
      ImpulseGenerator.Tick(Generator, Hrt);
      ICD.Tick(Icd => IcdUnit,
               Hrh=>ICDHistory,
               HeartRate=>HeartRate,
               CurrentTime=>CurrentTime,
               Generator => Generator,
               Hrt=>Hrt,
               ImpulseGeneratorcounter =>Impulsecounter,
               ActiveFlag=>TreatmentActiveFlag);
      HRM.Tick(Monitor, Hrt);
      Heart.Tick(Hrt);
      Network.Tick(Net);
      
--      PUT("Health condition: ");
--      if(ICD.IsOn(IcdUnit)) then
--         Put_Line(IcdUnit.HealthType'Image);
--      else
--         Put_Line("MODE OFF");
--      end if;

      
      CurrentTime := CurrentTime + 1;
      delay 0.1;
   end Tick;
  
end ClosedLoop;
