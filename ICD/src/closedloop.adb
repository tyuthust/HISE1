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
   
   CurrentTime : TickCount := 0;  -- current time as measured in ticks
  
   -- The array records the previous 7 heart rate histories 
   ICDHistory : ICD.HeartRateHistory;
   
   -- The counter records how many times a impulse generator should impulse the heart
   Impulsecounter: Integer range 0..10:=0;
   
   -- Treatment active flag
   TreatmentActiveFlag: Boolean:=False;
  
   -- Records how many ticks happen in one minute
   TickTimesInOneMinute: constant Integer:=600;
   
   -- It records whether an impulse tick procedure is done, if it has done, set as true, otherwise as false
   ImpulseTickFlag: Boolean:=False;
   
   -- It records whether an impulse needs to do, if it needs to do, set as true, otherwise as false
   Ventricle_fibrillationActiveFlag: Boolean:=False;
   
   -- The float records the progress of a beats, when this value is greater than 1, 
   -- address one impulse tick procedure and reset this variable as the mod result
   ImpulseTickFloat: Float:=0.0;
   
   -- the fixed joulse deliver to heart when tachycardia
   TachyJoules: constant Measures.Joules:=2;
   
   -- check whether the candidate Principal is a known one
   function CheckIsKnownPrincipal(
                                  CandidatePrincipalPtr : in Principal.PrincipalPtr;
                                  KnownPrincipalArray : in Network.PrincipalArray) 
                                  return Boolean is
      IsKnown : Boolean := False;
   begin
      for i in Integer range KnownPrincipalArray'Range loop
         if (Principal.PrincipalPtrToString(CandidatePrincipalPtr) 
             = Principal.PrincipalPtrToString(KnownPrincipalArray(i))) then
            IsKnown := True;
         end if;
      end loop;
--      if (not IsKnown) then
--         Put_Line("This is NOT Known Pricipal");
--      end if;
      return IsKnown;
   end CheckIsKnownPrincipal;
   
   -- check whether the candidate has the right to act
   function CheckIsAuthorisedRoles(
                                   CandidatePrincipal : 
                                   in Principal.Principal;
                                   AuthorisedRole: in Principal.Role) 
                                  return Boolean is
      IsAuthorised : Boolean := False;
   begin
      IsAuthorised := Principal.HasRole(
                                        P => CandidatePrincipal,
                                        R => AuthorisedRole);
--      if (not IsAuthorised) then
--         Put_Line("This is NOT an Authorised Role");
--      end if;
         return IsAuthorised;
   end CheckIsAuthorisedRoles;
  
   -- Call this function to init all the elements in the system
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
  
  -- Call the function to step the system.
   procedure Tick is

      
      -- Ready for response towards the request if applicable
      MsgJoules : Measures.Joules;
      MsgBPM : Measures.BPM;
      sendMSg : Network.NetworkMessage;
      MsgRateHistory:Network.RateHistory;
      
   begin
      Put_Line("**************TICK_START************** ");
      -- read messages from the network and act based on
      -- Message type
      Network.GetNewMessage(Net,MsgAvailable,Msg);
      if (MsgAvailable) then
         -- feature based on the MSG
         case Msg.MessageType is
            when Network.ModeOn =>
               -- Principal should be known
               -- only Card and Clin can access
               Put("Try ModeOn");New_Line;
               if(
                  CheckIsKnownPrincipal(
                                        CandidatePrincipalPtr
                                        =>  Msg.MOnSource,
                                        KnownPrincipalArray 
                                        => KnownPrincipals.all)
                  and (
                     CheckIsAuthorisedRoles(
                                            CandidatePrincipal => 
                                              Msg.MOnSource.all,
                                            AuthorisedRole =>
                                              Principal.Cardiologist
                                           )
                       or
                     CheckIsAuthorisedRoles(
                                            CandidatePrincipal => 
                                              Msg.MOnSource.all,
                                            AuthorisedRole =>
                                              Principal.ClinicalAssistant
                                           )
                    )
                 ) then
                  -- Turn On the ICD
                  Put("Set Mode On");New_Line;
                  ICD.On(Icd => IcdUnit);
                  if(ICD.IsOn(IcdUnit)) then
                     Put_Line("Mode Is On");
                  else
                     Put_Line("Mode Is Off");
                  end if;
                  
               end if;
               
            when Network.ModeOff =>
               -- Principal should be known
               -- only Card and Clin can access
               Put("Try ModeOff");New_Line;
               if(
                  CheckIsKnownPrincipal(
                                        CandidatePrincipalPtr=>
                                          Msg.MOffSource,
                                        KnownPrincipalArray => 
                                          KnownPrincipals.all)
                  and (
                       CheckIsAuthorisedRoles(
                                              CandidatePrincipal => 
                                                Msg.MOffSource.all,
                                              AuthorisedRole =>
                                                Principal.Cardiologist
                                           )
                       or
                         CheckIsAuthorisedRoles(
                                                CandidatePrincipal => 
                                                  Msg.MOffSource.all,
                                                AuthorisedRole =>
                                                  Principal.ClinicalAssistant
                                               )
                    )
                 ) then
                  -- turn off the ICD
                  Put("Set Mode Off");New_Line;
                  ICD.Off(Icd => IcdUnit);
                  if(ICD.IsOn(IcdUnit)) then
                     Put_Line("Mode Is On");
                  else
                     Put_Line("Mode Is Off");
                  end if;
                  
               end if;

            when Network.ReadRateHistoryRequest =>
               -- Principal should be known
               -- all roles can read
               Put("Try Read Rate History");New_Line;
               if(
                  CheckIsKnownPrincipal(
                                        CandidatePrincipalPtr=>
                                          Msg.HSource,
                                        KnownPrincipalArray => 
                                          KnownPrincipals.all)
                 ) then
                  -- read History from ICD history and save to the Msg history
                  -- one by one
                  Put("Read Rate History");New_Line;
                  for i in MsgRateHistory'Range loop
                     MsgRateHistory(i) := ICDHistory(i);
                  end loop;
                  sendMSg  :=(MessageType=>Network.ReadRateHistoryResponse,
                                 HDestination=>Msg.HSource,
                              History=>MsgRateHistory);
                  
                  Network.SendMessage(Net => Net,Message => sendMSg);
                  
               end if;
               
               
               
            when Network.ReadSettingsRequest =>
               -- Principal should be known
               -- only Card and Clin can access
               Put("Try Read Settings");New_Line;
               if(
                  CheckIsKnownPrincipal(
                                        CandidatePrincipalPtr=>Msg.RSource,
                                        KnownPrincipalArray => KnownPrincipals.all)
                  and (
                       CheckIsAuthorisedRoles(
                                              CandidatePrincipal => 
                                                Msg.RSource.all,
                                              AuthorisedRole =>
                                                Principal.Cardiologist
                                           )
                       or
                         CheckIsAuthorisedRoles(
                                                CandidatePrincipal => 
                                                  Msg.RSource.all,
                                                AuthorisedRole =>
                                                  Principal.ClinicalAssistant
                                               )
                    )
                 ) then
                  -- readSetting should only success if ICD is Off
                  -- the judgement is implemented in the ICD
                  Put("Read Settings Success if Off");New_Line;
                  if(ICD.readSet(TachyB => MsgBPM,
                              JoulesToD => MsgJoules,
                                 Icd => IcdUnit)) then
                     Put_Line("Icd is Off, Read Successfully");
                     sendMSg  :=(MessageType=>Network.ReadSettingsResponse,
                                 RDestination=>Msg.RSource,
                                 RTachyBound=>MsgBPM,
                                 RJoulesToDeliver=>MsgJoules);
                     Put_Line("Icd is On, Read failed return MIN Value to Network");
                     Network.SendMessage(Net => Net,Message => sendMSg);
                  else
                     sendMSg :=(MessageType=>Network.ReadSettingsResponse,
                                RDestination=>Msg.RSource,
                                RTachyBound=>Measures.MIN_BPM,
                                RJoulesToDeliver=>Measures.MIN_JOULES);
                      Network.SendMessage(Net => Net,Message => sendMSg);
                  end if;
               end if;               
            when Network.ChangeSettingsRequest =>
               -- Principal should be known
               -- only Card can access
               Put("Try ChangeSettings First Check Role");New_Line;
               if(
                  (CheckIsKnownPrincipal(
                                        CandidatePrincipalPtr=>Msg.CSource,
                                        KnownPrincipalArray => KnownPrincipals.all)
                  and
                    CheckIsAuthorisedRoles(
                                           CandidatePrincipal => 
                                             Msg.CSource.all,
                                           AuthorisedRole =>
                                             Principal.Cardiologist
                                          ))
                 ) then
                  Put("Try ChangeSettings Second Check OFF");New_Line;

                  --apply change to ICD
                  --should only success if ICD is Off
                  if(
                     (
                      ICD.changeTachycardiaUpperBoundSet
                        (
                         Icd => IcdUnit,
                         TachyB => 
                           Measures.LimitBPM(Input => Msg.CTachyBound))
                      and
                        ICD.changeJoulesDeliverNumForVentricle_fibrillation
                          (
                           Icd => IcdUnit,
                           JoulesToD => 
                             Measures.LimitJoules(Msg.CJoulesToDeliver))
                     )

                    ) then
                     Put("ChangeSettingsRequest | New TachyBound = ");
                     Put(Msg.CTachyBound);
                     Put("| New Joules = ");
                     Put(Msg.CJoulesToDeliver);
                     Put(") ");New_Line;
                     sendMSg  :=(MessageType=>Network.ChangeSettingsResponse,
                                 CDestination=>Msg.CSource);
                     Network.SendMessage(Net => Net,Message => sendMSg);
                  else
                     Put("Mode is off cannot change");
                     if(ICD.IsOn(Icd => IcdUnit))then
                        Put("|On");New_Line;
                     else
                        Put("|Off");New_Line;
                     end if;
                     
                  end if;
               end if;
               when others =>
               -- other info types is invalid
               -- do nothing
               null;
         end case;
      end if;
      -- Set On/Off
      if(ICD.IsOn(IcdUnit)) then
         if(not HRM.IsOn(Hrm => Monitor)) then
            HRM.On(Hrt => Hrt,Hrm => Monitor);
         end if;
         if(not ImpulseGenerator.IsOn(Gen => Generator)) then
            ImpulseGenerator.On(Gen => Generator);
         end if;
      else
         if(HRM.IsOn(Hrm => Monitor)) then
            HRM.Off(Monitor);
         end if;
         if(ImpulseGenerator.IsOn(Gen => Generator)) then
            ImpulseGenerator.Off(Gen => Generator);
         end if;
         
      end if;
      
      -- Read and print the current measured heart rate
      HRM.GetRate(Monitor, HeartRate);
      Put("Measured heart rate  = ");
      Put(Item => HeartRate);
      New_Line;
      

      
      -- Tick all components to simulate the passage of one decisecond
      ICD.Tick(Icd => IcdUnit,
               Hrh=>ICDHistory,
               HeartRate=>HeartRate,
               CurrentTime=>CurrentTime,
               Generator => Generator,
               TickTimesInOneMinute =>TickTimesInOneMinute,
               ImpulseTickFloat =>ImpulseTickFloat ,                               
               ImpulseGeneratorcounter =>Impulsecounter,
               ActiveFlag=>TreatmentActiveFlag,
               ImpulseTickFlag =>ImpulseTickFlag,
               Ventricle_fibrillationActiveFlag =>
                 Ventricle_fibrillationActiveFlag);
      
      -- Impulse tick will call if 
      -- 1 VF is happening
      -- or
      -- 2 Tachycardia is reach the action bound
      if Ventricle_fibrillationActiveFlag then
         ImpulseGenerator.Tick(Generator, Hrt);
         Ventricle_fibrillationActiveFlag:=False;
         PUT("Done the Vemtricle Fibrillation treatment");
         New_Line;
      elsif ImpulseTickFloat>1.0 then
         -- Set the Joules of the impulse deliver to haert when Tachycardia
         ImpulseGenerator.SetImpulse(Generator,TachyJoules);
         PUT("Signals Joules: ");
         PUT(TachyJoules);
         New_Line;
         ImpulseGenerator.Tick(Generator, Hrt);
         PUT(Hrt.Impulse'Image);
         ImpulseTickFloat:=ImpulseTickFloat-1.0;          
         ImpulseTickFlag:=True;
         New_Line;
      end if;

      -- other Tick will still act every Closedloop tick
      HRM.Tick(Monitor, Hrt);
      Heart.Tick(Hrt);
      Network.Tick(Net);
      
      -- Reset the Joules of the impulse deliver to heart as 0 
      -- when once Tachycardia treatment is done 
      -- or ventricle fibrallation is done
      Hrt.Impulse:=0;
      
      PUT("Health condition: ");
      if(ICD.IsOn(IcdUnit)) then
         Put_Line(IcdUnit.HealthType'Image);
      else
         Put_Line("MODE OFF");
      end if;
      Put_Line("***************TICK_FIN*************** ");

      
      CurrentTime := CurrentTime + 1;
      delay 0.1;
   end Tick;
  
end ClosedLoop;
