------------------------------------------------------------------------------
--                                                                          --
--                               Gela Project                               --
--                                                                          --
--                Programming Language Construction Framework               --
--                                                                          --
--                      Incremental Analysis Component                      --
--                                                                          --
------------------------------------------------------------------------------
--                                                                          --
-- Copyright © 2017, Maxim Reznik <max@gela.work>                           --
-- All rights reserved.                                                     --
--                                                                          --
-- Redistribution and use in source and binary forms, with or without       --
-- modification, are permitted provided that the following conditions       --
-- are met:                                                                 --
--                                                                          --
--  * Redistributions of source code must retain the above copyright        --
--    notice, this list of conditions and the following disclaimer.         --
--                                                                          --
--  * Redistributions in binary form must reproduce the above copyright     --
--    notice, this list of conditions and the following disclaimer in the   --
--    documentation and/or other materials provided with the distribution.  --
--                                                                          --
--  * Neither the name of the Maxim Reznik, IE nor the names of its         --
--    contributors may be used to endorse or promote products derived from  --
--    this software without specific prior written permission.              --
--                                                                          --
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS      --
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT        --
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR    --
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT     --
-- HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,   --
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED --
-- TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR   --
-- PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF   --
-- LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING     --
-- NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS       --
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.             --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Containers.Hashed_Maps;
with League.Strings.Hash;
package body Incr.Ada_Lexers is

   package body Tables is separate;

   package Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => League.Strings.Universal_String,
      Element_Type    => Token,
      Hash            => League.Strings.Hash,
      Equivalent_Keys => League.Strings."=",
      "="             => Lexers.Batch_Lexers."=");

   Default    : constant Lexers.Batch_Lexers.State := 0;
   Apostrophe : constant Lexers.Batch_Lexers.State := 87;

   Map : Maps.Map;

   overriding procedure Get_Token
     (Self   : access Batch_Lexer;
      Result : out Lexers.Batch_Lexers.Rule_Index)
   is
      use type Lexers.Batch_Lexers.Rule_Index;
      use type Lexers.Batch_Lexers.State;
      Start : constant Lexers.Batch_Lexers.State := Self.Get_Start_Condition;
   begin
      if Start = Apostrophe then
         Self.Set_Start_Condition (Default);
      end if;

      Base_Lexers.Batch_Lexer (Self.all).Get_Token (Result);

      if Result = 34 then
         Result := Vertical_Line_Token;
      elsif Result = 35 then
         Result := Numeric_Literal_Token;
      elsif Result = 36 then
         Result := String_Literal_Token;
      elsif Result > 36 then
         Result := Error_Token;
      elsif Result = Identifier_Token then
         declare
            Text   : constant League.Strings.Universal_String :=
              Self.Get_Text.To_Casefold;
            Cursor : constant Maps.Cursor := Map.Find (Text);
         begin
            if Maps.Has_Element (Cursor) then
               Result := Maps.Element (Cursor);

               if Start = Apostrophe and Result /= Range_Token then
                  Result := Identifier_Token;
               end if;
            end if;
         end;
      end if;

      if Result = Apostrophe_Token then
         Self.Set_Start_Condition (Apostrophe);
      else
         Self.Set_Start_Condition (Default);
      end if;
   end Get_Token;

   function "+" (V : Wide_Wide_String) return League.Strings.Universal_String
     renames League.Strings.To_Universal_String;
begin
   Map.Insert (+"abort", Abort_Token);
   Map.Insert (+"abs", Abs_Token);
   Map.Insert (+"abstract", Abstract_Token);
   Map.Insert (+"accept", Accept_Token);
   Map.Insert (+"access", Access_Token);
   Map.Insert (+"aliased", Aliased_Token);
   Map.Insert (+"all", All_Token);
   Map.Insert (+"and", And_Token);
   Map.Insert (+"array", Array_Token);
   Map.Insert (+"at", At_Token);
   Map.Insert (+"begin", Begin_Token);
   Map.Insert (+"body", Body_Token);
   Map.Insert (+"case", Case_Token);
   Map.Insert (+"constant", Constant_Token);
   Map.Insert (+"declare", Declare_Token);
   Map.Insert (+"delay", Delay_Token);
   Map.Insert (+"delta", Delta_Token);
   Map.Insert (+"digits", Digits_Token);
   Map.Insert (+"do", Do_Token);
   Map.Insert (+"else", Else_Token);
   Map.Insert (+"elsif", Elsif_Token);
   Map.Insert (+"end", End_Token);
   Map.Insert (+"entry", Entry_Token);
   Map.Insert (+"exception", Exception_Token);
   Map.Insert (+"exit", Exit_Token);
   Map.Insert (+"for", For_Token);
   Map.Insert (+"function", Function_Token);
   Map.Insert (+"generic", Generic_Token);
   Map.Insert (+"goto", Goto_Token);
   Map.Insert (+"if", If_Token);
   Map.Insert (+"in", In_Token);
   Map.Insert (+"interface", Interface_Token);
   Map.Insert (+"is", Is_Token);
   Map.Insert (+"limited", Limited_Token);
   Map.Insert (+"loop", Loop_Token);
   Map.Insert (+"mod", Mod_Token);
   Map.Insert (+"new", New_Token);
   Map.Insert (+"not", Not_Token);
   Map.Insert (+"null", Null_Token);
   Map.Insert (+"of", Of_Token);
   Map.Insert (+"or", Or_Token);
   Map.Insert (+"others", Others_Token);
   Map.Insert (+"out", Out_Token);
   Map.Insert (+"overriding", Overriding_Token);
   Map.Insert (+"package", Package_Token);
   Map.Insert (+"pragma", Pragma_Token);
   Map.Insert (+"private", Private_Token);
   Map.Insert (+"procedure", Procedure_Token);
   Map.Insert (+"protected", Protected_Token);
   Map.Insert (+"raise", Raise_Token);
   Map.Insert (+"range", Range_Token);
   Map.Insert (+"record", Record_Token);
   Map.Insert (+"rem", Rem_Token);
   Map.Insert (+"renames", Renames_Token);
   Map.Insert (+"requeue", Requeue_Token);
   Map.Insert (+"return", Return_Token);
   Map.Insert (+"reverse", Reverse_Token);
   Map.Insert (+"select", Select_Token);
   Map.Insert (+"separate", Separate_Token);
   Map.Insert (+"some", Some_Token);
   Map.Insert (+"subtype", Subtype_Token);
   Map.Insert (+"synchronized", Synchronized_Token);
   Map.Insert (+"tagged", Tagged_Token);
   Map.Insert (+"task", Task_Token);
   Map.Insert (+"terminate", Terminate_Token);
   Map.Insert (+"then", Then_Token);
   Map.Insert (+"type", Type_Token);
   Map.Insert (+"until", Until_Token);
   Map.Insert (+"use", Use_Token);
   Map.Insert (+"when", When_Token);
   Map.Insert (+"while", While_Token);
   Map.Insert (+"with", With_Token);
   Map.Insert (+"xor", Xor_Token);
end Incr.Ada_Lexers;
