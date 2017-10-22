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

with Incr.Lexers.Batch_Lexers.Generic_Lexers;
with Matreshka.Internals.Unicode;

package Incr.Ada_Lexers is

   subtype Token is Lexers.Batch_Lexers.Rule_Index;

   Arrow_Token             : constant Token := 1;
   Double_Dot_Token        : constant Token := 2;
   Double_Star_Token       : constant Token := 3;
   Assignment_Token        : constant Token := 4;
   Inequality_Token        : constant Token := 5;
   Greater_Or_Equal_Token  : constant Token := 6;
   Less_Or_Equal_Token     : constant Token := 7;
   Left_Label_Token        : constant Token := 8;
   Right_Label_Token       : constant Token := 9;
   Box_Token               : constant Token := 10;
   Ampersand_Token         : constant Token := 11;
   Apostrophe_Token        : constant Token := 12;
   Left_Parenthesis_Token  : constant Token := 13;
   Right_Parenthesis_Token : constant Token := 14;
   Star_Token              : constant Token := 15;
   Plus_Token              : constant Token := 16;
   Comma_Token             : constant Token := 17;
   Hyphen_Token            : constant Token := 18;
   Dot_Token               : constant Token := 19;
   Slash_Token             : constant Token := 20;
   Colon_Token             : constant Token := 21;
   Semicolon_Token         : constant Token := 22;
   Less_Token              : constant Token := 23;
   Equal_Token             : constant Token := 24;
   Greater_Token           : constant Token := 25;
   Vertical_Line_Token     : constant Token := 26;

   Identifier_Token        : constant Token := 27;
   Numeric_Literal_Token   : constant Token := 28;
   Character_Literal_Token : constant Token := 29;
   String_Literal_Token    : constant Token := 30;
   Comment_Token           : constant Token := 31;
   Space_Token             : constant Token := 32;
   New_Line_Token          : constant Token := 33;
   Error_Token             : constant Token := 34;

   Abort_Token             : constant Token := 35;
   Abs_Token               : constant Token := 36;
   Abstract_Token          : constant Token := 37;
   Accept_Token            : constant Token := 38;
   Access_Token            : constant Token := 39;
   Aliased_Token           : constant Token := 40;
   All_Token               : constant Token := 41;
   And_Token               : constant Token := 42;
   Array_Token             : constant Token := 43;
   At_Token                : constant Token := 44;
   Begin_Token             : constant Token := 45;
   Body_Token              : constant Token := 46;
   Case_Token              : constant Token := 47;
   Constant_Token          : constant Token := 48;
   Declare_Token           : constant Token := 49;
   Delay_Token             : constant Token := 50;
   Delta_Token             : constant Token := 51;
   Digits_Token            : constant Token := 52;
   Do_Token                : constant Token := 53;
   Else_Token              : constant Token := 54;
   Elsif_Token             : constant Token := 55;
   End_Token               : constant Token := 56;
   Entry_Token             : constant Token := 57;
   Exception_Token         : constant Token := 58;
   Exit_Token              : constant Token := 59;
   For_Token               : constant Token := 60;
   Function_Token          : constant Token := 61;
   Generic_Token           : constant Token := 62;
   Goto_Token              : constant Token := 63;
   If_Token                : constant Token := 64;
   In_Token                : constant Token := 65;
   Interface_Token         : constant Token := 66;
   Is_Token                : constant Token := 67;
   Limited_Token           : constant Token := 68;
   Loop_Token              : constant Token := 69;
   Mod_Token               : constant Token := 70;
   New_Token               : constant Token := 71;
   Not_Token               : constant Token := 72;
   Null_Token              : constant Token := 73;
   Of_Token                : constant Token := 74;
   Or_Token                : constant Token := 75;
   Others_Token            : constant Token := 76;
   Out_Token               : constant Token := 77;
   Overriding_Token        : constant Token := 78;
   Package_Token           : constant Token := 79;
   Pragma_Token            : constant Token := 80;
   Private_Token           : constant Token := 81;
   Procedure_Token         : constant Token := 82;
   Protected_Token         : constant Token := 83;
   Raise_Token             : constant Token := 84;
   Range_Token             : constant Token := 85;
   Record_Token            : constant Token := 86;
   Rem_Token               : constant Token := 87;
   Renames_Token           : constant Token := 88;
   Requeue_Token           : constant Token := 89;
   Return_Token            : constant Token := 90;
   Reverse_Token           : constant Token := 91;
   Select_Token            : constant Token := 92;
   Separate_Token          : constant Token := 93;
   Some_Token              : constant Token := 94;
   Subtype_Token           : constant Token := 95;
   Synchronized_Token      : constant Token := 96;
   Tagged_Token            : constant Token := 97;
   Task_Token              : constant Token := 98;
   Terminate_Token         : constant Token := 99;
   Then_Token              : constant Token := 100;
   Type_Token              : constant Token := 101;
   Until_Token             : constant Token := 102;
   Use_Token               : constant Token := 103;
   When_Token              : constant Token := 104;
   While_Token             : constant Token := 105;
   With_Token              : constant Token := 106;
   Xor_Token               : constant Token := 107;

   type Batch_Lexer is new Lexers.Batch_Lexers.Batch_Lexer with private;

   overriding procedure Get_Token
     (Self   : access Batch_Lexer;
      Result : out Lexers.Batch_Lexers.Rule_Index);

private

   package Tables is
      use Incr.Lexers.Batch_Lexers;

      function To_Class (Value : Matreshka.Internals.Unicode.Code_Point)
        return Character_Class;
      pragma Inline (To_Class);

      function Switch (S : State; Class : Character_Class) return State;
      pragma Inline (Switch);

      function Rule (S : State) return Rule_Index;
      pragma Inline (Rule);

   end Tables;

   package Base_Lexers is new Incr.Lexers.Batch_Lexers.Generic_Lexers
     (To_Class    => Tables.To_Class,
      Switch      => Tables.Switch,
      Rule        => Tables.Rule,
      Error_State => 86);

   type Batch_Lexer is new Base_Lexers.Batch_Lexer with null record;

end Incr.Ada_Lexers;
