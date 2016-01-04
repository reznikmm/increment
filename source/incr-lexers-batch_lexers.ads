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
-- Copyright © 2015, Maxim Reznik <max@gela.work>                           --
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
--  $Revision$ $Date$
------------------------------------------------------------------------------

with League.Strings;

package Incr.Lexers.Batch_Lexers is
   --  @summary
   --  Batch Lexer
   --
   --  @description
   --  This package provides batch lexical analyser and related types.

   type Batch_Lexer is tagged limited private;
   --  Type to perform lexical analysis
   type Batch_Lexer_Access is access all Batch_Lexer'Class;

   type Abstract_Source is abstract tagged limited null record;
   --  Abstract source of text for lexer
   type Source_Access is access all Abstract_Source'Class;

   not overriding function Get_Next
     (Self : not null access Abstract_Source)
      return Wide_Wide_Character is abstract;
   --  Retrive next character of text. Return End_Of_Input when no more data
   --  available.

   End_Of_Input : constant Wide_Wide_Character := Wide_Wide_Character'Val (4);
   --  Special character to represend end of input stream

   procedure Set_Source
     (Self   : in out Batch_Lexer'Class;
      Source : not null Source_Access);
   --  Configure new text source before perform a lexical analysis.

   type State is new Natural;
   --  State of lexer automaton

   INITIAL : constant State := 0;
   --  Initial state of lexer, valid to start analysis from very begining

   procedure Set_Start_Condition
    (Self : in out Batch_Lexer'Class; Condition : State);
   --  Assign new state.

   function Get_Start_Condition
     (Self : Batch_Lexer'Class) return State;
   --  Return current lexer state.

   type Rule_Index is new Natural;
   --  Recognized token rule, 0 for end of stream or error.

   procedure Get_Token
     (Self   : access Batch_Lexer'Class;
      Result : out Rule_Index);
   --  Recognize next token in the stream and return its rule number.

   function Get_Text
     (Self : Batch_Lexer'Class) return League.Strings.Universal_String;
   --  Return text of last recognized token.

   function Get_Token_Length (Self : Batch_Lexer'Class) return Positive;
   --  Return length of text of last recognized token.

   function Get_Token_Lookahead (Self : Batch_Lexer'Class) return Positive;
   --  Return number of character seen by lexer after end of the last
   --  recognized token.

private

   Buffer_Size : constant := 1024;
   --  Max length of any token

   subtype Buffer_Index is Positive range 1 .. Buffer_Size;

   type Character_Class is new Natural;

   type Character_Class_Array is array (Buffer_Index) of Character_Class;

   Error_Character : constant Character_Class := 0;

   type Batch_Lexer is tagged limited record
      Source  : Source_Access;
      Start   : State := INITIAL;
      Next    : Buffer_Index := 1;  --  First empty position in Buffer
      To      : Natural := 0;       --  Last char of the last read token
      Rule    : Rule_Index;         --  Rule      of the last read token
      Buffer  : Wide_Wide_String (Buffer_Index);
      Classes : Character_Class_Array;
   end record;

end Incr.Lexers.Batch_Lexers;
