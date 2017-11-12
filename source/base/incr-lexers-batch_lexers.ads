--  Copyright (c) 2015-2017 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with League.Strings;

package Incr.Lexers.Batch_Lexers is
   --  @summary
   --  Batch Lexer
   --
   --  @description
   --  This package provides batch lexical analyser and related types.

   type Batch_Lexer is abstract tagged limited private;
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

   not overriding procedure Get_Token
     (Self   : access Batch_Lexer;
      Result : out Rule_Index) is abstract;
   --  Recognize next token in the stream and return its rule number.

   function Get_Text
     (Self : Batch_Lexer'Class) return League.Strings.Universal_String;
   --  Return text of last recognized token.

   function Get_Token_Length (Self : Batch_Lexer'Class) return Positive;
   --  Return length of text of last recognized token.

   function Get_Token_Lookahead (Self : Batch_Lexer'Class) return Positive;
   --  Return number of character seen by lexer after end of the last
   --  recognized token.

   type Character_Class is new Natural;

private

   Buffer_Size : constant := 1024;
   --  Max length of any token

   subtype Buffer_Index is Positive range 1 .. Buffer_Size;

   type Character_Class_Array is array (Buffer_Index) of Character_Class;

   Error_Character : constant Character_Class := 0;

   type Batch_Lexer is abstract tagged limited record
      Source  : Source_Access;
      Start   : State := INITIAL;
      Next    : Buffer_Index := 1;  --  First empty position in Buffer
      To      : Natural := 0;       --  Last char of the last read token
      Rule    : Rule_Index;         --  Rule      of the last read token
      Buffer  : Wide_Wide_String (Buffer_Index);
      Classes : Character_Class_Array;
   end record;

end Incr.Lexers.Batch_Lexers;
