--  Copyright (c) 2015-2017 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with League.Strings;

with Incr.Lexers.Batch_Lexers;

package Incr.Nodes.Tokens is
   --  @summary
   --  Token nodes of parse tree
   --
   --  @description
   --  This package provides Token type.
   --  Tokens have no children, but usually contain some text of the document.
   --  Tokens provide support for incremental lexer by keeping related data
   --  such as
   --  * state - scanner's state at the end of the token
   --  * lookahead - number of characters after were seen by scanner after it
   --  * lookback - number of preceding tokens with lookahead over this one

   type Token is new Node with private;
   --  Token nodesof parse tree

   type Token_Access is access all Token'Class;

   overriding function Kind (Self : Token) return Node_Kind;
   --  Return type of the token. Kind is not expected to change

   not overriding function Text
     (Self : Token;
      Time : Version_Trees.Version) return League.Strings.Universal_String;
   --  Return text of the token

   not overriding procedure Set_Text
     (Self  : in out Token;
      Value : League.Strings.Universal_String);
   --  Assign text to the token

   not overriding function Next_Token
     (Self : aliased Token;
      Time : Version_Trees.Version) return Token_Access;
   --  Find next token in the parse tree

   not overriding function Previous_Token
     (Self : aliased Token;
      Time : Version_Trees.Version) return Token_Access;
   --  Find previous token in the parse tree

   not overriding function Lookback
     (Self : Token;
      Time : Version_Trees.Version) return Natural;
   --  Get number of preceding tokens with lookahead extented over this one

   subtype Scanner_State is Lexers.Batch_Lexers.State;

   not overriding function State
     (Self  : access Token;
      Time  : Version_Trees.Version) return Scanner_State;

   package Constructors is
      procedure Initialize
        (Self      : out Token'Class;
         Kind      : Node_Kind;
         Value     : League.Strings.Universal_String;
         State     : Scanner_State;
         Lookahead : Natural);

      procedure Initialize_Ancient
        (Self    : aliased in out Token'Class;
         Parent  : Node_Access;
         Back    : Natural);
      --  Initialize Self as token existent in initial version of the document.
   end Constructors;

private

   package Versioned_Strings is
     new Version_Trees.Versioned_Values (League.Strings.Universal_String);

   package Versioned_Naturals is new Version_Trees.Versioned_Values (Natural);

   type Token is new Node_With_Parent with record
      Kind   : Node_Kind;
      Text   : Versioned_Strings.Container;
      Back   : Versioned_Naturals.Container;
      Ahead  : Versioned_Naturals.Container;
      States : Versioned_Naturals.Container;
   end record;

   overriding function Is_Token (Self : Token) return Boolean;

   overriding function Arity (Self : Token) return Natural;

   overriding function Child
     (Self  : Token;
      Index : Positive;
      Time  : Version_Trees.Version) return Node_Access;

   overriding procedure Set_Child
     (Self  : aliased in out Token;
      Index : Positive;
      Value : Node_Access) is null;

   overriding function Nested_Changes
     (Self : Token;
      From : Version_Trees.Version;
      To   : Version_Trees.Version) return Boolean;

   overriding function Nested_Errors
     (Self : Token;
      Unused : Version_Trees.Version) return Boolean is (False);

   overriding function Span
     (Self : aliased in out Token;
      Kind : Span_Kinds;
      Time : Version_Trees.Version) return Natural;

   overriding procedure Discard (Self  : in out Token);

end Incr.Nodes.Tokens;
