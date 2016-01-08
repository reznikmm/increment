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
         Parent  : Node_Access);
      --  Initialize Self as token existent since Prehistoric time with given
      --  Parent.
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

   overriding function Span
     (Self : aliased in out Token;
      Kind : Span_Kinds;
      Time : Version_Trees.Version) return Natural;

end Incr.Nodes.Tokens;
