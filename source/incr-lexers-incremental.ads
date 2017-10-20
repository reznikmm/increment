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
-- Copyright © 2015-2017, Maxim Reznik <max@gela.work>                      --
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

with League.Strings.Cursors.Characters;

with Incr.Documents;
with Incr.Lexers.Batch_Lexers;
with Incr.Nodes.Tokens;
with Incr.Version_Trees;

package Incr.Lexers.Incremental is
   --  @summary
   --  Incremental Lexer
   --
   --  @description
   --  This package provides incremental lexical analyser and related types.
   --
   --  The lexer uses three versions of the document:
   --
   --  * Reference - version when last analysis completed
   --  * Changing - current version under construction
   --  * Previous - last read-olny version of document to be analyzed
   --
   --  Workflow of this lexer includes next steps:
   --
   --  * call Prepare_Document to explicitly mark tokens to start re-lexing
   --  * find first marked token using Nested_Changes property
   --  * get new token by call First_New_Token
   --  * continue calling Next_New_Token until Is_Synchronized
   --  * now new stream of token in sync with old one at Synchronized_Token
   --  * look for next marked token from here and continue from step 3
   --

   type Incremental_Lexer is tagged limited private;
   --  Type to perform incremental lexical analysis

   type Incremental_Lexer_Access is access Incremental_Lexer;

   not overriding procedure Set_Batch_Lexer
     (Self  : in out Incremental_Lexer;
      Lexer : Batch_Lexers.Batch_Lexer_Access);
   --  Assign batch lexer to Self.

   not overriding procedure Prepare_Document
     (Self      : in out Incremental_Lexer;
      Document  : Documents.Document_Access;
      Reference : Version_Trees.Version);
   --  Start analysis by looking for tokens where re-lexing should be start.
   --  Mark them with Need_Analysis flag.

   not overriding function First_New_Token
     (Self  : in out Incremental_Lexer;
      Token : Nodes.Tokens.Token_Access)
      return Nodes.Tokens.Token_Access;
   --  Start construction of new token stream from given Token.
   --  Token should be marked as Need_Analysis flag in Prepare_Document call.
   --  Return first created token.

   not overriding function Next_New_Token
     (Self : in out Incremental_Lexer) return Nodes.Tokens.Token_Access;
--       with Pre => not Is_Synchronized (Self);
   --  Continue construction of new token stream. Return next created token.
   --  Should be called when not yet Is_Synchronized (Self);

   not overriding function Is_Synchronized
     (Self : Incremental_Lexer) return Boolean;
   --  Check if new token stream in synch with old one.

   not overriding function Synchronized_Token
     (Self : Incremental_Lexer) return Nodes.Tokens.Token_Access
       with Pre => Is_Synchronized (Self);
   --  Return first token after join new token stream with old one.
   --  Should be called just after Is_Synchronized returns True.

private

   type Incremental_Lexer is new Batch_Lexers.Abstract_Source with record
      Batch       : Batch_Lexers.Batch_Lexer_Access;
      Document    : Documents.Document_Access;
      Reference   : Version_Trees.Version;  --  Last analyzed version
      Previous    : Version_Trees.Version;  --  Version to analyze
      Token       : Nodes.Tokens.Token_Access;
      Prev_Token  : Nodes.Tokens.Token_Access;
      Last_Reused : Nodes.Tokens.Token_Access;
      Count       : Integer;  --  Number of chars piped before Token
      State       : Batch_Lexers.State;  --  Lexer State before Token
      New_State   : Batch_Lexers.State;  --  State after Xxx_New_Token
      Text        : League.Strings.Universal_String;  --  Text of Token
      Cursor      : League.Strings.Cursors.Characters.Character_Cursor;
   end record;

   overriding function Get_Next (Self : not null access Incremental_Lexer)
     return Wide_Wide_Character;

end Incr.Lexers.Incremental;
