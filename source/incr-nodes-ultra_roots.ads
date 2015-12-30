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

with Incr.Nodes.Tokens;

package Incr.Nodes.Ultra_Roots is
   --  @summary
   --  Ultra_Root nodes of parse trees
   --
   --  @description
   --  This package provides implementation of ultra_root nodes

   type Ultra_Root is new Node with private;
   type Ultra_Root_Access is access all Ultra_Root'Class;

   package Constructors is
      procedure Initialize (Self  : out Ultra_Root'Class);
   end Constructors;

private

   type Versioned_Node_Array is array (Positive range <>) of
     Versioned_Nodes.Container;

   type Ultra_Root is new Node with record
      BOS    : Nodes.Tokens.Token_Access;
      Root   : Versioned_Nodes.Container;
      EOS    : Nodes.Tokens.Token_Access;
   end record;

   overriding function Is_Token (Self : Ultra_Root) return Boolean;

   overriding function Arity (Self : Ultra_Root) return Natural;

   overriding function Child
     (Self  : Ultra_Root;
      Index : Positive;
      Time  : Version_Trees.Version) return Node_Access;

   overriding function Nested_Changes
     (Self : Ultra_Root;
      From : Version_Trees.Version;
      To   : Version_Trees.Version) return Boolean;

   overriding function Parent
     (Self  : Ultra_Root;
      Time  : Version_Trees.Version) return Node_Access;

   overriding procedure Set_Parent
     (Self  : in out Ultra_Root;
      Value : Node_Access) is null;

   overriding function Exists
     (Self  : Ultra_Root;
      Time  : Version_Trees.Version) return Boolean;

   overriding function Get_Flag
     (Self  : Ultra_Root;
      Flag  : Transient_Flags) return Boolean;

   overriding procedure Set_Flag
     (Self  : in out Ultra_Root;
      Flag  : Transient_Flags;
      Value : Boolean := True) is null;

   overriding procedure On_Commit (Self : in out Ultra_Root) is null;

   overriding procedure On_Nested_Changes
     (Self : in out Ultra_Root;
      Diff : Integer) is null;

end Incr.Nodes.Ultra_Roots;
