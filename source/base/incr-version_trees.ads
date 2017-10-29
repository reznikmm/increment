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
--  @summary
--  Version Trees
--
--  @description
--  The package provides Version and Version_Tree types.
--  Nested generic package Versioned_Values provides Container type.
--
package Incr.Version_Trees is

   type Version is private;
   --  Version identificator
   type Version_Tree is tagged limited private;
   --  Version_Tree keeps history of a document as sequence (actually tree) of
   --  Versions as they are created. One version (changing) is different, this
   --  is a version where current chages are performed.
   --  Only read-write version of the document is its changing version.

   type Version_Tree_Access is access all Version_Tree'Class;

   not overriding function Changing (Self : Version_Tree) return Version;
   --  Version where current chages are performed

   not overriding function Is_Changing
     (Self : Version_Tree; Value : Version) return Boolean;
   --  Check if given Value is changing version of a document.
   --  @param Value version under test

   not overriding function Parent
     (Self : Version_Tree; Value : Version) return Version;
   --  Provide origin of given Version.
   --  @param Value version under query

   not overriding procedure Start_Change
     (Self     : in out Version_Tree;
      Parent   : Version;
      Changing : out Version);
   --  Create new changing version by branching it from given Parent version.
   --  @param Parent version to branch new one from
   --  @param Changing return new version. It becames changing version of
   --         a document

   generic
      type Element is private;
      --  @private Disable indexing this type in gnatdoc
   package Versioned_Values is
      --  @summary
      --  Versioned Values
      --
      --  @description
      --  The package provides Container to keep history of value changes
      --  over the time.

      type Container is private;
      --  Container to store history of value changes.

      procedure Initialize
        (Self          : in out Container;
         Initial_Value : Element);
      --  Initialize container and place Initial_Value as current.
      --  @param Initial_Value value at the initial version of a document

      function Get
        (Self : Container;
         Time : Version) return Element;
      --  Retrieve a value from container corresponding to given version.
      --  @param Time provides requested version
      --  @return Value at given time/version

      procedure Set
        (Self    : in out Container;
         Value   : Element;
         Time    : Version;
         Changes : in out Integer);
      --  Update container by given value. Version should be Is_Changing in
      --  the corresponding Version_Tree. The call returns Changes counter:
      --  * as +1 if Value becomes new value of the property
      --  * as -1 if Value is revereted to old value of the property
      --  * and 0 if Value has been changed already or match old value

      procedure Discard
        (Self    : in out Container;
         Time    : Version;
         Changes : out Integer);
      --  Update container by reverting its value. Version should be
      --  Is_Changing as in Set. See Set for description of Changes.

   private
      type Circle_Index is mod 8;
      type Element_Array is array (Circle_Index) of Element;
      type Version_Array is array (Circle_Index) of Version;

      type Container is record
         Elements : Element_Array;
         Versions : Version_Array;
         Index    : Circle_Index := 0;
      end record;

   end Versioned_Values;

private

   --  This is prototype implementation. It doesn't support branching
   --  and keeps history as linear sequence of versions. Only a few versions
   --  are kept and any older versions are dropped.

   type Version is mod 256;
   function "<" (Left, Right : Version) return Boolean;
   function ">=" (Left, Right : Version) return Boolean is
     (not (Left < Right));
   function ">" (Left, Right : Version) return Boolean is (Right < Left);

   type Version_Tree is tagged limited record
      Changing : Version := 1;
   end record;

end Incr.Version_Trees;
