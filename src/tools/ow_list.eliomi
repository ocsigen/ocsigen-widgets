(* Ocsigen-widgets
 * http://www.ocsigen.org/ocsigen-widgets
 *
 * Copyright (C) 2014 Université Paris Diderot
 *      Charly Chevalier
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

{shared{
  include module type of List

  (** [find_remove f l] takes the first value from the list returning true, and
        returns it (as the 'a). May raise Not_found *)
  val find_remove : ('a -> bool) -> 'a list -> 'a * 'a list

  (** [find_remove k l] takes the first value from the list equal to k, and
        returns it (as the 'a). May raise Not_found *)
  val assoc_remove : 'a -> ('a * 'b) list -> 'b * ('a * 'b) list

  (** Remove duplicates in a sorted list *)
  val uniq : 'a list -> 'a list
}}
