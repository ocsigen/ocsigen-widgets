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
  include List

  let rec find_remove f = function
    | [] -> raise Not_found
    | a::l when f a -> a, l
    | a::l -> let b, ll = find_remove f l in b, a::ll

  let rec assoc_remove x = function
    | [] -> raise Not_found
    | (k, v)::l when x = k -> v, l
    | a::l -> let b, ll = assoc_remove x l in b, a::ll

  let uniq =
    let rec aux last = function
      | [] -> []
      | a::l when a = last -> aux a l
      | a::l -> a::(aux a l)
    in
    function
      | [] -> []
      | a::l -> a::(aux a l)
}}
