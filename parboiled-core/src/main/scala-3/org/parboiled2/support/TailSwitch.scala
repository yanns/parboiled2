/*
 * Copyright 2009-2019 Mathias Doenitz
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.parboiled2.support

import hlist._

import scala.annotation.implicitNotFound

/**
  * type-level implementation of this logic:
  *   Out =
  *     R                      if T has a tail of type L
  *     (L dropRight T) ::: R  if L has a tail of type T
  */
@implicitNotFound("Illegal rule composition")
sealed trait TailSwitch[L <: HList, T <: HList, R <: HList] {
  type Out <: HList
}
object TailSwitch {
  type Reverse0[Acc <: HList, L <: HList] <: HList = L match {
    case HNil     => Acc
    case ::[h, t] => Reverse0[h :: Acc, t]
  }

  type Reverse1[L <: HList] <: HList = L match {
    case HNil     => HNil
    case ::[h, t] => Reverse0[h :: HNil, t]
  }
  /*trait Reverse[L <: HList] extends DepFn1[L] { type Out <: HList }
  object Reverse {
    implicit def reverse[L <: HList]: Reverse[L] {type Out = Reverse1[L]} = ???
  }*/

  type Prepend0[A <: HList, B <: HList] <: HList = A match {
    case HNil     => B
    case ::[h, t] => ::[h, Prepend0[t, B]]
  }
  /*trait Prepend[P <: HList, S <: HList] extends DepFn2[P, S] { type Out <: HList }
  object Prepend {
    implicit def prepend[P <: HList, S <: HList]: Prepend[P, S] {type Out = Prepend0[P, S]} = ???
  }*/

  type TailSwitch0[L <: HList, LI <: HList, T <: HList, TI <: HList, R <: HList, RI <: HList] <: HList = TI match {
    case L => R
    case _ =>
      LI match {
        case T => Prepend0[Reverse1[RI], R]
        case HNil =>
          TI match {
            case ::[_, t] => TailSwitch0[L, HNil, T, t, R, RI]
          }
        case ::[h, t] =>
          TI match {
            case HNil      => TailSwitch0[L, t, T, HNil, R, h :: RI]
            case ::[_, tt] => TailSwitch0[L, t, T, tt, R, h :: RI]
          }
      }
  }

  type Aux[L <: HList, LI <: HList, T <: HList, TI <: HList, R <: HList, RI <: HList, Out <: HList] =
    TailSwitch[L, T, R] { type Out = TailSwitch0[L, L, T, T, R, HNil] }

  implicit def tailSwitch[L <: HList, T <: HList, R <: HList]
      : TailSwitch[L, T, R] { type Out = TailSwitch0[L, L, T, T, R, HNil] } = `n/a`
}
