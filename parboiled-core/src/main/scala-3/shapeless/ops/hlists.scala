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

package shapeless
package ops

object hlist {

  /** Type class supporting prepending to this `HList`.
    *
    * @author Miles Sabin
    */
  trait Prepend[P <: HList, S <: HList] extends Serializable {
    type Out <: HList
    def inverse(p: Out): (P, S)
  }

  trait LowestPriorityPrepend {
    type Aux[P <: HList, S <: HList, Out0 <: HList] = Prepend[P, S] { type Out = Out0 }
  }

  trait LowPriorityPrepend extends LowestPriorityPrepend {

    implicit def hnilPrepend0[P <: HList, S >: HNil.type <: HNil]: Aux[P, S, P] =
      new Prepend[P, S] {
        type Out = P
        def apply(prefix: P, suffix: S): P = prefix

        def inverse(p: P): (P, S) = (p, HNil)
      }
  }

  object Prepend extends LowPriorityPrepend {
    def apply[P <: HList, S <: HList](implicit prepend: Prepend[P, S]): Aux[P, S, prepend.Out] = prepend

    implicit def hnilPrepend1[P >: HNil.type <: HNil, S <: HList]: Aux[P, S, S] =
      new Prepend[P, S] {
        type Out = S
        def apply(prefix: P, suffix: S): S = suffix

        def inverse(p: S): (P, S) = (HNil, p)
      }
  }

  /** Type class supporting reverse prepending to this `HList`.
    *
    * @author Miles Sabin
    */
  trait ReversePrepend[P <: HList, S <: HList] extends Serializable { type Out <: HList }

  trait LowPriorityReversePrepend {
    type Aux[P <: HList, S <: HList, Out0 <: HList] = ReversePrepend[P, S] { type Out = Out0 }
  }

  object ReversePrepend extends LowPriorityReversePrepend {
    def apply[P <: HList, S <: HList](implicit prepend: ReversePrepend[P, S]): Aux[P, S, prepend.Out] = prepend

    implicit def hnilReversePrepend1[P <: HNil, S <: HList]: Aux[P, S, S] =
      new ReversePrepend[P, S] {
        type Out = S
        def apply(prefix: P, suffix: S) = suffix
      }
  }
}
